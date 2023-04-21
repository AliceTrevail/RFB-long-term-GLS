### RFB non-breeding
### Q: do they have an extended pelagic period?
### Q: is there an annual pattern in wet / dry?

#-----------------------------#
## Load required libraries ####
#-----------------------------#

library(suncalc)
library(sf)
library(data.table)
library(here) #for reproducible filepaths
library(scales)
library(tidyverse) #installed using install.packages("tidyverse")
library(lubridate)
library(lme4)
library(MuMIn)
library(caret)
library(ggeffects)
library(flextable)

#-----------------------------#
## Load data ####
#-----------------------------#

df_GLSimmersion <- read_csv(here("Data", "WorkingDataFrames", "RFB_GLSimmersion_clean.csv"))



#-----------------------------#
## Local times & sunrise/sunset ####
#-----------------------------#

# add local time zone for meaningful day/night behaviour
df_GLSimmersion_localtime <- df_GLSimmersion %>% 
  mutate(DateTime_GMT = as_datetime(DateTime, tz = "GMT"),
         DateTime_local = with_tz(DateTime_GMT, tzone="Indian/Chagos"),
         Date_local = lubridate::as_date(DateTime_local),
         Time_local = hms::as_hms(DateTime_local),
         year = year(DateTime_local))

# add sunrise/sunset times
# based on deployment location because of error around GLS locations
# create dataframe in format for suncalc
df_suntimes <- df_GLSimmersion_localtime %>%
  dplyr::select(Date_local) %>%
  rename(date = Date_local) %>%
  mutate(lat = -7.235893,
         lon = 72.439628) 

df_sundata <- getSunlightTimes(data = df_suntimes, 
                               keep = c("nauticalDawn", "nauticalDusk"), tz = "Indian/Chagos")

df_GLSimmersion_suntimes <- df_GLSimmersion_localtime %>%
  mutate(nauticalDawn = df_sundata$nauticalDawn,
         nauticalDusk = df_sundata$nauticalDusk,
         dorn = case_when(DateTime_local >= nauticalDawn & DateTime_local < nauticalDusk ~ "day",
                          .default="night"))


# label each night as sunset on date0 -> sunrise on date1
# code based on central place trips definition from ExMove toolkit
df_GLSimmersion_nightslabelled <- df_GLSimmersion_suntimes %>%
  group_by(ID) %>%
  mutate(same_lag = ifelse(dorn == lag(dorn), "TRUE", "FALSE"), # is the point still day or night?
         same_lead = ifelse(dorn == lead(dorn), "TRUE", "FALSE"), # is the next point transition between day/night?
         label = case_when(dorn == "night" & same_lag == "FALSE" ~ "first", # label first point at night
                           dorn == "night" & same_lead == "FALSE" ~ "last")) %>% # label last point at night
  filter(dorn == "night") %>% # will add day points back in later
  mutate(night_row = case_when(label == "first" ~ cur_group_rows())) %>% # assign group number based on row number for first points of trip only
  fill(night_row) %>% # fill NAs with group number
  group_by(night_row) %>%
  ungroup() %>%
  group_split(ID) %>%
  purrr::map_df(~.x %>% group_by(night_row) %>% 
                  mutate(night_num = cur_group_id())) %>% # assign sequential trip number
  ungroup() %>% 
  mutate(NightID = paste0(ID, "_", night_num)) %>% # assign unique trip ID based on ID and trip number
  select(-c(same_lag, same_lead, label, night_row, night_num)) %>% # remove intermediate columns
  group_by(NightID) %>%
  mutate(NightDate = first(Date_local)) %>%
  ungroup()%>%
  st_drop_geometry() # remove the geometry column

df_GLSimmersion_nightID <- df_GLSimmersion_suntimes %>%
   left_join(., df_GLSimmersion_nightslabelled)

# remove intermediate files
rm(list=ls()[!ls() %in% c("df_GLSimmersion","df_GLSimmersion_nightID")]) #could just specify objects to keep? (no errors)


#-----------------------------#
## Add known breeding stage for month ####
#-----------------------------#

df_GLSimmersion_knownbr <- df_GLSimmersion_nightID %>%
  mutate(known_br_stage = case_when(month(DateTime_local) == month(Deploydatetime) & year(DateTime_local) == year(Deploydatetime)~ Deploy_BrStage,
                                    month(DateTime_local) == month(Retrievedatetime) & year(DateTime_local) == year(Retrievedatetime) ~ Retrieve_BrStage,
                                    .default = "unknown")) %>%
  mutate(known_br_stage = factor(known_br_stage, levels = c("S0", "S1", "S2", "NB", "unknown"))) %>%
  mutate(month = month(DateTime_local))
unique(df_GLSimmersion_knownbr$known_br_stage)

#-----------------------------#
## Calculate proportions dry/wet ####
#-----------------------------#

# re-order IDs for plotting, based on deployment date
# sort by earliest tag date
# Group the data by ID and find the minimum date for each ID
id_dates <- df_GLSimmersion_knownbr %>% group_by(ID) %>% summarize(min_date = min(Date_local), max_date = max(Date_local))
# Sort the IDs based on their earliest date
sorted_ids <- id_dates %>% arrange(min_date) %>% pull(ID)
# Use the sorted IDs to reorder the factor levels of the ID variable in the data
df_GLSimmersion_knownbr$ID <- factor(df_GLSimmersion_knownbr$ID, levels = sorted_ids)

# summarise by time period, calculate proportions dry, and identify dry periods

df_GLSimmersion_daily_24hr <- df_GLSimmersion_knownbr %>% 
  group_by(ID, Sex, known_br_stage, Date_local, year, month) %>%
  summarise(daily.imm.fixes = length(Immersion),
            prop.dry = sum(Immersion == 0)/daily.imm.fixes,
            prop.wet = sum(Immersion == 20)/daily.imm.fixes,
            prop.int = sum(Immersion %in% c(1:19))/daily.imm.fixes,
            sum.props = sum(prop.dry, prop.wet, prop.int)) %>%
  ungroup() %>%
  mutate(dry24hr = case_when(prop.dry >= 0.95 ~ 1, .default = 0)) 


df_GLSimmersion_daily_day <- df_GLSimmersion_knownbr %>% 
  filter(dorn == "day") %>%
  group_by(ID, Sex, known_br_stage, Date_local, year, month) %>%
  summarise(daily.imm.fixes = length(Immersion),
            prop.dry = sum(Immersion == 0)/daily.imm.fixes,
            prop.wet = sum(Immersion == 20)/daily.imm.fixes,
            prop.int = sum(Immersion %in% c(1:19))/daily.imm.fixes,
            sum.props = sum(prop.dry, prop.wet, prop.int)) %>%
  ungroup() %>%
  mutate(dryday = case_when(prop.dry >= 0.95 ~ 1, .default = 0)) 


df_GLSimmersion_daily_night <- df_GLSimmersion_knownbr %>% 
  filter(dorn == "night") %>%
  group_by(ID, Sex, known_br_stage, NightID, NightDate, year, month) %>%
  summarise(daily.imm.fixes = length(Immersion),
            prop.dry = sum(Immersion == 0)/daily.imm.fixes,
            prop.wet = sum(Immersion == 20)/daily.imm.fixes,
            prop.int = sum(Immersion %in% c(1:19))/daily.imm.fixes,
            sum.props = sum(prop.dry, prop.wet, prop.int))%>%
  ungroup() %>%
  mutate(drynight = case_when(prop.dry >= 0.95 ~ 1, .default = 0)) 




#-----------------------------#
## Plot occurrence of dry periods ####
#-----------------------------#
id_dates <- df_GLSimmersion_knownbr %>% group_by(ID) %>% summarize(min_date = min(Date_local), max_date = max(Date_local))

# refine plots by adding tagging date
# Create the rectangle annotations for each ID
rects_deploy <- id_dates %>% 
  mutate(ymin = as.numeric(rev(ID)) - 0.5, 
         ymax = as.numeric(rev(ID)) + 0.5, 
         xmin = as_date("2018-01-01"), 
         xmax = min_date,
         year = year(min_date))

rects_retrieve <- id_dates %>% 
  mutate(ymin = as.numeric(rev(ID)) - 0.5, 
         ymax = as.numeric(rev(ID)) + 0.5, 
         xmin = max_date, 
         xmax = as_date("2019-12-31"),
         year = year(max_date))

rects_2020 <- id_dates %>% 
  mutate(ymin = as.numeric(rev(ID)) - 0.5, 
         ymax = as.numeric(rev(ID)) + 0.5, 
         xmin = case_when(year(max_date) == 2020 ~ max_date, .default = as_date("2020-01-01")), 
         xmax = as_date("2020-12-31"),
         year = 2020)

# plot defaults:
br_stage_cols <- c("#a6dba0", "#5aae61", "#1b7837", "#9970ab", "#000000")

plot_base <- list(
  facet_wrap(~year, ncol = 1, scales = "free_x"),
  geom_rect(data = subset(rects_deploy, year == 2018), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey80", alpha = 0.5, inherit.aes = FALSE),
  geom_rect(data = subset(rects_retrieve, year == 2019), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey80", alpha = 0.5, inherit.aes = FALSE),
  scale_y_discrete(limits = rev(sorted_ids)),
  scale_x_date(expand = c(0,0), date_breaks = "1 month", date_labels = "%b-%d", name = "Date (local time; GMT+6)"),
  scale_color_manual(name = "Breeding\nstage", values = br_stage_cols,
                     labels = c("Pre-breeding", "Incubation", "Chick rearing", "Non-breeding", "Unkown")),
  guides(colour = guide_legend(override.aes = list(alpha = 1))),
  theme_light()
  )

rect_2020 <- list(
  geom_rect(data = subset(rects_2020, year == 2020), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey80", alpha = 0.5, inherit.aes = FALSE)
)

p.prop.dry24hr <- ggplot(subset(df_GLSimmersion_daily_24hr, dry24hr == 1), aes(x = Date_local, y = ID, col = known_br_stage))+
  plot_base+ rect_2020+
  geom_point(alpha = 0.7)+
  labs(title = "Temporal distribution of dry dates")
p.prop.dry24hr


p.prop.dryday <- ggplot(subset(df_GLSimmersion_daily_day, dryday == 1), aes(x = Date_local, y = ID, col = known_br_stage))+
  plot_base+ rect_2020+
  geom_point(alpha = 0.6)+
  labs(title = "Temporal distribution of dry days")
p.prop.dryday


p.prop.drynight <- ggplot(subset(df_GLSimmersion_daily_night, drynight == 1 & !year == "2020"), aes(x = NightDate, y = ID, col = known_br_stage))+
  plot_base+
  geom_point(alpha = 0.6)+
  labs(title = "Temporal distribution of dry nights")
p.prop.drynight

p.prop.drynight_2020 <- ggplot(subset(df_GLSimmersion_daily_night, drynight == 1), aes(x = NightDate, y = ID, col = known_br_stage))+
  plot_base+ rect_2020+
  geom_point(alpha = 0.6)+
  labs(title = "Temporal distribution of dry nights")
p.prop.drynight_2020

ggsave(plot = p.prop.drynight, filename = here("Figures", "Dry_night_2018_2019.png"),
       width = 24, height = 22, units = "cm")

ggsave(plot = p.prop.dryday, filename = here("Figures", "Supplementary", "Dry_day.png"),
       width = 24, height = 26, units = "cm")

ggsave(plot = p.prop.dry24hr, filename = here("Figures", "Supplementary", "Dry_24hr.png"),
       width = 24, height = 26, units = "cm")

ggsave(plot = p.prop.drynight_2020, filename = here("Figures", "Supplementary", "Dry_night_allyears.png"),
       width = 24, height = 26, units = "cm")


#-----------------------------#
## Model dry periods ####
#-----------------------------#
# set up table for model validation metrics

diagnostics <- c("Model", "AUC", "Threshold", "accuracy", "sensitivity", "specificity", 
                 "Pos.PP", "Neg.PP", "Precision", "Recall")
mod.valid <- as.data.frame(matrix(ncol = 10, nrow = 2))
colnames(mod.valid) <- diagnostics
mod.valid$Model <- c("night", "day")

### dry nights ####

options(na.action = na.fail)

df_GLSimmersion_daily_night <- df_GLSimmersion_daily_night %>%
  mutate(month_f = as.factor(month),
         year_f = as.factor(year))

names(df_GLSimmersion_daily_night)

mod.night <- glmer(drynight ~ month_f + Sex + known_br_stage + (1|ID), 
                   data = df_GLSimmersion_daily_night,
                   family = "binomial")
summary(mod.night)

dredge.night <- dredge(mod.night) 

dredge.night.out <- dredge.night  %>%
  tibble() %>%
  select(-c(`(Intercept)`, weight)) %>%
  flextable() %>%
  colformat_double(j = c(5,6,7), digits = 2) %>% # round numbers of specific columns to 2 decimal places
  add_header_row(values = as_paragraph(as_chunk(c("Explanatory variables", "Model selection metrics"))), colwidths = c(3,4), top = TRUE) %>% # add header row
  set_header_labels(known_br_stage = 'Known breeding stage',
                    month_f = 'Month') %>%
  fontsize(size = 11, part = 'all') %>% # set font size for the table
  autofit()
  
dredge.night.out
save_as_docx(dredge.night.out, path = here("Tables", "Supplementary", "Mod_dredge_night.docx"), align = 'center')


mod.night.upd <- glmer(drynight ~ month_f + known_br_stage + (1|ID), 
                   data = df_GLSimmersion_daily_night,
                   family = "binomial")

summary(mod.night.upd)
  
modpred.night <- as.numeric(predict(mod.night.upd, type="response"))
roccurve.night <- pROC::roc(df_GLSimmersion_daily_night$drynight, modpred.night)
  
t.night <- pROC::coords(roccurve.night, "best", ret = "threshold", transpose = F)[1,]
mod.valid$Threshold[mod.valid$Model == "night"] <- t.night

auc.night <- pROC::auc(roccurve.night)
mod.valid$AUC[mod.valid$Model == "night"] <- auc.night

  
# calculate confusion matrix to understand ratios of true and false negatives and positives
p.night <- as.factor(as.numeric(predict(mod.night.upd , type="response")>t))
df_GLSimmersion_daily_night$drynight_f <- as.factor(df_GLSimmersion_daily_night$drynight)
cm.night <- confusionMatrix(p.night,df_GLSimmersion_daily_night$drynight_f, positive = "1")
cm.night

mod.valid$accuracy[mod.valid$Model == "night"] <- cm.night$overall[[1]]*100
mod.valid$sensitivity[mod.valid$Model == "night"] <- cm.night$byClass[["Sensitivity"]]
mod.valid$specificity[mod.valid$Model == "night"] <- cm.night$byClass[["Specificity"]]
mod.valid$Pos.PP[mod.valid$Model == "night"] <- cm.night$byClass[["Pos Pred Value"]]
mod.valid$Neg.PP[mod.valid$Model == "night"] <- cm.night$byClass[["Neg Pred Value"]]
mod.valid$Precision[mod.valid$Model == "night"] <- cm.night$byClass[["Precision"]]
mod.valid$Recall[mod.valid$Model == "night"] <- cm.night$byClass[["Recall"]]


# model predictions
pred.night.br_stage <- ggemmeans(mod.night.upd, terms = c("known_br_stage")) %>%
  tibble() %>%
  mutate(labels = case_when(x == "S0" ~"Pre-breeding", 
                            x == "S1" ~ "Incubation", 
                            x == "S2" ~ "Chick rearing", 
                            x == "NB" ~ "Non-breeding", 
                            .default = "Unkown")) %>%
  select(labels, predicted, std.error, conf.low, conf.high) %>%
  rename('Known breeding stage' = labels,
         'Estimate' = predicted,
         's.e.' = std.error,
         '2.5%CI' = conf.low,
         '97.5%CI' = conf.high) %>%
  mutate(time_period = "night")


pred.night <- ggpredict(mod.night.upd, terms = c("month_f", "known_br_stage"))
plot(pred.night)+
  scale_color_manual(name = "Breeding\nstage", values = br_stage_cols,
                                   labels = c("Pre-breeding", "Incubation", "Chick rearing", "Non-breeding", "Unkown"))+
  theme_bw()

df.pred.night <- pred.night %>%
  tibble() %>%
  rename("Month" = x) %>%
  mutate(time_period = "night")


### dry days ####

options(na.action = na.fail)

df_GLSimmersion_daily_day <- df_GLSimmersion_daily_day %>%
  mutate(month_f = as.factor(month),
         year_f = as.factor(year))

names(df_GLSimmersion_daily_day)

mod.day <- glmer(dryday ~ month_f + Sex + known_br_stage +  (1|ID), 
                   data = df_GLSimmersion_daily_day,
                   family = "binomial")
summary(mod.day)

dredge.day <- dredge(mod.day) 

dredge.day.out <- dredge.day  %>%
  tibble() %>%
  select(-c(`(Intercept)`, weight)) %>%
  flextable() %>%
  colformat_double(j = c(5,6,7), digits = 2) %>% # round numbers of specific columns to 2 decimal places
  add_header_row(values = as_paragraph(as_chunk(c("Explanatory variables", "Model selection metrics"))), colwidths = c(3,4), top = TRUE) %>% # add header row
  set_header_labels(known_br_stage = 'Known breeding stage',
                    month_f = 'Month') %>%
  fontsize(size = 11, part = 'all') %>% # set font size for the table
  autofit()
dredge.day.out
save_as_docx(dredge.day.out, path = here("Tables", "Supplementary", "Mod_dredge_day.docx"), align = 'center')


mod.day.upd <- glmer(dryday ~ month_f + known_br_stage + (1|year_f) + (1|ID), 
                       data = df_GLSimmersion_daily_day,
                       family = "binomial")

summary(mod.day.upd)

modpred.day <- as.numeric(predict(mod.day.upd, type="response"))
roccurve.day <- pROC::roc(df_GLSimmersion_daily_day$dryday, modpred.day)

t.day <- pROC::coords(roccurve.day, "best", ret = "threshold", transpose = F)[1,]
mod.valid$Threshold[mod.valid$Model == "day"] <- t.day

auc.day <- pROC::auc(roccurve.day)
mod.valid$AUC[mod.valid$Model == "day"] <- auc.day


# calculate confusion matrix to understand ratios of true and false negatives and positives
p.day <- as.factor(as.numeric(predict(mod.day.upd , type="response")>t))
df_GLSimmersion_daily_day$dryday_f <- as.factor(df_GLSimmersion_daily_day$dryday)
cm.day <- confusionMatrix(p.day,df_GLSimmersion_daily_day$dryday_f, positive = "1")
cm.day

mod.valid$accuracy[mod.valid$Model == "day"] <- cm.day$overall[[1]]*100
mod.valid$sensitivity[mod.valid$Model == "day"] <- cm.day$byClass[["Sensitivity"]]
mod.valid$specificity[mod.valid$Model == "day"] <- cm.day$byClass[["Specificity"]]
mod.valid$Pos.PP[mod.valid$Model == "day"] <- cm.day$byClass[["Pos Pred Value"]]
mod.valid$Neg.PP[mod.valid$Model == "day"] <- cm.day$byClass[["Neg Pred Value"]]
mod.valid$Precision[mod.valid$Model == "day"] <- cm.day$byClass[["Precision"]]
mod.valid$Recall[mod.valid$Model == "day"] <- cm.day$byClass[["Recall"]]


# model predictions
pred.day.br_stage <- ggemmeans(mod.day.upd, terms = c("known_br_stage")) %>%
  tibble() %>%
  mutate(labels = case_when(x == "S0" ~"Pre-breeding", 
                            x == "S1" ~ "Incubation", 
                            x == "S2" ~ "Chick rearing", 
                            x == "NB" ~ "Non-breeding", 
                            .default = "Unkown")) %>%
  select(labels, predicted, std.error, conf.low, conf.high) %>%
  rename('Known breeding stage' = labels,
         'Estimate' = predicted,
         's.e.' = std.error,
         '2.5%CI' = conf.low,
         '97.5%CI' = conf.high) %>%
  mutate(time_period = "day")


pred.day <- ggpredict(mod.day.upd, terms = c("month_f", "known_br_stage"))
plot(pred.day)+
  scale_color_manual(name = "Breeding\nstage", values = br_stage_cols,
                     labels = c("Pre-breeding", "Incubation", "Chick rearing", "Non-breeding", "Unkown"))+
  theme_bw()

df.pred.day <- pred.day %>%
  tibble() %>%
  rename("Month" = x) %>%
  mutate(time_period = "day")


### combine model validation and parameters to export

mod.valid.out <- mod.valid %>%
  flextable() %>%
  colformat_double(j = c(2:10), digits = 2) %>% # round numbers of specific columns to 2 decimal places
  set_header_labels(accuracy = 'Accuracy',
                    sensitivity = 'Sensitivity',
                    specificity = 'Specificity',
                    Pos.PP = 'Positive predictive power',
                    Neg.PP = 'Negative predictive power') %>%
  fontsize(size = 11, part = 'all') # set font size for the table
  
  
mod.valid.out
save_as_docx(mod.valid.out, path = here("Tables", "Supplementary", "Mod_AUC_night_day.docx"), align = 'center')


pred_br_stage <- pred.night.br_stage %>%
  bind_rows(., pred.day.br_stage) %>%
  select(time_period, `Known breeding stage`, Estimate, s.e.)
  
pred_br_stage_out <- pred_br_stage %>%
  pivot_wider(names_from = time_period, values_from = c(Estimate, s.e.)) %>%
  select(`Known breeding stage`, Estimate_night, s.e._night,  Estimate_day, s.e._day) %>%
  flextable() %>%
  set_header_labels(Estimate_night = 'Estimate',
                    Estimate_day = 'Estimate',
                    s.e._night = '± s.e.',
                    s.e._day = '± s.e.') %>%
  colformat_double(j = c(2,3,4,5), digits = 2) %>% # round numbers of specific columns to 2 decimal places
  add_header_row(values = as_paragraph(as_chunk(c("Known breeding stage", "Probability of dry night", "Probability of dry day"))), colwidths = c(1,2,2), top = TRUE) %>%# add header row
  merge_v(j = c(1), part = 'header') %>% # merge column names together
  autofit()
pred_br_stage_out
save_as_docx(mod.valid.out, path = here("Tables", "Mod_pred_night_day_brstage.docx"), align = 'center')

ggplot(pred_br_stage, aes(x = ))

