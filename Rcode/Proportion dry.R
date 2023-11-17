### RFB non-breeding
### Q: do they have an extended pelagic period?
### Q: is there an annual pattern in wet / dry?

#-----------------------------#
## Load required libraries ####
#-----------------------------#
pacman::p_load(suncalc, sf, data.table, here, scales, tidyverse, lubridate, lme4, MuMIn, caret, ggeffects, flextable, patchwork, cowplot)

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
  mutate(month = month(NightDate)) %>%
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
## Monthly summary of dry periods ####
#-----------------------------#

df_monthly_night <- df_GLSimmersion_daily_night %>%
  group_by(ID, year, month) %>%
  summarise(n = n(),
            sum_dry = sum(drynight)) %>%
  mutate(max_n = case_when(month == 2 ~ 28,
                           month %in% c(4,6,9,11) ~ 30,
                           .default = 31)) %>%
  filter(n == max_n) %>% # filter to complete tracked months
  mutate(percent_dry = (sum_dry/n)*100)

# mean sum nights dry per month
mean_sum_night <- mean(df_monthly_night$sum_dry)
# standard error
se_sum_night <- sd(df_monthly_night$sum_dry) / sqrt(length(df_monthly_night$sum_dry))

mean_nights_yr <- (mean_sum_night)*12
max_nights_yr <- (mean_sum_night+se_sum_night)*12
min_nights_yr <- (mean_sum_night-se_sum_night)*12


# mean percent nights dry per month
mean(df_monthly_night$percent_dry)
# standard error
sd(df_monthly_night$percent_dry) / sqrt(length(df_monthly_night$percent_dry))
# range
min(df_monthly_night$percent_dry)
max(df_monthly_night$percent_dry)



df_monthly_day <- df_GLSimmersion_daily_day %>%
  #filter(known_br_stage == "S1") %>%
  group_by(ID, year, month) %>%
  summarise(n = n(),
            sum_dry = sum(dryday)) %>%
  mutate(max_n = case_when(month == 2 ~ 28,
                           month %in% c(4,6,9,11) ~ 30,
                           .default = 31)) %>%
  filter(n == max_n) %>% # filter to complete tracked months
  mutate(percent_dry = (sum_dry/n)*100)

# mean sum nights dry per month
mean_sum_day <- mean(df_monthly_day$sum_dry)
# standard error
se_sum_day <- sd(df_monthly_day$sum_dry) / sqrt(length(df_monthly_day$sum_dry))

mean_days_yr <- (mean_sum_day)*12
max_days_yr <- (mean_sum_day+se_sum_day)*12
min_days_yr <- (mean_sum_day-se_sum_day)*12


# mean percent nights dry per month
mean(df_monthly_day$percent_dry)
# standard error
sd(df_monthly_day$percent_dry) / sqrt(length(df_monthly_day$percent_dry))
# range
min(df_monthly_day$percent_dry)
max(df_monthly_day$percent_dry)

mean_nights_yr+mean_days_yr
max_nights_yr+max_days_yr
min_nights_yr+min_days_yr


#-----------------------------#
## Monthly summary of not dry periods ####
#-----------------------------#

df_nights_notdry <- df_GLSimmersion_daily_night %>%
  group_by(ID) %>%
  mutate(same_lag = ifelse(drynight == lag(drynight), "TRUE", "FALSE"), # is the night still dry or not dry?
         same_lead = ifelse(drynight == lead(drynight), "TRUE", "FALSE"), # is the next night transition between dry/not dry?
         label = case_when(drynight == "0" & same_lag == "FALSE" ~ "first", # label first night not dry
                           drynight == "0" & same_lead == "FALSE" ~ "last")) %>% # label last night not dry
  filter(drynight == "0") %>% 
  mutate(ndry_row = case_when(label == "first" ~ cur_group_rows())) %>% # assign group number based on row number for first points of trip only
  fill(ndry_row) %>% # fill NAs with group number
  group_by(ndry_row) %>%
  ungroup() %>%
  group_split(ID) %>%
  purrr::map_df(~.x %>% group_by(ndry_row) %>% 
                  mutate(ndry_num = cur_group_id())) %>% # assign sequential trip number
  ungroup() %>% 
  mutate(ndryID = paste0(ID, "_", ndry_num)) %>% # assign unique trip ID based on ID and trip number
  select(-c(same_lag, same_lead, label, ndry_row, ndry_num)) %>% # remove intermediate columns
  group_by(ndryID) %>%
  mutate(ndryDate = first(NightDate)) %>%
  ungroup()%>%
  st_drop_geometry() # remove the geometry column

df_consecutivenights_notdry <- df_nights_notdry %>%
  mutate(month = month(ndryDate)) %>%
  group_by(ID, month, ndryID) %>%
  summarise(cons_notdry = n())

mean(df_consecutivenights_notdry$cons_notdry)
sd(df_consecutivenights_notdry$cons_notdry) / sqrt(length(df_consecutivenights_notdry$cons_notdry))
min(df_consecutivenights_notdry$cons_notdry)
max(df_consecutivenights_notdry$cons_notdry)


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
br_stage_cols <- c("#fde725", "#7ad151", "#22a884", "#de4968", "#000000")

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
                     labels = c("Pre-breeding", "Incubation", "Chick rearing", "Non-breeding", "Unknown")),
  guides(colour = guide_legend(override.aes = list(alpha = 1))),
  theme_light(base_size = 12)
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
  geom_point(alpha = 0.6)
p.prop.drynight

RFBimg <- "/Users/at687/Library/CloudStorage/OneDrive-UniversityofExeter/BIOT/Seabird graphics/booby_roosting.png"

t <- ggdraw() +
  draw_plot(p.prop.drynight) +
  draw_image(
    RFBimg, x = 0.11, y = 0.14,
    width = 0.15
  )


p.prop.drynight_2020 <- ggplot(subset(df_GLSimmersion_daily_night, drynight == 1), aes(x = NightDate, y = ID, col = known_br_stage))+
  plot_base+ rect_2020+
  geom_point(alpha = 0.6)+
  labs(title = "Temporal distribution of dry nights")
p.prop.drynight_2020

ggsave(plot = t, filename = here("Figures", "Dry_night_2018_2019.png"),
       width = 24, height = 22, units = "cm")

ggsave(plot = p.prop.dryday, filename = here("Figures", "Supplementary", "Dry_day.png"),
       width = 24, height = 26, units = "cm")

ggsave(plot = p.prop.dry24hr, filename = here("Figures", "Supplementary", "Dry_24hr.png"),
       width = 24, height = 26, units = "cm")

ggsave(plot = p.prop.drynight_2020, filename = here("Figures", "Supplementary", "Dry_night_allyears.png"),
       width = 24, height = 26, units = "cm")


# Reviewer comment test: plot landscape

rects_retrieve_w <- id_dates %>% 
  mutate(ymin = as.numeric(rev(ID)) - 0.5, 
         ymax = as.numeric(rev(ID)) + 0.5, 
         xmin = max_date, 
         xmax = as_date("2020-02-28"),
         year = year(max_date))


plot_base_nofacet <- list(
  geom_rect(data = rects_deploy, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey80", alpha = 0.5, inherit.aes = FALSE),
  geom_rect(data = rects_retrieve_w, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey80", alpha = 0.5, inherit.aes = FALSE),
  scale_y_discrete(limits = rev(sorted_ids)),
  scale_x_date(expand = c(0,0), date_breaks = "1 month", date_labels = "%b-%d", name = "Date (local time; GMT+6)"),
  scale_color_manual(name = "Breeding\nstage", values = br_stage_cols,
                     labels = c("Pre-breeding", "Incubation", "Chick rearing", "Non-breeding", "Unknown")),
  guides(colour = guide_legend(override.aes = list(alpha = 1))),
  theme_light(base_size = 12)
)

p.prop.drynight_wide <- ggplot(subset(df_GLSimmersion_daily_night, drynight == 1), aes(x = NightDate, y = ID, col = known_br_stage))+
  plot_base_nofacet+ 
  geom_point(alpha = 0.6)+
  labs(title = "Temporal distribution of dry nights")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p.prop.drynight_wide


t_w <- ggdraw() +
  draw_plot(p.prop.drynight_wide) +
  draw_image(
    RFBimg, x = 0.11, y = -0.16,
    width = 0.12
  )

ggsave(plot = t_w, filename = here("Figures", "Dry_night_wide.png"),
       width = 30, height = 15, units = "cm")


#-----------------------------#
## Model dry periods ####
#-----------------------------#

options(na.action = na.fail)

#-----------------------------#
### combine day and night data ####
#-----------------------------#

df_GLSimmersion_daily_day <- df_GLSimmersion_daily_day %>%
  mutate(time_period = "day")%>%
  rename(dry = dryday)

df_GLSimmersion_daily_night <- df_GLSimmersion_daily_night %>%
  mutate(time_period = "night") %>%
  rename(dry = drynight)

df_GLSimmersion_daily_comb <- df_GLSimmersion_daily_day %>%
  bind_rows(df_GLSimmersion_daily_night) %>%
  mutate(month_f = as.factor(month),
         year_f = as.factor(year))

#-----------------------------#
### run full model ####
#-----------------------------#

mod.comb <- glmer(dry ~ month_f:time_period + known_br_stage:time_period + 
                    time_period + month_f + known_br_stage + Sex + (1|ID), 
                 data = df_GLSimmersion_daily_comb,
                 family = "binomial")
summary(mod.comb)

#-----------------------------#
### model selection ####
#-----------------------------#

dredge.comb <- dredge(mod.comb) 

# save model selection results using flextable
dredge.comb.out <- dredge.comb  %>%
  tibble() %>%
  select(-c(`(Intercept)`, weight)) %>%
  flextable() %>%
  colformat_double(j = c(8,9,10), digits = 2) %>% # round numbers of specific columns to 2 decimal places
  add_header_row(values = as_paragraph(as_chunk(c("Explanatory variables", "Interactions", "Model selection metrics"))), colwidths = c(4,2,4), top = TRUE) %>% # add header row
  set_header_labels(known_br_stage = 'Known breeding stage',
                    month_f = 'Month',
                    time_period = 'Time period',
                    `known_br_stage:time_period` = "Known breeding stage : Time period",
                    `month_f:time_period` = "Month : Time period") %>%
  fontsize(size = 11, part = 'all') # set font size for the table
dredge.comb.out
save_as_docx(dredge.comb.out, path = here("Tables", "Supplementary", "Mod_dredge_comb.docx"), align = 'center')


# update to most parsimonious model
mod.comb.upd <- glmer(dry ~ month_f:time_period + known_br_stage:time_period + 
                        time_period + month_f + known_br_stage + (1|ID), 
                      data = df_GLSimmersion_daily_comb,
                      family = "binomial")
summary(mod.comb.upd)


#-----------------------------#
### model validation ####
#-----------------------------#

# set up table for model validation metrics

diagnostics <- c("AUC", "Threshold", "Accuracy", "Sensitivity", "Specificity", 
                 "Positive Pred Value", "Negative Pred Value", "Precision", "Recall")
mod.valid <- as.data.frame(matrix(nrow = 9, ncol = 2))
colnames(mod.valid) <- c("Diagnostic", "Value")
mod.valid$Diagnostic <- diagnostics

# calculate roc curve
modpred.comb <- as.numeric(predict(mod.comb.upd, type="response"))
roccurve.comb <- pROC::roc(df_GLSimmersion_daily_comb$dry, modpred.comb)

t.comb <- pROC::coords(roccurve.comb, "best", ret = "threshold", transpose = F)[1,]
mod.valid$Value[mod.valid$Diagnostic == "Threshold"] <- t.comb

auc.comb <- pROC::auc(roccurve.comb)
mod.valid$Value[mod.valid$Diagnostic == "AUC"] <- auc.comb


# calculate confusion matrix to understand ratios of true and false negatives and positives
p.comb <- as.factor(as.numeric(predict(mod.comb.upd , type="response")>t.comb))
df_GLSimmersion_daily_comb$dry_f <- as.factor(df_GLSimmersion_daily_comb$dry)
cm.comb <- confusionMatrix(p.comb, df_GLSimmersion_daily_comb$dry_f, positive = "1")
cm.comb

mod.valid$Value[mod.valid$Diagnostic == "Accuracy"] <- cm.comb$overall[[1]]*100
mod.valid$Value[mod.valid$Diagnostic == "Sensitivity"] <- cm.comb$byClass[["Sensitivity"]]
mod.valid$Value[mod.valid$Diagnostic == "Specificity"] <- cm.comb$byClass[["Specificity"]]
mod.valid$Value[mod.valid$Diagnostic == "Positive Pred Value"] <- cm.comb$byClass[["Pos Pred Value"]]
mod.valid$Value[mod.valid$Diagnostic == "Negative Pred Value"] <- cm.comb$byClass[["Neg Pred Value"]]
mod.valid$Value[mod.valid$Diagnostic == "Precision"] <- cm.comb$byClass[["Precision"]]
mod.valid$Value[mod.valid$Diagnostic == "Recall"] <- cm.comb$byClass[["Recall"]]


mod.valid.out <- mod.valid %>%
  flextable() %>%
  colformat_double(j = 2, digits = 2) %>% # round numbers of specific columns to 2 decimal places
  fontsize(size = 11, part = 'all') %>% # set font size for the table
  autofit()

mod.valid.out
save_as_docx(mod.valid.out, path = here("Tables", "Supplementary", "Mod_AUC_night_day.docx"), align = 'center')


#-----------------------------#
### model predictions: time & breeding stage ####
#-----------------------------#

# extract predictions
pred.time.br_stage <- ggemmeans(mod.comb.upd, terms = c("time_period","known_br_stage")) %>%
  tibble() %>%
  select(x, group, predicted, std.error, conf.low, conf.high) %>%
  rename('time_period' = x,
         'known_br_stage' = group)%>%
  mutate(known_br_stage = factor(known_br_stage, levels = c("S0", "S1", "S2", "NB", "unknown")))

# plot predictions
p.time.br_stage <- ggplot(pred.time.br_stage, aes(x = known_br_stage, y = predicted, col = time_period))+
  geom_point()+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1)+
  scale_y_continuous(limits = c(0,1), expand = c(0,0))+
  scale_x_discrete(labels = c("Pre-breeding", "Incubation", "Chick rearing", "Non-breeding", "Unknown"))+
  scale_color_viridis_d(begin = 0.6, end = 0.2, name = "Time period", option = 'mako')+
  theme_light()+
  labs(x= "Known breeding stage", y = "Predicted probability dry")
p.time.br_stage

# tabulate predictions
pred_br_stage_out <- pred.time.br_stage %>%
  select(time_period, known_br_stage, predicted, std.error) %>%
  pivot_wider(names_from = time_period, values_from = c(predicted, std.error)) %>%
  select(known_br_stage, predicted_night, std.error_night,  predicted_day, std.error_day) %>%
  mutate(known_br_stage = case_when(known_br_stage == "S0" ~"Pre-breeding", 
                                    known_br_stage == "S1" ~ "Incubation", 
                                    known_br_stage == "S2" ~ "Chick rearing", 
                                    known_br_stage == "NB" ~ "Non-breeding", 
                                    .default = "Unknown")) %>%
  flextable() %>%
  set_header_labels(known_br_stage = "Known breeding stage",
                    predicted_night = 'Estimate',
                    predicted_day = 'Estimate',
                    std.error_night = '± s.e.',
                    std.error_day = '± s.e.') %>%
  colformat_double(j = c(2,3,4,5), digits = 2) %>% # round numbers of specific columns to 2 decimal places
  add_header_row(values = as_paragraph(as_chunk(c("Known breeding stage", "Probability of dry night", "Probability of dry day"))), colwidths = c(1,2,2), top = TRUE) %>%# add header row
  merge_v(j = c(1), part = 'header') %>% # merge column names together
  autofit()
pred_br_stage_out
save_as_docx(pred_br_stage_out, path = here("Tables", "Mod_pred_brstage.docx"), align = 'center')



#-----------------------------#
### model predictions: time & month ####
#-----------------------------#

# extract predictions
pred.comb <- ggpredict(mod.comb.upd, terms = c("time_period", "month_f")) %>%
  tibble() %>%
  select(x, group, predicted, std.error, conf.low, conf.high) %>%
  rename('time_period' = x,
         'month' = group) %>%
  mutate(month = month(as.numeric(month), label = TRUE))

# plot predictions
p.time.month <- ggplot(pred.comb, aes(x = month, y = predicted, col = time_period))+
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.5, position=position_dodge(width=0.5))+
  scale_color_viridis_d(begin = 0.6, end = 0.2, name = "Time period", option = 'mako')+
  scale_y_continuous(limits = c(0,1), expand = c(0,0))+
  theme_light()+
  labs(x= "Month", y = "Predicted probability dry")
p.time.month


#-----------------------------#
### Save plots ####
#-----------------------------#

patchwork.dry <- p.time.br_stage / p.time.month + 
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'a')


ggsave(plot = patchwork.dry, filename = here("Figures", "Predicted_dry.png"),
       width = 18, height = 18, units = "cm")
