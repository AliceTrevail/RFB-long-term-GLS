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
## Calculate proportions dry/wet ####
#-----------------------------#

# sort by earliest tag date

# Group the data by ID and find the minimum date for each ID
id_dates <- df_GLSimmersion_nightID %>% group_by(ID) %>% summarize(min_date = min(Date_local), max_date = max(Date_local))

# Sort the IDs based on their earliest date
sorted_ids <- id_dates %>% arrange(min_date) %>% pull(ID)

# Use the sorted IDs to reorder the factor levels of the ID variable in the data
df_GLSimmersion_nightID$ID <- factor(df_GLSimmersion_nightID$ID, levels = sorted_ids)

# summarise by time period, calculate proportions dry, and identify dry periods

df_GLSimmersion_daily_24hr <- df_GLSimmersion_nightID %>% 
  group_by(ID, Date_local, year) %>%
  summarise(daily.imm.fixes = length(Immersion),
            prop.dry = sum(Immersion == 0)/daily.imm.fixes,
            prop.wet = sum(Immersion == 20)/daily.imm.fixes,
            prop.int = sum(Immersion %in% c(1:19))/daily.imm.fixes,
            sum.props = sum(prop.dry, prop.wet, prop.int)) %>%
  ungroup() %>%
  mutate(dry24hr = case_when(prop.dry >= 0.95 ~ 1, .default = 0)) 


df_GLSimmersion_daily_day <- df_GLSimmersion_nightID %>% 
  filter(dorn == "day") %>%
  group_by(ID, Date_local, year) %>%
  summarise(daily.imm.fixes = length(Immersion),
            prop.dry = sum(Immersion == 0)/daily.imm.fixes,
            prop.wet = sum(Immersion == 20)/daily.imm.fixes,
            prop.int = sum(Immersion %in% c(1:19))/daily.imm.fixes,
            sum.props = sum(prop.dry, prop.wet, prop.int)) %>%
  ungroup() %>%
  mutate(dryday = case_when(prop.dry >= 0.95 ~ 1, .default = 0)) 


df_GLSimmersion_daily_night <- df_GLSimmersion_nightID %>% 
  filter(dorn == "night") %>%
  group_by(ID, NightID, NightDate, year) %>%
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

# plot defaults:
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
  theme_light()
  )


p.prop.dry24hr <- ggplot(subset(df_GLSimmersion_daily_24hr, dry24hr == 1& !year == "2020"), aes(x = Date_local, y = ID))+
  plot_base+
  geom_point(alpha = 0.6)+
  labs(title = "Temporal distribution of dry dates")
p.prop.dry24hr


p.prop.dryday <- ggplot(subset(df_GLSimmersion_daily_day, dryday == 1 & !year == "2020"), aes(x = Date_local, y = ID))+
  plot_base+
  geom_point(alpha = 0.6)+
  labs(title = "Temporal distribution of dry days")
p.prop.dryday


p.prop.drynight <- ggplot(subset(df_GLSimmersion_daily_night, drynight == 1 & !year == "2020"), aes(x = NightDate, y = ID))+
  plot_base+
  geom_point(alpha = 0.6)+
  labs(title = "Temporal distribution of dry nights")
p.prop.drynight


ggsave(plot = p.prop.drynight, filename = here("Figures", "Overnight_dry.png"),
       width = 18, height = 22, units = "cm")


ggsave(plot = p.prop.dryday, filename = here("Figures", "Supplementary", "Overnight_day.png"),
       width = 18, height = 22, units = "cm")


ggsave(plot = p.prop.dry24hr, filename = here("Figures", "Supplementary", "Overnight_24hr.png"),
       width = 18, height = 22, units = "cm")


