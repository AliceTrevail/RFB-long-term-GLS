### RFB non-breeding
### Q: do they have an extended pelagic period?
### Q: is there an annual pattern in wet / dry?


#-----------------------------#
## Load required libraries ####
#-----------------------------#

library(suncalc)
library(tidyverse) #installed using install.packages("tidyverse")
library(sf)
library(data.table)
library(here) #for reproducible filepaths


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

# remove intermediate files
rm(list=ls()[!ls() %in% c("df_GLSimmersion","df_GLSimmersion_suntimes")]) #could just specify objects to keep? (no errors)



#-----------------------------#
## Calculate proportions dry/wet ####
#-----------------------------#



df_GLSimmersion_daily_24hr <- df_GLSimmersion_suntimes %>% 
  group_by(ID, Date_local, year) %>%
  summarise(daily.imm.fixes = length(Immersion),
            prop.dry = sum(Immersion == 0)/daily.imm.fixes,
            prop.wet = sum(Immersion == 20)/daily.imm.fixes,
            prop.int = sum(Immersion %in% c(1:19))/daily.imm.fixes,
            sum.props = sum(prop.dry, prop.wet, prop.int)) %>%
  ungroup() %>%
  mutate(dry24hr = case_when(prop.dry == 1 ~ 1, .default = 0)) 


df_GLSimmersion_daily_day <- df_GLSimmersion_suntimes %>% 
  filter(dorn == "day") %>%
  group_by(ID, Date_local, year) %>%
  summarise(daily.imm.fixes = length(Immersion),
            prop.dry = sum(Immersion == 0)/daily.imm.fixes,
            prop.wet = sum(Immersion == 20)/daily.imm.fixes,
            prop.int = sum(Immersion %in% c(1:19))/daily.imm.fixes,
            sum.props = sum(prop.dry, prop.wet, prop.int)) %>%
  ungroup() %>%
  mutate(dryday = case_when(prop.dry == 1 ~ 1, .default = 0)) 



df_GLSimmersion_daily_night <- df_GLSimmersion_suntimes %>% 
  filter(dorn == "night") %>%
  group_by(ID, Date_local, year) %>%
  summarise(daily.imm.fixes = length(Immersion),
            prop.dry = sum(Immersion == 0)/daily.imm.fixes,
            prop.wet = sum(Immersion == 20)/daily.imm.fixes,
            prop.int = sum(Immersion %in% c(1:19))/daily.imm.fixes,
            sum.props = sum(prop.dry, prop.wet, prop.int))%>%
  ungroup() %>%
  mutate(drynight = case_when(prop.dry == 1 ~ 1, .default = 0)) 




#-----------------------------#
## Plot occurence of dry periods ####
#-----------------------------#


p.prop.dry24hr <- ggplot(subset(df_GLSimmersion_daily_24hr, dry24hr == 1& !year == "2020"), aes(x = Date_local, y = ID))+
  geom_point(alpha = 0.6)+
  labs(title = "Temporal distribution of 24hr periods recorded as dry by GLS immersion loggers")+
  facet_wrap(~year, ncol = 1, scales = "free_x")+
  theme_bw()
p.prop.dry24hr


p.prop.dryday <- ggplot(subset(df_GLSimmersion_daily_day, dryday == 1 & !year == "2020"), aes(x = Date_local, y = ID))+
  geom_point(alpha = 0.6)+
  labs(title = "Temporal distribution of days recorded as dry by GLS immersion loggers")+
  facet_wrap(~year, ncol = 1, scales = "free_x")+
  theme_bw()
p.prop.dryday


p.prop.drynight <- ggplot(subset(df_GLSimmersion_daily_night, drynight == 1 & !year == "2020"), aes(x = Date_local, y = ID))+
  geom_point(alpha = 0.6)+
  labs(title = "Temporal distribution of nights recorded as dry by GLS immersion loggers")+
  facet_wrap(~year, ncol = 1, scales = "free_x")+
  theme_bw()
p.prop.drynight

png(here("Figures", "Overnight_dry.png"),width = 30, height = 20, units = "cm", res = 300)
print(p.prop.drynight)
dev.off()

png(here("Figures", "Daytime_dry.png"),width = 30, height = 20, units = "cm", res = 300)
print(p.prop.dryday)
dev.off()


png(here("Figures", "24hr_dry.png"),width = 30, height = 20, units = "cm", res = 300)
print(p.prop.dry24hr)
dev.off()
