
#-----------------------------#
## Load required libraries ####
#-----------------------------#

library(tidyverse) #installed using install.packages("tidyverse")
library(lubridate) #installed as part of the tidyverse but needs to be loaded manually *NB. soon to be core!
library(sf)
library(data.table)
library(here) #for reproducible filepaths

#--------------------------#
##0. Pre-flight checks  ####
#--------------------------#

## Data processed following ExMove workflow
## Data contain ID, timestamp and sensor data (location/immersion/light)
## All data cropped to deployment period, only

#---------------------------#
##1. Read in data files  ####
#---------------------------#

df_GLSlocs <- read_csv(here("Data", "WorkingDataFrames", "RFB_GLSlocs_diagnostic.csv"))
df_GLSlight <- read_csv(here("Data", "WorkingDataFrames", "RFB_GLSlight_clean.csv"))
df_GLSimmersion <- read_csv(here("Data", "WorkingDataFrames", "RFB_GLSimmersion_clean.csv"))

head(df_GLSlocs)
head(df_GLSlight)
head(df_GLSimmersion)


#---------------------------#
##2. Create rolling means  ####
#---------------------------#


df_GLSlocs_weekly <- df_GLSlocs %>%
  group_by(ID) %>%
  mutate(Lon_rolling = frollmean(Lon, n = 14, fill = NA, align = "center"),
         Lat_rolling = frollmean(Lat, n = 14, fill = NA, align = "center"),
         cpdist_rolling_km = frollmean(cpdist, n = 14, fill = NA, align = "center")/1000) %>%
  ungroup()


### convert to indian ocean time before calculating daily metric


df_GLSimmersion_daily <- df_GLSimmersion %>% 
  mutate(DateTime_GMT = as_datetime(DateTime, tz = "GMT"),
         DateTime_local = with_tz(DateTime_GMT, tzone="Indian/Chagos"),
         Date_local = lubridate::as_date(DateTime_local),
         Time_local = hms::as_hms(DateTime_local),
         year = year(DateTime_local)) %>%
  group_by(ID, Date_local, year) %>%
  summarise(daily.imm.fixes = length(Immersion),
            prop.dry = sum(Immersion == 0)/daily.imm.fixes,
            prop.wet = sum(Immersion == 20)/daily.imm.fixes,
            prop.int = sum(Immersion %in% c(1:19))/daily.imm.fixes,
            sum.props = sum(prop.dry, prop.wet, prop.int)) %>%
  group_by(ID) %>%
  mutate(prop.dry_rolling = frollmean(prop.dry, n = 7, fill = NA, align = "center"),
         prop.wet_rolling = frollmean(prop.wet, n = 7, fill = NA, align = "center"),
         prop.int_rolling = frollmean(prop.int, n = 7, fill = NA, align = "center")) %>%
  ungroup() 

head(df_GLSimmersion_daily)

df_GLS_comb <- df_GLSlocs_weekly %>%
  left_join(., df_GLSimmersion_daily)

 
#---------------------------#
##3. Plot individual locations  ####
#---------------------------#
# 
# 
# world_map <- map_data("world")
# MPA <- read.csv("/Users/at687/Documents/BIOT/Non-breeding/Data/Map Indian Ocean/MPA.csv", as.is = T)
# 
# 
# 
# ind_rolling_locs <- ggplot() +
#   coord_cartesian(xlim = c(64, 80), ylim = c(-35, 22), expand = F)+
#   #geom_point(data = df_GLSlocs, aes(x = Lon, y = Lat, group = ID), inherit.aes = FALSE, colour = "gray60", lwd = 0.2, alpha = 0.1)+ #  col = "#053061"
#   #geom_path(data = df_GLSlocs, aes(x = Lon, y = Lat, group = ID), inherit.aes = FALSE, colour = "gray60", lwd = 0.2, alpha = 0.9)+ #  col = "#053061"
#   geom_path(data = df_GLSlocs, aes(x = Lon, y = Lat, group = ID), inherit.aes = FALSE, colour = "gray50", lwd = 0.2, alpha = 0.5)+ #  col = "#053061"
#   geom_path(data = df_GLSlocs, aes(x = Lon_rolling, y = Lat_rolling, group = ID, colour = DateTime), inherit.aes = FALSE, lwd = 0.3, alpha = 0.9)+ #  col = "#053061" colour = "gray10"
#   geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill="gray80", colour = "gray80")+
#   geom_path(data = MPA, aes(x = Long, y = Lat), inherit.aes = FALSE, col = "gray20", lwd = 0.2)+
#   geom_point(data = df_GLSlocs[1,], aes(x = CPLon, y = CPLat), inherit.aes = FALSE, colour = "gray20")+
#   scale_colour_viridis_c()+
#   theme_light()+
#   theme(panel.grid = element_blank())+labs(x = "Longitude (°E)", y = "Latitude (°N)")+
#   facet_wrap(.~ID)

trans.coef <- max(df_GLSlocs_weekly$cpdist_rolling_km, na.rm = T)

ind_rolling_activity <- ggplot(df_GLSlocs_weekly, aes(x = DateTime, y = cpdist_rolling_km))+
  geom_line()+
  scale_y_continuous(name = "Colony Distance", sec.axis = sec_axis(trans = ~./trans.coef, name = "Proportion"))+
  geom_line(data = df_GLSimmersion_daily, aes(x = Date, y = prop.dry * trans.coef), col = "blue")+
  facet_grid(ID~.)+
  theme_light()



# png(here("Figures", "Individual_rolling_locations.png"),width = 12, height = 20, units = "cm", res = 300)
# print(ind_rolling_locs)
# dev.off()


# 
# png(here("Figures", "Individual_rolling_dry.png"),width = 12, height = 20, units = "cm", res = 300)
# print(ind_rolling_activity)
# dev.off()

p.dist.dry<- ggplot(df_GLS_comb, aes(x = cpdist_rolling_km, y = prop.dry))+
  geom_point(alpha = 0.4, cex = 0.2)

png(here("Figures", "Dist_dry.png"), width = 8, height = 8, units = "cm", res = 200)
print(p.dist.dry)
dev.off()


ind_rolling_activity <- ggplot(df_GLSlocs_weekly, aes(x = DateTime, y = cpdist_rolling_km))+
  geom_line()+
  scale_y_continuous(name = "Colony Distance", sec.axis = sec_axis(trans = ~./trans.coef, name = "Proportion"))+
  geom_line(data = df_GLSimmersion_daily, aes(x = Date, y = prop.dry * trans.coef), col = "blue")+
  facet_grid(ID~.)+
  theme_light()

p.prop.dry <- ggplot(df_GLSimmersion_daily, aes(x = Date_local, y = prop.dry))+
  geom_line()+
  facet_grid(ID~year, scales = "free_x")
p.prop.dry

indsubset <- subset(df_GLSimmersion_daily, year == "2018" & ID %in% unique(df_GLSimmersion_daily$ID)[5:8])
p.prop.dry2 <- ggplot(subset(indsubset, year == "2018"), aes(x = Date_local, y = prop.dry_rolling))+
  geom_line()+
  facet_grid(ID~.)
p.prop.dry2

drysubset <- subset(df_GLSimmersion_daily, prop.dry == "1" & year == "2018")
p.prop.dry100 <- ggplot(drysubset, aes(x = Date_local, y = ID))+
  geom_point()
p.prop.dry100



df_GLSimmersion_night <- df_GLSimmersion %>% 
  mutate(DateTime_GMT = as_datetime(DateTime, tz = "GMT"),
         DateTime_local = with_tz(DateTime_GMT, tzone="Indian/Chagos"),
         Date_local = lubridate::as_date(DateTime_local),
         Time_local = hms::as_hms(DateTime_local)) %>%
  filter(Time_local < hms(hours(6))) %>%
  group_by(ID, Date_local) %>%
  summarise(daily.imm.fixes = length(Immersion),
            prop.dry = sum(Immersion == 0)/daily.imm.fixes,
            prop.wet = sum(Immersion == 20)/daily.imm.fixes,
            prop.int = sum(Immersion %in% c(1:19))/daily.imm.fixes,
            sum.props = sum(prop.dry, prop.wet, prop.int))
