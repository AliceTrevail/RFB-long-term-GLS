
#-----------------------------#
## Load required libraries ####
#-----------------------------#

library(adehabitatHR) #kernels
library(tidyverse) #installed using install.packages("tidyverse")
library(lubridate) #installed as part of the tidyverse but needs to be loaded manually *NB. soon to be core!
library(sf)
library(data.table)
library(here) #for reproducible filepaths
library(rgdal)
library(cowplot) #ggdraw
library(magick) # image_read
library(ggspatial)
library(ggpubr)
library(patchwork)

#--------------------------#
##0. Pre-flight checks  ####
#--------------------------#

## Data processed following code workflow
## Data contain ID, timestamp and sensor data (location/immersion/light)
## All data cropped to deployment period, only

#----------------------------#
## 1. Read in data files  ####
#----------------------------#

df_GLSlocs <- read_csv(here("Data", "WorkingDataFrames", "RFB_GLSlocs_filtered.csv"))

world_map <- map_data("world")

sf_world_map <- sf::st_as_sf(world_map, coords = c("long", "lat"), crs = 4326) 
sf_world_map = st_sf(
  aggregate(
    sf_world_map$geometry,
    list(sf_world_map$group),
    function(g){
      st_cast(st_combine(g),"POLYGON")
    }
  ))

MPA <- read_csv(here("Data", "MPA.csv"))


#--------------------------------#
## 2. BirdLife Distributions  ####
#--------------------------------#
# 
# # The input file geodatabase
# fgdb = "/Users/at687/Documents/BIOT/Non-breeding/Data/Birdlife distributions/BOTW/BOTW.gdb"
# 
# # List all feature classes in a file geodatabase
# subset(ogrDrivers(), grepl("GDB", name))
# fc_list <- ogrListLayers(fgdb)
# print(fc_list)
# 
# # Get bird taxonomy table
# bird_taxonomy_data <- sf::read_sf(dsn = fgdb, layer="BirdLife_Taxonomic_Checklist_V5")
# head(bird_taxonomy_data)
# 
# # Find a species in this data
# bird_info = subset(bird_taxonomy_data, ScientificName=='Sula sula')
# bird_info
# 
# # For each bird we want, query on reading...
# 
# RFB_range_maps <- sf::read_sf(dsn = fgdb,
#                               layer = "All_Species",
#                               query = "SELECT * from All_Species WHERE SISID=22696694")
# head(RFB_range_maps)
# 
# RFB_range_resident <- RFB_range_maps[RFB_range_maps$seasonal == "1",]
# RFB_range_resident.sp <- RFB_range_resident %>% st_as_sf(., crs = 4326) # %>% st_transform(crs = 3857)
# 
# #st_write(RFB_range_resident.sp, here("Data", "Range shape files", "RFB_range_resident.shp"))
RFB_range_resident.sp <- st_read(here("Data", "Range shape files", "RFB_range_resident.shp"))

# RFB_range_br <- RFB_range_maps[RFB_range_maps$seasonal == "2",]
# RFB_range_br.sp  <- RFB_range_br %>% st_as_sf(., crs = 4326) # %>% st_transform(crs = 3857)
# 
# #st_write(RFB_range_br.sp, here("Data", "Range shape files", "RFB_range_breeding.shp"))
RFB_range_br.sp <- st_read(here("Data", "Range shape files", "RFB_range_breeding.shp"))

#plot(RFB_range_resident.sp)
#plot(RFB_range_br.sp)

# Crop to Indian Ocean

ls_IOCrop <- list("IO" = c(xmin = 25, xmax = 125, ymin = -40, ymax = 25))

# convert list of extents to spatial polygon
sf_IOCrop <- ls_IOCrop %>%
  lapply(., function(x) st_sf(geometry = st_as_sfc(st_bbox(x, crs = 4326)))) %>%
  rbindlist() %>%
  st_as_sf() %>%
  st_transform(crs = 3857)

# create dataframe to plot to check extent
df_IOCrop <- sf_IOCrop %>% st_transform(crs = 4326) %>% st_coordinates() %>% as.data.frame()
df_RFB_range_br <- RFB_range_br.sp %>% st_coordinates() %>% as.data.frame()

ggplot() +
  coord_cartesian(xlim = c(20, 130), ylim = c(-45, 30), expand = F)+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill="gray80", colour = "gray80")+
  geom_point(data = df_RFB_range_br, aes(x = X, y = Y), col = "blue")+
  geom_point(data = df_GLSlocs[1,], aes(x = CPLon, y = CPLat), inherit.aes = FALSE, colour = "gray20")+
  geom_path(data = df_IOCrop, aes(x = X, y = Y))+
  theme_light()+
  theme(panel.grid = element_blank())+labs(x = "Longitude (°E)", y = "Latitude (°N)")

df_RFB_IO_br <- RFB_range_br.sp %>%
  dplyr::select(SISID, geometry) %>%
  st_transform(crs = 3857) %>%
  st_crop(., sf_IOCrop) %>%
  #aggregate(., ., FUN = mean, join = function(x,y) st_is_within_distance(x,y, dist = 100000)) %>% #within 100k
  st_transform(crs = 4326) %>%
  st_coordinates() %>% as.data.frame() %>%
  group_by(L2) %>%
  summarise(Lon = mean(X), Lat = mean(Y))


#----------------------------------#
## 3. Calculate daily locations ####
#----------------------------------#


### convert to indian ocean time then calculating daily location
df_GLSlocs_daily <- df_GLSlocs %>% 
  mutate(DateTime_GMT = as_datetime(DateTime, tz = "GMT"),
         DateTime_local = with_tz(DateTime_GMT, tzone="Indian/Chagos"),
         Date_local = lubridate::as_date(DateTime_local),
         Time_local = hms::as_hms(DateTime_local),
         year = year(DateTime_local)) %>%
  group_by(ID, Date_local) %>%
  summarise(Lon = mean(Lon),
            Lat = mean(Lat)) %>%
  mutate(Month = month(Date_local),
         Resolution = "daily") %>%
  ungroup() 

head(df_GLSlocs_daily)
unique(df_GLSlocs_daily$Month)


# Plot raw vs daily locations
df_locplot <- df_GLSlocs %>%
  mutate(Resolution = "12 hour") %>%
  bind_rows(., df_GLSlocs_daily)
head(df_locplot)

p_loc_resolution <- ggplot(df_locplot, aes(x = Lon, y = Lat, group = ID, col = Resolution)) +
  coord_cartesian(xlim = c(25, 125), ylim = c(-35, 25), expand = F)+
  geom_point(alpha = 0.6, cex = 0.2)+
  scale_color_grey(start = 0.7, end = 0, name = "Temporal\nresolution")+
  scale_y_continuous(n.breaks = 7)+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill="gray80", colour = "gray80")+
  geom_point(data = df_GLSlocs[1,], aes(x = CPLon, y = CPLat), inherit.aes = FALSE, colour = "#D40D12")+
  guides(colour = guide_legend(override.aes = list(cex = 1)))+
  theme_light()+
  theme(panel.grid = element_blank())+labs(x = "Longitude (°E)", y = "Latitude (°N)")

ggsave(plot = p_loc_resolution, filename = here("Figures", "Supplementary", "Loc_resolution.png"),
       width = 18, height = 12, units = "cm")

#----------------------------#
## 4. Kernels: population ####
#----------------------------#

sf_GLSlocs_daily <- df_GLSlocs_daily %>%
  ungroup() %>%
  dplyr::select(Lon, Lat) %>% # add ID here if I can work out how to average?
  mutate(id = 1) %>%
  st_as_sf(., coords = c("Lon", "Lat"), crs = 4326) %>%
  st_transform(crs = 3857)

bb_GLSlocs <- st_bbox(sf_GLSlocs_daily)
x <- seq(as.numeric(bb_GLSlocs["xmin"])-1000,as.numeric(bb_GLSlocs["xmax"])+1000,by=10000) # by = 10km grid
y <- seq(as.numeric(bb_GLSlocs["ymin"])-1000,as.numeric(bb_GLSlocs["ymax"])+1000,by=10000)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE

# convert to sp because adehabitat not updated to work with sf
sp_GLSlocs_daily <- as(sf_GLSlocs_daily, "Spatial")

UD_GLS <- kernelUD(sp_GLSlocs_daily, kern = "bivnorm", grid = xy)

get_kernel <- function(x, percent = 50){
  getverticeshr(x, percent = c(percent)) %>% 
    fortify() %>% 
    mutate(percent = percent)
}

kernel_95 <- get_kernel(UD_GLS, 95)
kernel_75 <- get_kernel(UD_GLS, 75)
kernel_50 <- get_kernel(UD_GLS, 50)
kernel_25 <- get_kernel(UD_GLS, 25)

df_kernel <- bind_rows(lapply(ls(pattern = "kernel_"), get))
head(df_kernel)

write.csv(df_kernel, here("Data", "WorkingDataFrames", "RFB_GLS_kernels.csv"))


sf_kernels <- sf::st_as_sf(df_kernel, coords = c("long", "lat"), crs = 3857) %>% st_transform(4326) 

sf_kernels_poly = st_sf(
  aggregate(
    sf_kernels$geometry,
    list(sf_kernels$piece, sf_kernels$percent),
    function(g){
      st_cast(st_combine(g),"POLYGON")
    }
  )) %>%
  rename(percent = Group.2) %>%
  arrange(desc(percent)) %>%
  mutate(percent_f = factor(percent, levels = unique(percent)))


plot_kernel_all <- ggplot() +
  scale_x_continuous(limits = c(25, 125), expand = c(0,0))+
  scale_y_continuous(limits = c(-35,25), expand = c(0,0))+
  geom_path(data = MPA, aes(x = Long, y = Lat), inherit.aes = FALSE, col = "gray20", lwd = 0.2)+
  geom_sf(data = filter(sf_kernels_poly, !percent == 95), aes(alpha = percent_f), lwd = 0.4, fill = "#ff333c", col = "#ab0007")+
  scale_alpha_discrete("% UD",range = c(0.1, 1))+
  geom_sf(data = sf_world_map, fill="gray80", colour = "gray80")+
  #geom_point(data = df_RFB_range_br, aes(x = X, y = Y), col = "#670004", pch = 4, cex = 0.8)+
  geom_point(data = df_RFB_IO_br, aes(x = Lon, y = Lat), col = "#670004", pch = 4, cex = 1.2)+
  geom_point(data = df_GLSlocs[1,], aes(x = CPLon, y = CPLat), inherit.aes = FALSE, colour = "#670004", cex = 3)+
  theme_light()+
  theme(panel.grid = element_blank())+labs(x = NULL, y = NULL)
  
plot_kernel_all 


#Reds = #https://colordesigner.io/#450003-5C0002-94090D-D40D12-FF1D23



#### Split kernels by month ####

get_kernel <- function(x, percent = 50){
  getverticeshr(x, percent = c(percent)) %>% 
    fortify() %>% 
    mutate(percent = percent)
}

for (m in unique(df_GLSlocs_daily$Month)){
  sf_m <- df_GLSlocs_daily %>%
    ungroup() %>%
    filter(., Month == m) %>%
    dplyr::select(Lon, Lat) %>% # add ID here if I can work out how to average?
    mutate(id = 1) %>%
    st_as_sf(., coords = c("Lon", "Lat"), crs = 4326) %>%
    st_transform(crs = 3857)
  
  bbox_m <- st_bbox(sf_m)
  x <- seq(as.numeric(bbox_m["xmin"])-500000,as.numeric(bbox_m["xmax"])+500000,by=10000) # by = 10km grid
  y <- seq(as.numeric(bbox_m["ymin"])-500000,as.numeric(bbox_m["ymax"])+500000,by=10000)
  xy <- expand.grid(x=x,y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  
  sp_m <- as(sf_m, "Spatial")
  
  UD_m <- kernelUD(sp_m, kern = "bivnorm", grid = xy)
  
  #kernel_m_95 <- get_kernel(UD_m, 95)
  kernel_m_75 <- get_kernel(UD_m, 75)
  kernel_m_50 <- get_kernel(UD_m, 50)
  kernel_m_25 <- get_kernel(UD_m, 25)
  

  df_kernel_m <- bind_rows(lapply(ls(pattern = "kernel_m_"), get))
  df_kernel_m$Month <- m
  
  write.csv(df_kernel_m, here("Data", "WorkingDataFrames", "Monthly kernels", paste0("RFB_GLS_monthlykernels_",m,".csv")), row.names = F)
  
  
}


filepath <- here("Data", "WorkingDataFrames", "Monthly kernels") #create relative filepath using folder names
filepattern <- "*.csv" # data file format (use "*.csv" to import all csv files within filepath folders)

df_UDs_m <- fs::dir_ls(path = filepath, recurse = TRUE, glob =filepattern) %>%
  purrr::map_dfr(read_csv, col_types = cols(.default = "c"))



sf_kernels_m <- sf::st_as_sf(df_UDs_m, coords = c("long", "lat"), crs = 3857) %>% st_transform(4326) 

sf_kernels_m_poly = st_sf(
  aggregate(
    sf_kernels_m$geometry,
    list(sf_kernels_m$piece, sf_kernels_m$percent, sf_kernels_m$Month),
    function(g){
      st_cast(st_combine(g),"POLYGON")
    }
  )) %>%
  rename(percent = Group.2, Month = Group.3) %>%
  arrange(desc(percent)) %>%
  mutate(percent_f = factor(percent, levels = unique(percent)))



plot_kernel_months <- ggplot() +
  scale_x_continuous(limits = c(62, 82), expand = c(0,0))+
  scale_y_continuous(limits = c(-22,10), expand = c(0,0))+
  geom_path(data = MPA, aes(x = Long, y = Lat), inherit.aes = FALSE, col = "gray20", lwd = 0.2)+
  geom_sf(data = filter(sf_kernels_m_poly, !percent == 95), aes(alpha = percent_f), lwd = 0.4, fill = "#ff333c", col = "#ab0007")+
  scale_alpha_discrete("% UD",range = c(0.1, 1))+
  facet_wrap(.~lubridate::month(as.numeric(Month), label = T), ncol = 5)+
  geom_sf(data = sf_world_map, fill="gray80", colour = "gray80")+
  annotate("point", x = 72.4, y = -7.24, colour = "#670004", cex = 1)+
  theme_light()+
  theme(panel.grid = element_blank())+
  labs(x = NULL, y = NULL)


kernelplots <- ggarrange(
  plot_kernel_all, plot_kernel_months, labels = c("a", "b"), ncol = 1, 
  common.legend = T, legend = "right", heights = c(1,1.16)
)

RFBimg <- png::readPNG("/Users/at687/Documents/BIOT/Seabird graphics/white morph red footed booby adult.png", native = T)

t <- ggdraw() +
  draw_plot(kernelplots) +
  draw_image(
    RFBimg, x = 1, y = 1, hjust = 1, vjust = 1, valign = 1,
    width = 0.25
  )

png(here("Figures", "Kernels.png"), width = 18, height = 20, units = "cm", res = 300)
print(t)
dev.off()

# plot_kernel_all / plot_kernel_months +
#   theme(plot.margin = margin(0, 0, 0, 0, "pt")) +
#   plot_layout(guides = "collect")+
#   plot_annotation(tag_levels = "a") & theme(legend.position = "right")

