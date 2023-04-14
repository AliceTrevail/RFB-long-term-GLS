# Import and collate all location files
# These have already been processed in SGAT

# code following code workshop

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

## Data files should all be stored in a specific folder (ideally within TestData)
## ID (for individual) should be in data file name
## Data must contain timestamp and at least one other sensor column
## Metadata file should be in parent directory of data files


#---------------------------#
##1. Read in data files  ####
#---------------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

## set filepath for folder containing raw data files
## NB, this will try to open all files matching the file pattern within this folder
## Therefore, it is best if this folder containes the raw data files, only

filepath <- here("Data", "Raw GLS data") #create relative filepath using folder names
filepattern <- "*.csv" # data file format (use "*.csv" to import all csv files within filepath folders)

#Think of way of view filename/ID???

# adjust these numbers to take out ID number from file name

IDstart <- 7 #start position of the ID in the filename 
IDend <- 13 #end position of the ID in the filename

# define date format(s) used (for passing to lubridate)
# "d"=day as decimal, "m"=month as decimal, "y"=year w/o century, "Y"=year w/ century

date_formats <- c("dmY", "Ymd") #specify date formats (e.g. "dmY" works for 01-12-2022 and 01/12/2022)
datetime_formats <- c("dmY HMS", "Ymd HMS") #specify date & time format 

# define time zone for tracking data 

trackingdatatimezone <- "GMT"

#------------------#
## USER INPUT END ##
#------------------#

# Read in and merge tracking data files directly from github repository
# If your data is already combined into one file, skip this step!

df_combined <- fs::dir_ls(path = filepath, recurse = TRUE, type="file", glob =filepattern) %>%
  purrr::set_names(nm = basename(.)) %>% #removing path prefix (makes filename more manageable)
  purrr::map_dfr(read_csv, .id="filename", col_types = cols(.default = "c")) %>% #read list of files (currently chr for all cols)
  mutate(ID = str_sub(filename, start=IDstart, end=IDend),.after=filename) #substring ID from the filename (start to end of substring)
colnames(df_combined)


#--------------------#
## USER INPUT START ##
#--------------------#

## Select necessary comments & coerce column names
## Compulsory columns = ID, Date & Time (or single DateTime column?)
## Optional columns depending on sensor type, e.g. Lat, Lon, error

df_slim <- data.frame(ID = df_combined$ID,
                      DateTime = df_combined$Time,
                      Lon = df_combined$Lon.mean,
                      Lon_sd = df_combined$Lon.sd,
                      Lon_50 = df_combined$`Lon.50%`,
                      Lon_2.5 = df_combined$`Lon.2.5%`,
                      Lon_97.5 = df_combined$`Lon.97.5%`,
                      Lat = df_combined$Lat.mean,
                      Lat_sd = df_combined$Lat.sd,
                      Lat_50 = df_combined$`Lat.50%`,
                      Lat_2.5 = df_combined$`Lat.2.5%`,
                      Lat_97.5 = df_combined$`Lat.97.5%`)


#------------------#
## USER INPUT END ##
#------------------#


str(df_slim);head(df_slim)

# # If your date and time are in separate columns:
# # Parse date and create datetime column
# # Create datetime (append time to date object using parse_date_time)
# 
# message("If you see any 'failed to parse' warnings below a date or time has not formatted (we will deal with this later)")
# 
# df_slim$Date <- lubridate::parse_date_time(df_slim$Date, orders=date_formats) #use lubridate to parse Date using date_formats
# df_slim$DateTime <- lubridate::parse_date_time(paste(df_slim$Date, df_slim$Time),orders=datetime_formats, tz=trackingdatatimezone) #can add timezone here with "tz="

# If you already have one datetime column:
df_slim$DateTime <- lubridate::parse_date_time(df_slim$DateTime,orders=datetime_formats, tz=trackingdatatimezone)

# Order for our finished raw dataframe

df_raw <- df_slim %>% arrange(ID, DateTime) %>%
  drop_na(DateTime) #remove NA's in datetime column
head(df_raw)

# Remove intermediate files/objects
rm(list=ls()[!ls() %in% c("df_raw","date_formats","datetime_formats","trackingdatatimezone")]) #could just specify objects to keep? (no errors)



#----------------------------#
##2. Merge with metadata  ####
#----------------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

# set file path to metadata
filepath_meta <- here("Data","GLS_Metadata.csv")

# Set date and time formats of metadata file here
metadate_formats <- c("dmY") #specify date format used in metadata
metadatetime_formats <- c("Ymd HMS", "dmY HMS") #specify date & time format (no need to change this)
metadatatimezone <- "Indian/Chagos" #specify timezone used for metadata

#------------------#
## USER INPUT END ##
#------------------#

# Read in metadata file

df_metadata <- read_csv(filepath_meta) #read in the metadata file
names(df_metadata)

#--------------------#
## USER INPUT START ##
#--------------------#

# Select necessary comments & coerce column names
# Compulsory columns: ID, deployment date & deployment time
# Optional columns depending on sensor type: e.g. colony, sex, age, central place (CP) Lat, CP Lon
# Add or delete columns here as appropriate

df_metadataslim <- data.frame(ID = df_metadata$BirdID,
                              DeployDate_local = df_metadata$Deploy_Date, # note in col name that these are recorded in local time
                              DeployTime_local = df_metadata$Deploy_Time, # note in col name that these are recorded in local time
                              RetrieveDate_local = df_metadata$Retrieve_Date, # note in col name that these are recorded in local time
                              RetrieveTime_local = df_metadata$Retrieve_Time, # note in col name that these are recorded in local time
                              Species = "RFB", 
                              Population = "DG",
                              Age = "Adult",
                              Sex = df_metadata$Sex,
                              Deploy_BrStage = df_metadata$Deploy_BrStage,
                              Retrieve_BrStage = df_metadata$Retrieve_BrStage)


## If central place for each individual is not known,
## add population level central places here using the following code:

# create a dataframe of population CPs.
df_PopCPs <- tribble(
  ~Population,  ~CPLat,    ~CPLon,
  "DG",         -7.24,     72.43,
  #"NI",         -5.68,     71.24
)

# merge df_metadataslim and df_PopCPs
# The column "Population" must be present in both dataframes
# This will overwrite "CPLat" and "CPLon" if present in df_metadataslim
df_metadataslim  <- df_metadataslim %>%
  #select(matches(setdiff(names(df_metadataslim), c("CPLat", "CPLon")))) %>%
  left_join(., df_PopCPs, by = "Population")

# count breeding stages 
n_brstage_deploy <- df_metadataslim %>%
  group_by(Deploy_BrStage) %>%
  summarise(count = n())

n_brstage_retrieve <- df_metadataslim %>%
  group_by(Retrieve_BrStage) %>%
  summarise(count = n())

#------------------#
## USER INPUT END ##
#------------------#

# Parse dates and times 
# if NAs in deployment/retrieval date times these will throw up warnings, these are safe to iignore if you know there are NAs in these columns
df_metadataslim <- df_metadataslim %>%
  mutate(Deploydatetime = parse_date_time(paste(DeployDate_local, DeployTime_local),#create deploy datetime
                                          order=metadatetime_formats, 
                                          tz=metadatatimezone),
         Retrievedatetime = parse_date_time(paste(RetrieveDate_local, RetrieveTime_local), #create retrieve datetime
                                            order=metadatetime_formats,
                                            tz=metadatatimezone)) %>%
  dplyr::select(-any_of(c("DeployDate_local","DeployTime_local", "RetrieveDate_local", "RetrieveTime_local"))) %>%
  mutate(across(contains('datetime'), #return datetime as it would appear in a different tz
                ~with_tz(., tzone=trackingdatatimezone)))

## create dataframe of temporal extents in data to use in absence of deploy/retrieve times
## also useful for e.g., data checks/writing up methods
df_temporalextents <- df_raw %>%
  group_by(ID) %>%
  summarise(min_datetime = min(DateTime),
            max_datetime = max(DateTime))

## fill in NAs in deploy/retrieve times with extent of tracking data
df_metadataslim <- df_metadataslim %>%
  left_join(., df_temporalextents, by = "ID") %>%
  mutate(Deploydatetime = case_when(!is.na(Deploydatetime) ~ Deploydatetime,
                                    is.na(Deploydatetime) ~ min_datetime),
         Retrievedatetime = case_when(!is.na(Retrievedatetime) ~ Retrievedatetime,
                                      is.na(Retrievedatetime) ~ max_datetime)) %>%
  dplyr::select(-c(min_datetime, max_datetime))


## Merge metadata with raw data using ID column
df_metamerged <- df_raw %>%
  left_join(., df_metadataslim, by="ID") 
head(df_metamerged)



### Remove intermediate files/objects
#rm(list=ls()[!ls() %in% c("df_metamerged")]) #specify objects to keep





#--------------------#
##3. Cleaning  ####
#--------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

# Define your own no/empty/erroneous data values
# e.g. bad values in Lat & Lon columns
# This filter would be a good place to change code below for argos filter

No_data_vals <- c(0, -999)

# Define a vector of columns which can't have NAs

na_cols <- c("Lat", "Lon", "DateTime", "ID") #column to check for na's

# Define a period to filter after tag deployment
# units = hours - can change within function
# has to be an integer

cutoff <- as.period(0, unit="minutes") #IMPROVE THIS COMMENT LIAM!!!


# define number days to exclude around equinox
equinox <- 30

# Spring equinox = 20 March = 79th Julian day
# Autumn equinox = 22 Sept = 265th Julian day

#------------------#
## USER INPUT END ##
#------------------#

# Pipe to filer data:
# Remove NAs
# Remove user-defined no data values
# Remove duplicates
# Remove un deployed locations
# Remove locations within temporal cut off following deployment

df_clean <- df_metamerged %>%
  drop_na(all_of(na_cols)) %>% 
  filter(!Lat %in% No_data_vals & !Lon %in% No_data_vals) %>% # specify columns for not data values/and for satellite counts
  distinct(DateTime, ID, .keep_all = TRUE) %>% # this might be a problem for ACC data where we don't have milliseconds
  dplyr::filter((Deploydatetime+cutoff) < DateTime & DateTime < Retrievedatetime) %>%
  mutate(jday = as.numeric(format(DateTime, "%j"))) %>%
  dplyr::filter(!jday %in% c(c((79  - equinox):(79  + equinox)),c((265 - equinox)  :(265 + equinox)))) %>%
  dplyr::select(-jday) %>%
  mutate(Month = month(DateTime)) 
head(df_clean)

unique(df_clean$Month)

# Remove intermediate files/objects
#rm(list=ls()[!ls() %in% c("df_clean")]) #specify objects to keep


#-------------------#
##4. Processing  ####
#-------------------#

## Some useful temporal and spatial calculations on the data

#--------------------#
## USER INPUT START ##
#--------------------#

## Specify co-ordinate projection systems for the tracking data and meta data
## Default here is lon/lat for both tracking data & metadata, for which the EPSG code is 4326. 
## Look online for alternative EPSG codes, e.g., https://inbo.github.io/tutorials/tutorials/spatial_crs_coding/

tracking_crs <- 4326 # Only change if data are in a different coordinate system
meta_crs <- 4326 # Only change if data are in a different coordinate system

#------------------#
## USER INPUT END ##
#------------------#

## Transform coordinates of data and perform spatial calculations
## It is good practice to run all spatial analyses in a coordinate system with units in metres

## As an example, we will use Spherical Mercator projection — aka 'WGS' (crs = 3857)
## Consider the location and scale of your data (e.g., equatorial/polar/local scale/global scale) when choosing a projection system
## Other options include (but are not limited to) UTM, Lambert azimuthal equal-area (LAEA)

df_diagnostic <-  df_clean %>%
  ungroup() %>% #need to ungroup to extract geometry of the whole dataset
  mutate(geometry_GPS = st_transform( # assign geometry and transform to WGS for distance calculations
      st_as_sf(., coords=c("Lon","Lat"), crs=tracking_crs), crs = 3857)$geometry,
    geometry_first = st_transform(
      st_as_sf(slice(., 1), coords=c("Lon","Lat"), crs=tracking_crs), crs = 3857)$geometry,
    geometry_CP = st_transform(
      st_as_sf(., coords=c("CPLon","CPLat"), crs=meta_crs), crs = 3857)$geometry) %>%
  group_by(ID) %>% #back to grouping by ID for calculations per individual
  mutate(dist = st_distance(geometry_GPS, lag(geometry_GPS), by_element = T), # distance travelled from previous fix
         difftime = difftime(DateTime, lag(DateTime), units="secs"),          # time passed since previous fix
         netdisp = st_distance(geometry_GPS, geometry_first, by_element = T), # calculate distance between first location and current location
         speed = as.numeric(dist)/as.numeric(difftime),                       # calculate speed (distance/time)
         dLon = as.numeric(Lon)-lag(as.numeric(Lon)), #difference in longitude, relative to previous location
         dLat = as.numeric(Lat)-lag(as.numeric(Lat)), #difference in longitude, relative to previous location
         turnangle = atan2(dLon, dLat)*180/pi + (dLon < 0)*360, #angle (in degrees) from previous to current location using formula theta = atan(y/x), where y = change along y axis & x = change along x axis
         CPdist = st_distance(geometry_GPS, geometry_CP, by_element = T), #calculate distance between central place and current location
         dLon_CP = as.numeric(Lon)-CPLon, #difference in longitude between current location and central place
         dLat_CP = as.numeric(Lat)-CPLat, #difference in longitude between current location and central place
         CPbearing = atan2(dLon_CP, dLat_CP)*180/pi + (dLon_CP < 0)*360) %>%
  ungroup() %>% dplyr::select(-c(geometry_GPS, geometry_CP, dLon, dLat, dLon_CP, dLat_CP)) # ungroup and remove geometries



#---------------------------#
##Save df_diagnostic #### 
#---------------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

# It is good practice to include species in the data frame
species_code <- "RFB"

filepath_dfout <- here("Data","WorkingDataFrames") #specify where df's will be saved

# define name for new saved file
# here, we use the species code and "_diagnostic"
filename_dfout <- paste0(species_code, "_GLSlocs_diagnostic")

## If not added from the metadata, use this code to add a species column and any other columns here relevant to your data
# df_diagnostic$Species <- species_code


#------------------#
## USER INPUT END ##
#------------------#

# save dataframe
write_csv(df_diagnostic, file = here(filepath_dfout, paste0(filename_dfout,".csv")))



#-----------------#
##6. Filtering #### 
#-----------------#

## This filtering stage is designed to remove outliers in the data
## You can use outputs from the shiny app to inform these choices

## Accessing Shiny App

## OPTION 1:
## Access the shiny app online at the following link: https://lukeozsanlav.shinyapps.io/exmove_explorer/

## OPTION 2:
## Alternatively run the app from your local R session with the following code
## This willrequire some additonal packages to be installed on the start up of the app but this will happen automatically
if (!require("shiny")) install.packages("shiny")
library(shiny)
runGitHub("ExMoveApp", username = "LukeOzsanlav",
          ref = "master", subdir = "app")

## APP USAGE:
## Upload your csv version of df_diagnostic to the app by clicking the `Upload data` button in the top left
## At the bottom of each app page are printed code chunks that can be copied into subsequent user input sections
## Those code chunks contain the user input values you manually select in the app

## If you don't need to filter for outliers, skip this step and keep using df_diagnostic


#--------------------#
## USER INPUT START ##
#--------------------#

## Define a period to filter after tag deployment
## All points before the cutoff will be removed
## For example, to remove potentially unnatural behaviour following the tagging event
## This has to be an integer value
## change the units within the function (e.g., "min"/"mins"/"hours"/"year"...)
filter_cutoff <- as.period(0, unit="minutes") 

## Define speed filter in m/s
## Any points with faster speeds will be removed
filter_speed <- 20

## Define net displacement filter and specify units
## Any points further away from the first tracking point will be removed
## If you want to retain points no matter the net displacement value then run `filter_netdisp_dist <- max(df_diagnostic$netdisp)` and `filter_netdist_units <- "m`
filter_netdisp_dist <- 3000
filter_netdist_units <- "km" # e.g., "m", "km"


#------------------#
## USER INPUT END ##
#------------------#

# create net displacement filter using distance and units
filter_netdisp <- units::as_units(filter_netdisp_dist, filter_netdist_units)

# filter df_diagnostic
df_filtered <- df_diagnostic %>%
  filter(Deploydatetime + filter_cutoff < DateTime, # keep times after cutoff
         speed < filter_speed, # keep speeds slower than speed filter
         netdisp <= filter_netdisp) # keep distances less than net displacement filter
head(df_filtered)


## Remove intermediate files/objects if necessary to speed up processing
rm(list=ls()[!ls() %in% c("df_filtered", "species_code")]) #specify objects to keep


#-----------------------------------------#
##8. Save df_filtered  #### 
#-----------------------------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

## Define file path for df_filtered
filepath_filtered_out <- here("Data","WorkingDataFrames")

## Create species folder if it doesn't exist
dir.create(filepath_filtered_out)

## Define file names for saved files
## here, we use the species code and "_summary_" followed by ind (individual level) or pop (population level)
## Change if you want to use a different naming system
filename_filtered_out <- paste0(species_code, "GLSlocs_filtered")


#------------------#
## USER INPUT END ##
#------------------#

## save dataframes as .csv files
write_csv(df_filtered, file = here(filepath_filtered_out, paste0(filename_filtered_out,".csv")))


## Remove intermediate files/objects if necessary to speed up processing
#rm(list=ls()[!ls() %in% c("df_filtered", "df_summary_ind", "df_summary_pop", "species_code")]) #specify objects to keep



#------------------------------------------------------#
##11. Reformat data for upload to public databases  ####
#------------------------------------------------------#

#-----------------------------#
## SEABIRD TRACKING DATABASE ##
#-----------------------------#

## Secondly, reformat our data for upload to the Seabird Tracking Database (STDB)
## The STDB (http://seabirdtracking.org) is the largest collection of seabird tracking data
## and is hosted by BirdLife International

## The STDB requests data to be down-sampled to 2 min resolution or lower, to reduce file storage demands
## If relevant, use the optional processing code to do this for your data

## The STDB requires specific columns
## At the time of writing, there isn't a process for uploading multiple grouping factors in one data file (e.g., species/population)
## Each level of the grouping factor therefore needs to be saved in it's own file
## Metadata (species, population location, etc., are added in the web portal)
## Different deployment periods can presumably be uploaded together, as they are distinguishable by date

## The STDB provide this template for data:
# Field name    Type            Data format       Options
# ------------------------------------------------------------- #
# BirdId	      user defined	  String														
# Sex	          limited choice	String	          female	male 	unknown											
# Age	          limited choice	String	          adult	immature	juvenile	fledgling	unknown									
# Breed Stage	  limited choice	String	          pre-egg	incubation	brood-guard	post-guard	chick-rearing	creche	breeding	fail (breeding season)	migration	winter	sabbatical	pre-moult	non-breeding	unknown
# TrackId	      user defined	  String														
# DateGMT     	user defined	  dd/mm/yyyy														
# TimeGMT	      user defined	  hh:mm:ss														
# Latitude	    user defined	  Decimal Degrees														
# Longitude	    user defined	  Decimal Degrees														
# Equinox	      limited choice	Yes/No	          yes	no												
# ArgosQuality	user defined	  String	          3	2	1	0	A	B	Z	


## Below, we provide an example for GPS tracking data of a central place forager
## We have read in df_trips from 'Central place trips.R' as df_final
## NB: this will only work on data where trips have been defined

## remind ourselves of the data structure
df_final <- df_filtered
df_diagnostic
names(df_final)

#--------------------#
## USER INPUT START ##
#--------------------#

## time zone of data
tz_data <- "GMT"

## Levels of grouping factors (each will be saved as separate file)
grouping_factors <- c("Species") 

## File path for saving data
filepath_dfout <- here("Data","DataBaseUploadFiles")

## Create species folder within "DataBaseUploadFiles" if it doesn't exist
dir.create(filepath_dfout, recursive = TRUE)


## Re-format data for STDB, but retain grouping factor columns
## Requires user input to rename appropriate columns 
df_STDB <- df_final %>%
  group_by(across(grouping_factors)) %>%
  # format date and time columns
  mutate(DateTimeGMT = with_tz(ymd_hms(DateTime, tz = tz_data), "GMT"), # make column of DateTime in GMT
         DateGMT = as_date(DateTimeGMT), # use lubridate to extract date, only
         TimeGMT = hms::as_hms(DateTimeGMT)) %>% #use hms from within tidyverse to extract time, only
  # rename columns as necessary (e.g. remove "TrackID = TripID," if trips have not been defined)
  rename(BirdID = ID,
         #BreedStage = BreedingStage,
         #TrackID = TripID,
         Latitude = Lat,
         Longitude = Lon) %>%
  ungroup()%>%
  # select columns specified in template (this will automatically add grouping factors, too)
  dplyr::select(any_of(c(grouping_factors, "Species", "BirdID", "Sex", "Age", "BreedStage", "TrackID", "DateGMT", "TimeGMT", "Latitude", "Longitude")))



## save each group as unique file
## requires user input to specify each level of grouping factor to keep in column name
## .y$Species returns the species of the current group
## 
df_STDB %>%
  group_by(across(grouping_factors)) %>%
  group_walk(~ write_csv(.x, 
                         here(filepath_dfout, 
                              paste0(.y$Species, "_LongTermGLS", "_STDB.csv")))) ## include ", "_", .y$..." for every level of grouping factor


#-----------------#
##USER INPUT END##
#-----------------#