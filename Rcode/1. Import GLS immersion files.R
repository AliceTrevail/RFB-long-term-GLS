# Import and collate all immersion files

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
filepattern <- "*.deg" # data file format (use "*.csv" to import all csv files within filepath folders)

## number of lines at top of file to skip (e.g., if importing a text file with additional info at top)
skiplines <- 19

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
  purrr::map_dfr(read_table, .id="filename", col_types = cols(.default = "c"), skip = skiplines) %>% #read list of files (currently chr for all cols)
  mutate(ID = str_sub(filename, start=IDstart, end=IDend),.after=filename) #substring ID from the filename (start to end of substring)
colnames(df_combined)


#--------------------#
## USER INPUT START ##
#--------------------#

## Select necessary comments & coerce column names
## Compulsory columns = ID, Date & Time (or single DateTime column?)
## Optional columns depending on sensor type, e.g. Lat, Lon, error


df_slim <- data.frame(ID = df_combined$ID,
                      Date = df_combined$`DD/MM/YYYY`,
                      Time = df_combined$`HH:MM:SS`,
                      Immersion = df_combined$`wets0-20`)



#------------------#
## USER INPUT END ##
#------------------#


str(df_slim);head(df_slim)

# # If your date and time are in separate columns:
# # Parse date and create datetime column
# # Create datetime (append time to date object using parse_date_time)
# 
message("If you see any 'failed to parse' warnings below a date or time has not formatted (we will deal with this later)")

df_slim$Date <- lubridate::parse_date_time(df_slim$Date, orders=date_formats) #use lubridate to parse Date using date_formats
df_slim$DateTime <- lubridate::parse_date_time(paste(df_slim$Date, df_slim$Time),orders=datetime_formats, tz=trackingdatatimezone) #can add timezone here with "tz="

# If you already have one datetime column:
# df_slim$DateTime <- lubridate::parse_date_time(df_slim$DateTime,orders=datetime_formats, tz=trackingdatatimezone)

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
metadatetime_formats <- c("Ymd HMS") #specify date & time format (no need to change this)
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


# ## If central place for each individual is not known,
# ## add population level central places here using the following code:
# 
# # create a dataframe of population CPs.
# df_PopCPs <- tribble(
#   ~Population,  ~CPLat,    ~CPLon,
#   "DG",         -7.24,     72.43,
#   #"NI",         -5.68,     71.24
# )
# 
# # merge df_metadataslim and df_PopCPs
# # The column "Population" must be present in both dataframes
# # This will overwrite "CPLat" and "CPLon" if present in df_metadataslim
# df_metadataslim  %<>%
#   select(matches(setdiff(names(df_metadataslim), c("CPLat", "CPLon")))) %>%
#   left_join(., df_PopCPs, by = "Population")
# 

#------------------#
## USER INPUT END ##
#------------------#

# Format all dates and times, combine them and specifies timezone (tz) - CHECK tz formatting!

df_metadataslim %<>%
  mutate(across(contains('Date'), ~parse_date_time(., orders=metadate_formats))) %>% #format all Date column with lubridate
  mutate(Deploydatetime = parse_date_time(paste(DeployDate_local,DeployTime_local),order=metadatetime_formats, tz=metadatatimezone), #create deploy datetime
         Retrievedatetime = parse_date_time(paste(RetrieveDate_local,RetrieveTime_local), order=metadatetime_formats, tz=metadatatimezone)) %>% #create retrieve datetime
  select(-c(DeployDate_local,DeployTime_local,RetrieveDate_local,RetrieveTime_local)) %>%
  mutate(across(contains('datetime'), ~with_tz(., tzone=trackingdatatimezone))) #return datetime as it would appear in a different tz

# Come back to redeployments

# Merge metadata with raw data using ID column

df_metamerged <- df_raw %>%
  left_join(., df_metadataslim, by="ID") #add metadata to summarised data (nest lat/long)



### Remove intermediate files/objects
rm(list=ls()[!ls() %in% c("df_metamerged")]) #specify objects to keep



#--------------------#
##3. Filtering 1  ####
#--------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

# Define your own no/empty/erroneous data values
# e.g. bad values in Lat & Lon columns
# This filter would be a good place to change code below for argos filter

No_data_vals <- c(0, -999)

# Define a vector of columns which can't have NAs

na_cols <- c("DateTime", "ID", "Immersion") #column to check for na's

# Define a period to filter after tag deployment
# units = hours - can change within function
# has to be an integer

cutoff <- as.period(0, unit="minutes") #IMPROVE THIS COMMENT LIAM!!!


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
  #filter(!Lat %in% No_data_vals & !Lon %in% No_data_vals) %>% # specify columns for not data values/and for satellite counts
  distinct(DateTime, ID, .keep_all = TRUE) %>% # this might be a problem for ACC data where we don't have milliseconds
  filter((Deploydatetime+cutoff) < DateTime & DateTime < Retrievedatetime)
head(df_clean)

# Remove intermediate files/objects

rm(list=ls()[!ls() %in% c("df_clean")]) #specify objects to keep


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
filename_dfout <- paste0(species_code, "_GLSimmersion_clean")

## If not added from the metadata, use this code to add a species column and any other columns here relevant to your data
# df_diagnostic$Species <- species_code


#------------------#
## USER INPUT END ##
#------------------#

# save dataframe
write_csv(df_clean, file = here(filepath_dfout, paste0(filename_dfout,".csv")))

