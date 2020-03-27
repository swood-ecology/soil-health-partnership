#########################
# SHP data manipulation #
# Author: Steve Wood    #
#########################

# Load needed packages
library(tidyverse)    # General manipulation and pipe operators
library(forcats)      # For combining factors
library(readxl)       # For reading in Excel data
library(fastDummies)  # For generating dummy variables

# Load raw data
data.2018 <- read_csv("raw-data/SHP_core_data_with_updated_SOM_8_20_2019.csv")
data.2019 <- read_excel("raw-data/Cornell_2019_9_17_2019.xlsx")
data.redo <- read_csv("raw-data/som.subset.reanalysis.csv")

# Remove nutrient management farms
not.nm.fields <- data.2018[which(data.2018$TrtType!="NM"),"field_name"] %>% unique() %>% as_vector()
data.2018 <- data.2018 %>%
  filter(field_name %in% not.nm.fields)
data.2019 <- data.2019 %>%
  filter(field_name %in% not.nm.fields)
rm(not.nm.fields)

# Load intended crop rotation
rotate <- read_excel("raw-data/rotations.xlsx")

# Load corrected treatment categories
new_treat <- read_excel("raw-data/new_treatments_10_14_2019.xlsx")

# Rename new treatment categories
# and replace treatment variable in 2018 data
new_treat <- new_treat %>%
  mutate(
    treatment_new=recode(treatment_new,
                      `T` = "AMS",
                      `C` = "Control"
                      ),
    zone_name = paste0(field_name,"_",Strip)
    )

# Filter out farm that has different managements over the course of the experiment
data.2018 <- data.2018 %>% filter(field_name != 'SHP2017WI001')
data.2019 <- data.2019 %>% filter(field_name != 'SHP2017WI001')

# Create new 2018 data set with corrected treatment
data.2018.new <-
  left_join(data.2018, new_treat) %>%
  mutate(
    treat_final = case_when(
        Treat != treatment_new & !is.na(treatment_new) ~ treatment_new,
        is.na(treatment_new) ~ Treat,
        is.na(Treat) ~ treatment_new,
        Treat == treatment_new ~ treatment_new
        )
    )
rm(new_treat); rm(data.2018)

# Add to the 2019 data the year the farms started with SHP
data.2019 <- data.2019 %>%
  left_join(
    data.2018.new %>%
      select(field_name,YRStrtSHP) %>%
      unique()
  )

# Generate SHPID_Strip variable to combine 2019 and 2018
data.2019 <- data.2019 %>%
  mutate(
    SHPID_Strip = paste0(field_name, "_", Strip)
  )
names(data.2019)[7] <- "SOMnew"         # Rename OM variable to match 2018

data.2018.new <- data.2018.new %>%
  mutate(
    SHPID_Strip = paste0(field_name, "_", Strip)
  ) %>%
  # Remove unnecessary variables
  select(
    -enterprise_name,-Multi_YR_all_data,-Multi_YR_YLDonly,-Concat,-ISAYield,
    -`_`,-ISAandFinalYLDCorrect,-Yield,-raster_source_data_type,-zone_name,
    -treatment_new,-Treat,-CoverCropEST,-CCSeedMethod,-FieldTile,-`Trial Type`
  ) 
names(data.2018.new)[21] <- "Respiration"   # Fix typo in 'Respiration' name

# Separate out data that match 2019 from the covariates that don't
data.2018.sub <- data.2018.new %>%
  select(
    one_of(
      names(data.2019)
      )
  )

# Merge 2019 and 2018 data
data <- full_join(data.2018.sub, data.2019)

# Create data set of covariates
## Create one that is time-invariant and one that is time-variant
data.2018.covars <- data.2018.new %>%
  select(
    -one_of(
      names(data.2019)
    ),
    SHPID_Strip,TrtType,
    -SHPID_YR,-SHPID_YR_Strip,-crop,-TrtDetails,-zone_area,-YrsofTrt,-Tillage,
    -Manure
  )
data.2018.covars.t <- data.2018.new %>%
  select(
    -one_of(
      names(data.2019)
    ),
    SHPID_Strip,year,crop,YrsofTrt,Manure,Tillage,
    -SHPID_YR,-SHPID_YR_Strip,-State,-zone_area,-treat_final,-TrtType
  )

# Merge 2018 and 2019 data with time-invariant covariates
data <- data %>%
  left_join(data.2018.covars %>% 
              unique()
            ) %>%
  mutate(SHPID_Strip_YR = paste0(SHPID_Strip,'_',year))

# Generate SHPID_Strip_YR id of time-variant
data.2018.covars.t <- data.2018.covars.t %>%
  mutate(SHPID_Strip_YR = paste0(SHPID_Strip,'_',year))

# Add time-variant covariates to data
data <- data %>% 
  full_join(data.2018.covars.t)

# Calculate new variable, with YrsofTrt for 2019 data
data <- data %>% 
  arrange(SHPID_Strip, year) %>%
  group_by(SHPID_Strip) %>%
  mutate(
    YrTrt = case_when(
      !is.na(YrsofTrt) ~ YrsofTrt,
      is.na(YrsofTrt) ~ lag(YrsofTrt) + (year-lag(year))
    )
  )
rm(data.2018.covars); rm(data.2018.sub); rm(data.2018.covars.t)

# Generate new variables
data <- data %>%
  mutate(
    anyManure = ifelse(Manure == "0" | Manure == "N", "No", "Yes"),
    SHPID_Strip_Crop = paste0(SHPID_Strip, "_", crop),
    SHPID_Crop = paste0(field_name, "_", crop)
  ) %>%
  select(-Manure)

# Merge categories that were only different based on capitalization
data$SoilTextureClass <- data$SoilTextureClass %>%
  forcats::fct_collapse(
    loam = c("loam", "Loam"),
    silty_clay = c("silty clay", "Silty Clay")
  )
data$TrtType <- data$TrtType %>%
  forcats::fct_collapse(
    Tillage = c("TILL", "Tillage")
  )
data$crop <- data$crop %>%
  forcats::fct_collapse(
    `CS` = c("Corn Silage","Silage-actual (Corn NDVI)"),
    `SB` = c("Soybean"),
    `SC` = c("Seed Corn","Seet Corn"),
    `W` = c("Winter Wheat"),
    `CG` = c("Corn","Corn/Wheat")
  )
data$crop_cat <- data$crop %>%
  forcats::fct_collapse(
    `Corn` = c("CS","SC","CG"),
    `Soy` = c("SB"),
    `Wheat` = c("W")
  )

# Define variable that is crop type of previous year
data <- data %>% 
  arrange(SHPID_Strip, year) %>%
  group_by(SHPID_Strip) %>%
  mutate(
    lagCrop = lag(crop),
    lagCropCat = lag(crop_cat)
  )

# Replace YrTrt with 0 for control plots
data <- data %>%
  mutate(
    YrTrtFINAL = case_when(
      treat_final == "Control" ~ 0,
      treat_final == "AMS" ~ YrTrt
    )
  )

# Does the sample have a baseline?
data <- data %>%
  group_by(field_name) %>%
  mutate(
    hasBaseline = ifelse(
      (str_detect(field_name,"2014") && min(year) <= 2015), FALSE, TRUE
    )
  ) 

# Filter out observations
## 1. has SOM data
## 2. has a lagged crop
## 3. are only AMS or control
## 4. have at least one year of measurement
data.sub <- data %>%
  filter(!is.na(SOMnew),
         !is.na(lagCrop),
         (treat_final == "AMS" | treat_final == "Control")) %>%
  group_by(SHPID_Strip) %>%
  filter(first(year) != last(year))

# Generate dummy variables for categories
data.sub <- dummy_cols(data.sub,
           select_columns=c("treat_final","lagCrop","lagCropCat","year"))

# Which rotation has Soy?
rotate <- rotate %>%
  mutate(
    hasSoy = case_when(
      str_detect(rotation_from_jack,"SB") ~ TRUE,
      TRUE ~ FALSE
    )  
  )

# Add crop rotation system
data.sub <- left_join(data.sub, 
               rotate %>%
                 select(field_name, hasSoy)
               )
rm(rotate)

# Add in re-do data
data.sub <- data.sub %>%
  left_join(
    data.redo %>%
      mutate(
        SHPID_Strip_YR = paste0(SHPID, "_", Strip, "_", Year)
      )
  ) %>%
  select(`SoilTextureClass`:`OM%`) %>%
  rename(som.redo = `OM%`) %>%
  mutate(som.final = ifelse(is.na(som.redo) == FALSE, som.redo, SOMnew)) %>%
  select(-som.redo)

rm(data.redo)

# Subset SOM data for last year of each farm
last_yr <- aggregate(year ~ SHPID_Strip_Crop, FUN="max",data=data.sub)
names(last_yr) <- c("SHPID_Strip_Crop","year_max")

data.last_yr <- data.sub %>%
  full_join(last_yr, by="SHPID_Strip_Crop") %>%
  group_by("SHPID_Strip_Crop") %>%
  filter(year == year_max)
rm(last_yr)

# Data of the difference between first and last year
data.diff <- data.sub %>%
  group_by(SHPID_Strip) %>%
  mutate(
    ac.diff = last(ActiveCarbon) - first(ActiveCarbon),
    as.diff = last(AggStab) - first(AggStab),
    whc.diff = last(WHC) - first(WHC),
    som.diff = last(som.final) - first(som.final),
    resp.diff = last(Respiration) - first(Respiration),
    pro.diff = last(ACESoilProtein) - first(ACESoilProtein)
  )

save.image("~/Box Sync/Work/Code/shp/shp-data.RData")
