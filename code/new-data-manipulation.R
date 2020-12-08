##########################
# SHP data manipulation  #
# Updated with 2020 data #
# Author: Steve Wood     #
##########################

# Load needed packages
library(tidyverse)    # General manipulation and pipe operators
library(readxl)       # For reading in Excel data

setwd("/Users/stephen.wood/Box Sync/Work/Code/shp/")

# Read data
soil <- read_excel("raw-data/data-sep2020/Cornell_20200915.xlsx")
treatment <- read_excel("raw-data/data-sep2020/Strips_20200915.xlsx")
cashcrop <- read_excel("raw-data/data-sep2020/CashCrop20200915.xlsx")
tillagetype <- read_excel("raw-data/data-sep2020/Tillage20200915.xlsx")
treatmenttiming <- read_excel("raw-data/data-sep2020/TreatmentTiming_20200915.xlsx")
rotate <- read_excel("raw-data/rotations.xlsx")


# Manipulate data
names(treatmenttiming)[1] <- "IDtoUse"
names(treatment)[3] <- "IDtoUse"
names(cashcrop)[1] <- "IDtoUse"
names(tillagetype)[1] <- "IDtoUse"

## Modify ID to be able to merge
soil$IDtoUse <- paste0("SH",soil$shp_id)
stringi::stri_sub(soil$IDtoUse, 4, 4) <- '201'
stringi::stri_sub(soil$IDtoUse, 10, 10) <- ''


# Merge data
soilfiltered <- full_join(treatmenttiming, soil %>%
                    select(-ID1:-sample_number), by = "IDtoUse") %>%
  filter(is.na(`Community College`)==TRUE) %>%
  filter(is.na(`Exclude from analysis?`)==TRUE) %>%
  select(-5:-7) %>%
  mutate('idyear' = paste0(IDtoUse,smpl_yr),
         'idstrp' = paste0(IDtoUse,strip_no),
         'idyearstrp' = paste0(IDtoUse,smpl_yr,strip_no))

temp <- full_join(soilfiltered, 
                   cashcrop %>%
                     select(1:3,19,20) %>%
                     mutate('idyear' = paste0(IDtoUse,Year)),
                   by = "idyear")
names(temp)[1] <- "IDtoUse"
temp <- left_join(temp, treatment %>% 
              select(-Shape_Length,-Shape_Area,-`Field ID`,-Comments,-Barcode) %>%
                mutate('idstrp' = paste0(IDtoUse,`Strip No`)), 
            by = "idstrp") 

final <- left_join(temp, tillagetype %>%
                     select(1:3) %>%
                     mutate('idyear' = paste0(IDtoUse,Year)),
            by="idyear")

rm(temp); rm(soilfiltered)
  
final <- final %>% 
  select(-pred_water_capacity:-subsurface_hardness_rating,-root_pathogen_pressure,-root_pathogen_pressure_rating,
         -comments,-shp_id,-barcode,-error,-IDtoUse.y,-IDtoUse.y.y,-IDtoUse,-Year.y)

names(final)[1] <- "IDtoUse"
names(final)[50] <- "Year"


# Generate new variables
## Calculate years involved
final$yrsTrt <- (as.numeric(final$smpl_yr) - as.numeric(final$`Crop Year Cover Crop Treatment First Implemented`)) + 1
## Filter out years without some data
final <- final %>% filter(is.na(yrsTrt)==FALSE)

## Assign all baseline years to 0, which is the baseline
final <- final %>%
  mutate(yrsTrt = ifelse(yrsTrt < 0, 0, yrsTrt))

## Combine SOM data
final <- final %>%
  mutate(final_OM = ifelse(OM_rerun!='<Null>', OM_rerun, organic_matter))

## Define if crop rotation has soybean
rotate <- rotate %>%
  mutate(
    hasSoy = case_when(
      str_detect(rotation_from_jack,"SB") ~ TRUE,
      TRUE ~ FALSE
    )  
  )
names(rotate)[1] <- "IDtoUse"
## Add crop rotation system
final <- left_join(final, 
                      rotate %>%
                        select(IDtoUse, hasSoy)
)
rm(rotate)

## Dummy for tile drainage
final <- final %>%
  mutate(tiled = ifelse(`How is this field tiled?` == 'Not tiled', 0, 1))

## Generate state variable
final$State <- stringi::stri_sub(final$IDtoUse, 8, 9)

## Recategorize tillage
final <- final %>%
  mutate(tillage = ifelse(`Assign a tillage category to this field`=='Tillage is different across the field', 'Reduced till',`Assign a tillage category to this field`)) %>%
  mutate(tillagecont = recode(tillage, 'No-till' = 1, "Strip till" = 2, 'Reduced till' = 3, 'Conventional till' = 4))

## Drop <Null> cover crops
final <- final %>% filter(
  `Cover Crop` != '<Null>'
)

## Convert to numeric forms
final$active_carbon <- as.numeric(final$active_carbon)
final$aggregate_stability <- as.numeric(final$aggregate_stability)
final$water_capacity <- as.numeric(final$water_capacity)
final$ph <- as.numeric(final$ph)
final$p <- as.numeric(final$p)
final$k <- as.numeric(final$k)
final$mg <- as.numeric(final$mg)
final$fe <- as.numeric(final$fe)
final$mn <- as.numeric(final$mn)
final$zn <- as.numeric(final$zn)
final$final_OM <- as.numeric(final$final_OM)
final$respiration <- as.numeric(final$respiration)
final$ace_soil_protein_index <- as.numeric(final$ace_soil_protein_index)

## Take difference in all soil properties between first and last years
final <- final %>%
  arrange(idstrp,as.numeric(smpl_yr)) %>%
  group_by(idstrp) %>%
  mutate(
    ac.diff = (last(active_carbon) - first(active_carbon)),
    as.diff = (last(aggregate_stability) - first(aggregate_stability)),
    whc.diff = (last(water_capacity) - first(water_capacity)),
    som.diff = (last(final_OM) - first(final_OM)),
    resp.diff = (last(respiration) - first(respiration)),
    pro.diff = (last(ace_soil_protein_index) - first(ace_soil_protein_index))
  )

## Remove data without cover crops
final <- final %>%
  filter(is.na(`Cover Crop`)==FALSE)

## Filter data to observations with at least two years
temp <- plyr::count(final,"idstrp")
final <- left_join(final, temp)

final <- final %>% filter(
  freq > 1
)

## Filter unnecessary variables
final <- final %>% 
  select(-freq)

## Remove extra data
rm(cashcrop); rm(soil); rm(tillagetype); rm(treatment); rm(treatmenttiming); rm(temp)

## Save data
save.image("~/Box Sync/Work/Code/shp/shp-data.RData")
