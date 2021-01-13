#### LOAD PACKAGES ####
library(tidyverse)  # General data manipulation
library(lme4)       # Mixed-effects modeling
library(lmerTest)   # P-values for lme objects
library(table1)     # For summary table
library(performance) # For model checking
library(influence.ME) # For Cook's influence

#### READ DATA ####
load("shp-data.RData")

#### FILTER FARMS ####
# Generate list of farms to filter by for Maria's interest in cover crop type
farmSub <- c(
  'SHP2014IA001','SHP2014IA002','SHP2014IA004','SHP2014IA005','SHP2014IL001','SHP2014IL002','SHP2014IL005','SHP2014IN002',
  'SHP2015IA003','SHP2015IA004','SHP2015IA005','SHP2015IA006','SHP2015IA007','SHP2015IA008','SHP2015IL001','SHP2015IL003',
  'SHP2015IL004','SHP2015OH001','SHP2016IA002','SHP2016IA003','SHP2016IA004','SHP2016IA005','SHP2016IL003','SHP2016IN003',
  'SHP2016IN005','SHP2016IN008','SHP2017IA002','SHP2017IA003','SHP2017IA004','SHP2017IA006','SHP2017IL003','SHP2017IL005',
  'SHP2017IL009','SHP2017IN003','SHP2017IN004','SHP2017MO004','SHP2017MO005','SHP2017NE001','SHP2017NE002','SHP2017NE004',
  'SHP2017OH003','SHP2017WI003','SHP2017WI004'
)
# final <- final %>% filter(IDtoUse %in% farmSub)

#### SUMMARY STATISTICS #####
summary.dat <- final %>%
  select(
    soil_texture_sand:water_capacity, aggregate_stability, ace_soil_protein_index, final_OM, respiration, 
    active_carbon, yrsTrt, `Cover Crop`, State
  )

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 2), c("",
    "Mean (SD)" = sprintf("%s (&plusmn; %s)", MEAN, SD)
  ))
}

table1(~  as.numeric(soil_texture_clay) + as.numeric(soil_texture_silt) + as.numeric(soil_texture_sand) + water_capacity 
       + aggregate_stability + ace_soil_protein_index + final_OM + respiration + active_carbon  + yrsTrt 
       | State,
  data = summary.dat,
  render.continuous = my.render.cont,
  overall = NULL
)

table1(~  water_capacity + aggregate_stability + ace_soil_protein_index + final_OM + respiration + active_carbon 
       | `Cover Crop` + yrsTrt,
       data = summary.dat %>% filter(yrsTrt != 0),
       render.continuous = my.render.cont,
       overall = NULL)



#### DATA MANIPULATION ####
## Create dummy variables
final <- final %>%
  mutate(
    CC = ifelse(`Cover Crop`=="Yes",1,0),
  )

## Variable standardization
data.std <- final %>%
  mutate(
    c.CC = CC - mean(CC),
    s.yrsTrt = (yrsTrt - mean(yrsTrt)) / 2 * sd(yrsTrt),
    s.yr = (as.numeric(smpl_yr)-mean(as.numeric(smpl_yr))) / 2*sd(as.numeric(smpl_yr)),
    s.clay = (as.numeric(soil_texture_clay) - mean(as.numeric(soil_texture_clay))) / 2 * sd(as.numeric(soil_texture_clay)),
    s.silt = (as.numeric(soil_texture_silt) - mean(as.numeric(soil_texture_silt))) / 2 * sd(as.numeric(soil_texture_silt)),
    IDtoUse = IDtoUse
  )


#### 1. ALL DATA MODELS ####
#### 1a. Active Carbon ####
# Fit initial model
ac <- lmer(active_carbon ~ `Cover Crop`*yrsTrt + as.numeric(smpl_yr) + as.numeric(soil_texture_clay) + as.numeric(soil_texture_silt) + (1|IDtoUse),
           data = final)

# Check model
ac %>% r2()
summary(ac)
ac %>% performance::check_model()

#### 1b. Aggregate Stability ####
# Fit initial model
as <- lmer(log(aggregate_stability) ~ `Cover Crop`*yrsTrt + as.numeric(smpl_yr) + as.numeric(soil_texture_clay) + as.numeric(soil_texture_silt) + (1|IDtoUse),
           data = final
)

# Check model
as %>% r2()
as %>% summary()
as %>% check_model() 

#### 1c. Protein ####
pro <- lmer(log(ace_soil_protein_index) ~ `Cover Crop`*yrsTrt + as.numeric(smpl_yr) + as.numeric(soil_texture_clay) + as.numeric(soil_texture_silt) + (1|IDtoUse),
            data = final)

pro %>% r2()
pro %>% summary()
pro %>% check_model()

#### 1d. Respiration ####
resp <- lmer(log(respiration) ~ `Cover Crop`*yrsTrt + as.numeric(smpl_yr) + as.numeric(soil_texture_clay) + as.numeric(soil_texture_silt) + (1|IDtoUse),
             data = final
)

resp %>% r2()
resp %>% summary()
resp %>% check_model()

#### 1e. Water Holding Capacity ####
whc <- lmer(water_capacity ~ `Cover Crop`*yrsTrt + as.numeric(smpl_yr) + as.numeric(soil_texture_clay) + as.numeric(soil_texture_silt) + (1|IDtoUse),
            data = final
)

whc %>% r2()
whc %>% summary()
whc %>% check_model()

#### 1f. Soil Organic Matter ####
som <- lmer(final_OM ~ `Cover Crop`*yrsTrt + as.numeric(smpl_yr) + as.numeric(soil_texture_clay) + as.numeric(soil_texture_silt) + (1|IDtoUse),
            data = final
)

som %>% r2()
som %>% summary()
som %>% check_model()


#### 2. MODELS BY YEAR ####
#### 2a. Active Carbon ####
# Run model for all data
ac.yr <- list()
for(i in 2015:2019){
  ac.yr[[i]] <- lmer(active_carbon ~ CC + yrsTrt + as.numeric(soil_texture_clay) + 
                       as.numeric(soil_texture_silt) + (1|IDtoUse),
                   data = final %>%
                     filter(as.numeric(smpl_yr)==i)
  )
}

ac.yr[[2015]] %>% r2()
ac.yr[[2015]] %>% summary()
ac.yr[[2015]] %>% check_model()

#### 2b. Aggregate Stability ####
as.yr <- list()
for(i in 2015:2019){
  as.yr[[i]] <- lmer(log(aggregate_stability) ~ CC + yrsTrt + as.numeric(soil_texture_clay) + 
                       as.numeric(soil_texture_silt) + (1|IDtoUse),
                     data = final %>%
                       filter(as.numeric(smpl_yr)==i)
  )
}

as.yr[[2015]] %>% r2()
as.yr[[2015]] %>% summary()
as.yr[[2015]] %>% check_model()

#### 2c. Protein #####
# Model for each year
pro.yr <- list()
for(i in 2015:2019){
  pro.yr[[i]] <- lmer(log(ace_soil_protein_index) ~ CC + yrsTrt + as.numeric(soil_texture_clay) + 
                        as.numeric(soil_texture_silt) + (1|IDtoUse),
                     data = final %>%
                       filter(smpl_yr==i)
  ) 
}

pro.yr[[2015]] %>% r2()
pro.yr[[2015]] %>% summary()
pro.yr[[2015]] %>% check_model()

#### 2d. Respiration ####
resp.yr <- list()
for(i in 2015:2019){
  resp.yr[[i]] <- lmer(log(respiration) ~ CC + yrsTrt + as.numeric(soil_texture_clay) + 
                         as.numeric(soil_texture_silt) + (1|IDtoUse),
                      data = final %>%
                        filter(smpl_yr==i)
  ) 
}

resp.yr[[2015]] %>% r2()
resp.yr[[2015]] %>% summary()
resp.yr[[2015]] %>% check_model()


#### 2e. Water Holding Capacity ####
whc.yr <- list()
for(i in 2015:2019){
  whc.yr[[i]] <- lmer(water_capacity ~ CC + yrsTrt + as.numeric(soil_texture_clay) + 
                        as.numeric(soil_texture_silt) + (1|IDtoUse),
                     data = final %>%
                       filter(smpl_yr==i)
  ) 
}
whc.yr[[2015]] %>% r2()
whc.yr[[2015]] %>% summary()
whc.yr[[2015]] %>% check_model()


#### 2f. Soil organic matter ####
som.yr <- list()
for(i in 2015:2019){
  som.yr[[i]] <- lmer(final_OM ~ CC + yrsTrt + as.numeric(soil_texture_clay) + 
                        as.numeric(soil_texture_silt) + (1|IDtoUse),
                      data = final %>%
                        filter(smpl_yr==i)
  )
}
som.yr[[2015]] %>% r2()
som.yr[[2015]] %>% summary()
som.yr[[2015]] %>% check_model()


#### EXPORT DATA ####
rm(final); rm(data.std); rm(summary.dat)
save.image("~/Box Sync/Work/Code/shp/shp-models.RData")
