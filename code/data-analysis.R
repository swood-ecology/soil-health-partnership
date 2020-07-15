#### LOAD PACKAGES ####
library(tidyverse)  # General data manipulation
library(lme4)       # Mixed-effects modeling
library(lmerTest)   # P-values for lme objects
library(table1)     # For summary table
library(performance) # For model checking
library(influence.ME) # For Cook's influence

#### READ DATA ####
load("shp-data.RData")

#### SUMMARY STATISTICS #####
summary.dat <- data.sub %>%
  filter(TrtType == "Cover" | TrtType == "Cover/TILL") %>%
  select(
    Sand:Clay, AggStab, ACESoilProtein, WHC, som.final, Respiration, ActiveCarbon, State
  )

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 2), c("",
    "Mean (SD)" = sprintf("%s (&plusmn; %s)", MEAN, SD)
  ))
}

label(summary.dat$som.final) <- "Organic Matter (%)"
label(summary.dat$ActiveCarbon) <- "Active Carbon"
label(summary.dat$ACESoilProtein) <- "Soil Protein"
label(summary.dat$WHC) <- "Water Holding Capacity"
label(summary.dat$AggStab) <- "Aggregate Stability"

table1(~ som.final + Respiration + ActiveCarbon + ACESoilProtein + WHC + AggStab + Sand + Silt + Clay | State,
  data = summary.dat,
  render.continuous = my.render.cont,
  overall = NULL
)

rm(summary.dat)

#### DATA MANIPULATION ####
## Variable standardization
data.sub.std <- data.sub %>%
  mutate(
    c.AMS = treat_final_AMS - mean(treat_final_AMS),
    c.SC = lagCrop_SC - mean(lagCrop_SC),
    c.SB = lagCrop_SB - mean(lagCrop_SB),
    c.W = lagCrop_W - mean(lagCrop_W),
    c.Soy = lagCropCat_Soy - mean(lagCropCat_Soy),
    c.Wheat = lagCropCat_Wheat - mean(lagCropCat_Wheat),
    s.yrs = (YrTrtFINAL - mean(YrTrtFINAL)) / 2 * sd(YrTrtFINAL),
    s.yr = (year-mean(year)) / 2 * sd(year),
    s.yrsInvolved = (YrTrt - mean(YrTrt)) / 2 * sd(YrTrt),
    s.clay = (Clay - mean(Clay)) / 2 * sd(Clay),
    s.silt = (Silt - mean(Silt)) / 2 * sd(Silt),
    year.farm = paste0(field_name, "_", year)
  )

## Subset data to cover crops ##
data.sub.cc <- data.sub.std %>%
  filter(TrtType == "Cover" | TrtType == "Cover/TILL")
data.sub.cc$treat_final <- as.factor(data.sub.cc$treat_final)

#### 1. ALL DATA MODELS ####
#### 1a. Active Carbon ####
# Fit initial model
ac <- lmer(ActiveCarbon ~ c.AMS*s.yrsInvolved + c.Soy + s.silt + s.clay + (1|SHPID_Strip),
           data = data.sub.std %>%
             filter(row_number() != 768,
                    row_number() != 769))
# Check model
ac %>% r2()
summary(ac)
ac %>% check_model()

# Panel data model
plm(ActiveCarbon ~ c.AMS*s.yrsInvolved + c.Soy + s.silt + s.clay,
    data=data.sub.std, model = "random",
    index=c("SHPID_Strip")
) %>% 
  summary()

#### 1b. Aggregate Stability ####
# Fit initial model
as <- lmer(AggStab ~ c.AMS*s.yrsInvolved + c.Soy + s.silt + s.clay +
  (1 | field_name),
  data = data.sub.std
)
# Check model
as %>% r2()
as %>% summary()
as %>% check_model() 

#### 1c. Protein ####
pro <- lmer(log(ACESoilProtein) ~ c.AMS*s.yrsInvolved + c.Soy + s.silt + s.clay +
  (1 | field_name),
data = data.sub.std
)
pro %>% r2()
pro %>% summary()
pro %>% check_model()

#### 1d. Respiration ####
data.sub.std$Respiration <- data.sub.std$Respiration + 0.000001
resp <- lmer(log(Respiration) ~ c.AMS*s.yrsInvolved + c.Soy + s.silt + s.clay +
  (1 | field_name),
data = data.sub.std %>%
  filter(row_number() != 449)
)
resp %>% r.squaredGLMM()
resp %>% summary()
resp %>% check_model()

#### 1e. Water Holding Capacity ####
whc <- lmer(WHC ~ c.AMS+s.yrsInvolved + c.Soy + s.silt + s.clay +
  (1 | field_name),
data = data.sub.std
)
whc %>% r2()
whc %>% summary()
whc %>% check_model()

#### 2. COVER CROP MODELS ####
#### 2a. Active Carbon ####
# Run model for all data
ac.cc.all <- lmer(ActiveCarbon ~ c.AMS*s.yr + s.yrsInvolved + s.silt + s.clay + c.Soy + 
       (1 | field_name),
     data = data.sub.cc)
summary(ac.cc.all)
r2(ac.cc.all)

ac.cc <- list()
for(i in 2015:2019){
  ac.cc[[i]] <- lmer(ActiveCarbon ~ c.AMS+s.yrsInvolved + s.silt + s.clay + c.Soy +
                     (1 | field_name),
                   data = data.sub.cc %>%
                     filter(year==i)
  ) 
}

ac.cc[[2019]] %>% r2()
ac.cc[[2019]] %>% summary()
ac.cc[[2015]] %>% check_model()

#### 2b. Aggregate Stability ####
data.sub.cc$AggStab <- data.sub.cc$AggStab + 0.00001
as.cc.all <- lmer(log(AggStab) ~ c.AMS*s.yr+s.yrsInvolved + c.Soy + s.clay + s.silt +
  (1 | field_name), data = data.sub.cc %>% filter(AggStab < 75,
                                                  row_number() != 210)
)
as.cc.all %>% r2()
as.cc.all %>% summary()
as.cc.all %>% check_model()

as.cc <- list()
for(i in 2015:2019){
  as.cc[[i]] <- lmer(log(AggStab) ~ c.AMS+s.yrsInvolved + s.silt + s.clay + c.Soy+
                       (1 | field_name),
                     data = data.sub.cc %>%
                       filter(AggStab < 75,
                              row_number() != 210,
                              year==i)
  ) 
}

as.cc[[2019]] %>% r2()
as.cc[[2019]] %>% summary()
as.cc[[2015]] %>% check_model()


#### 2c. Protein #####
# Model across all years
pro.cc.all <- lmer(log(ACESoilProtein) ~ c.AMS*s.yr + s.yrsInvolved + c.Soy + s.silt + s.clay +
                  (1 | field_name),
                data = data.sub.cc 
)

pro.cc.all %>% r2()
pro.cc.all %>% summary()
pro.cc.all %>% check_model()

# Model for each year
pro.cc <- list()
for(i in 2015:2019){
  pro.cc[[i]] <- lmer(log(ACESoilProtein) ~ c.AMS+s.yrsInvolved + s.silt + s.clay + c.Soy+
                       (1 | field_name),
                     data = data.sub.cc %>%
                       filter(year==i)
  ) 
}

pro.cc[[2019]] %>% r2()
pro.cc[[2019]] %>% summary()
pro.cc[[2015]] %>% check_model()


#### 2d. Respiration ####
resp.cc.all <- lmer(Respiration ~ c.AMS*s.yr + s.yrsInvolved + c.Soy + s.silt + s.clay +
  (1 | field_name),
data = data.sub.cc %>%
  filter(row_number() != 1285)
)
resp.cc.all %>% r2()
resp.cc.all %>% summary()
resp.cc.all %>% check_model()

resp.cc <- list()
for(i in 2015:2019){
  resp.cc[[i]] <- lmer(Respiration ~ c.AMS + s.yrsInvolved +s.silt + s.clay + c.Soy +
                        (1 | field_name),
                      data = data.sub.cc %>%
                        filter(year==i,
                               row_number() != 1285)
  ) 
}

resp.cc[[2018]] %>% r2()
resp.cc[[2019]] %>% summary()
resp.cc[[2015]] %>% check_model()


#### 2e. Water Holding Capacity ####
whc.cc.all <- lmer(WHC ~ c.AMS*s.yr+s.yrsInvolved+c.Soy+s.clay + s.silt +
  (1 | field_name),
data = data.sub.cc 
)
whc.cc.all %>% summary()
whc.cc.all %>% r2()
whc.cc.all %>% check_model()

whc.cc <- list()
for(i in 2015:2019){
  whc.cc[[i]] <- lmer(WHC ~ c.AMS+s.yrsInvolved + s.silt + s.clay + c.Soy+
                       (1 | field_name),
                     data = data.sub.cc %>%
                       filter(year==i)
  ) 
}
whc.cc[[2019]] %>% r2()
whc.cc[[2019]] %>% summary()
whc.cc[[2015]] %>% check_model()


#### 2f. Soil organic matter ####
som.cc.all <- lmer(log(som.final) ~ c.AMS*s.yr+s.yrsInvolved + s.silt + s.clay + c.Soy+
                       (1 | field_name),
                     data = data.sub.cc
)
som.cc.all %>% summary()
som.cc.all %>% r2()
som.cc.all %>% check_model()

som.cc <- list()
for(i in 2015:2019){
  som.cc[[i]] <- lmer(log(som.final) ~ c.AMS+s.yrsInvolved + s.silt + s.clay + c.Soy+
                        (1 | field_name),
                      data = data.sub.cc %>%
                        filter(year==i)
  ) 
}
som.cc[[2019]] %>% r2()
som.cc[[2019]] %>% summary()
som.cc[[2015]] %>% check_model()


#### EXPORT DATA ####
rm(data)
rm(data.2018.new)
rm(data.2019)
rm(data.last_yr)
rm(data.sub)
rm(data.sub.cc)
rm(data.sub.std)
rm(data.diff)
rm(i)
save.image("~/Box Sync/Work/Code/shp/shp-models.RData")
