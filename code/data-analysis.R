library(tidyverse) # General data manipulation
library(lme4) # Mixed-effects modeling
library(lmerTest)
library(MuMIn)
library(sjstats)
library(table1) # For summary table

load("~/Box Sync/Work/Code/shp/shp-data.RData")

# Summary statistics
summary.dat <- data.sub %>%
  select(
    Sand:Clay, AggStab, ACESoilProtein, WHC, som.final, Respiration, ActiveCarbon, State, TrtType,
    lagCrop, YrTrt
  )

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 2), c("",
    "Mean (SD)" = sprintf("%s (&plusmn; %s)", MEAN, SD)
  ))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) {
    with(
      y,
      sprintf("%d (%0.0f %%)", FREQ, PCT)
    )
  }))
}

label(summary.dat$som.final) <- "Organic Matter (%)"
label(summary.dat$ActiveCarbon) <- "Active Carbon"
label(summary.dat$ACESoilProtein) <- "Soil Protein"
label(summary.dat$WHC) <- "Water Holding Capacity"
label(summary.dat$AggStab) <- "Aggregate Stability"
label(summary.dat$YrTrt) <- "Years of Treatment"
label(summary.dat$TrtType) <- "Type of Treatment"

table1(~ som.final + Respiration + ActiveCarbon + ACESoilProtein + WHC + AggStab + Sand + Silt + Clay + YrTrt + TrtType | State,
  data = summary.dat,
  render.continuous = my.render.cont,
  render.categorical = my.render.cat,
  overall = NULL
)

rm(summary.dat)

# Standardize variables for regression
data.sub.std <- data.sub %>%
  mutate(
    c.AMS = treat_final_AMS - mean(treat_final_AMS),
    c.SC = lagCrop_SC - mean(lagCrop_SC),
    c.SB = lagCrop_SB - mean(lagCrop_SB),
    c.W = lagCrop_W - mean(lagCrop_W),
    c.Soy = lagCropCat_Soy - mean(lagCropCat_Soy),
    c.Wheat = lagCropCat_Wheat - mean(lagCropCat_Wheat),
    s.yrs = (YrTrtFINAL - mean(YrTrtFINAL)) / 2 * sd(YrTrtFINAL),
    s.clay = (Clay - mean(Clay)) / 2 * sd(Clay),
    s.silt = (Silt - mean(Silt)) / 2 * sd(Silt),
    year.farm = paste0(field_name,"_",year)
  )

## FOR ALL DATA
## Active Carbon
# ac <- lmer(ActiveCarbon ~ treat_final_AMS:YrTrtFINAL + lagCropCat_Soy + lagCropCat_Wheat + Clay + Silt +
#   (1 | field_name),
# data = data.sub.std
# )
# ac %>% r.squaredGLMM()
# ac %>% summary()

# ## Respiration
# resp <- lmer(Respiration ~ treat_final_AMS:YrTrtFINAL + lagCropCat_Soy + lagCropCat_Wheat + Clay + Silt +
#   (1 | field_name),
# data = data.sub.std
# )
# resp %>% r.squaredGLMM()
# resp %>% summary()

# ## Aggregate Stability
# as <- lmer(AggStab ~ treat_final_AMS:YrTrtFINAL + lagCropCat_Soy + lagCropCat_Wheat + Clay + Silt +
#   (1 | field_name),
# data = data.sub.std
# )
# as %>% r.squaredGLMM()
# as %>% summary()

# ## Water Holding Capacity
# whc <- lmer(WHC ~ treat_final_AMS:YrTrtFINAL + lagCropCat_Soy + lagCropCat_Wheat + Clay + Silt +
#   (1 | field_name),
# data = data.sub.std
# )
# whc %>% summary()
# whc %>% r.squaredGLMM()

# ## Protein
# pro <- lmer(ACESoilProtein ~ treat_final_AMS:YrTrtFINAL + lagCropCat_Soy + lagCropCat_Wheat + Clay + Silt +
#   (1 | field_name),
# data = data.sub.std
# )
# pro %>% summary()
# pro %>% r.squaredGLMM()


# Subset to cover crops
data.sub.cc <- data.sub.std %>%
  filter(TrtType == "Cover" | TrtType == "Cover/TILL")

#### Active Carbon ####
ac.cc <- lmer(ActiveCarbon ~ c.AMS:s.yrs + c.Soy + c.Wheat + s.clay + s.silt +
  (1 | field_name),
data = data.sub.cc
)
ac.cc.inter <- lmer(ActiveCarbon ~ treat_final_AMS:YrTrtFINAL + lagCropCat_Soy + lagCropCat_Wheat + Clay + Silt +
                      (1 | field_name),
                    data = data.sub.cc
)
ac.cc %>% r.squaredGLMM()
ac.cc %>% summary()

#### Respiration ####
resp.cc.inter <- lmer(Respiration ~ treat_final_AMS:YrTrtFINAL + lagCropCat_Soy + lagCropCat_Wheat + Clay + Silt +
  (1 | field_name),
data = data.sub.cc
)
resp.cc.inter %>% r.squaredGLMM()
resp.cc.inter %>% summary()

#### Aggregate Stability ####
as.cc <- lmer(AggStab ~ c.AMS:s.yrs + c.Soy + c.Wheat + s.clay + s.silt +
  (1 | field_name),
data = data.sub.cc %>% filter(AggStab < 75)
)
as.cc.inter <- lmer(AggStab ~ treat_final_AMS:YrTrtFINAL + lagCropCat_Soy + lagCropCat_Wheat + Clay + Silt +
                      (1 | field_name),
                    data = data.sub.cc %>% filter(AggStab < 75)
)
as.cc.inter %>% r.squaredGLMM()
as.cc.inter %>% summary()

#### Water Holding Capacity ####
whc.cc <- lmer(WHC ~ c.AMS:s.yrs + c.Soy + c.Wheat + s.clay + s.silt +
  (1 | field_name),
data = data.sub.cc
)
whc.cc.inter <- lmer(WHC ~ treat_final_AMS:YrTrtFINAL + lagCropCat_Soy + lagCropCat_Wheat + Clay + Silt +
                      (1 | field_name),
                    data = data.sub.cc
)
whc.cc %>% summary()
whc.cc %>% r.squaredGLMM()

#### Protein #####
pro.cc.inter <- lmer(ACESoilProtein ~ treat_final_AMS:YrTrtFINAL + lagCropCat_Soy + lagCropCat_Wheat + Clay + Silt +
                 (1 | field_name),
               data = data.sub.cc
)
pro.cc.inter %>% summary()
pro.cc.inter %>% r.squaredGLMM()

#### Soil organic matter ####
som.cc.inter <- lmer(log(som.final) ~ treat_final_AMS:YrTrtFINAL + lagCropCat_Soy + lagCropCat_Wheat + Clay + Silt +
                       (1 | year.farm),
                     data = data.sub.cc
)

interactions::interact_plot(som.cc.inter,pred=YrTrtFINAL,modx=treat_final_AMS,
              interval=TRUE,width=0.5,
              partial.residuals=TRUE,jitter=0.3,colors=c("#00703c","#f3901d"),
              x.label = "\nYears of treatment", y.label = "Partial Residuals\n",
              main.title = "Soil organic matter",  legend.main = "",modx.labels=c("Control","Treatment")) +
  theme_classic()


som.cc.inter %>% tidy()
lmer(log(som.final) ~ treat_final_AMS:YrTrtFINAL + Clay + Silt +
                            (1 | year.farm),
                          data = data.sub.cc) %>% r.squaredGLMM()

som.plot <- tidy(som.cc.inter) %>%
  dotwhisker::relabel_predictors(c(
    `c.AMS:s.yrs` = "Treatment (1/0)\nx Years of treatment",
    c.Soy = "Soy (1/0)",
    c.Wheat = "Wheat (1/0)",
    s.clay = "Clay",
    s.silt = "Silt"
  )) %>%
  slice(1:7) %>%
  filter(term != "(Intercept)",
         term != "sd__(Intercept)")

dotwhisker::dwplot(som.plot,
       vline = geom_vline(xintercept = 0, colour = "#7e6a65", linetype = 2)
) + 
  xlab("\nCoefficient Estimate") + ylab("") +
  ggtitle("Soil organic matter") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none")

########### EXPORT DATA #######

rm(data)
rm(data.2018.new)
rm(data.2019)
rm(data.last_yr)
rm(data.redo)
rm(data.sub)
rm(data.sub.cc)
rm(data.sub.std)
save.image("~/Box Sync/Work/Code/shp/shp-models.RData")
