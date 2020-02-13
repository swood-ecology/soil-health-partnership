library(rstan)        # For interfacing with stan
library(tidyverse)
library(broom.mixed)
library(tidybayes)

load("~/Box Sync/Work/Code/shp/shp-data.RData")

# Global options for running model
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Standardize and center data
data.sub.std <- data.sub %>%
  mutate(
    c.AMS = treat_final_AMS - mean(treat_final_AMS),
    c.SC = lagCrop_SC - mean(lagCrop_SC),
    c.SB = lagCrop_SB - mean(lagCrop_SB),
    c.W = lagCrop_W - mean(lagCrop_W),
    c.Soy = lagCropCat_Soy - mean(lagCropCat_Soy),
    c.Wheat = lagCropCat_Wheat - mean(lagCropCat_Wheat),
    s.yrs = (YrTrtFINAL - mean(YrTrtFINAL))/2*sd(YrTrtFINAL),
    s.clay = (Clay - mean(Clay))/2*sd(Clay),
    s.silt = (Silt - mean(Silt))/2*sd(Silt),
    trtfinal = treat_final_AMS * YrTrtFINAL,
    s.trtfinal = (trtfinal - mean(trtfinal))/2*sd(trtfinal),
    year.farm = paste0(field_name,"_",year)
  )

# Subset to cover crops
data.sub.cc <- data.sub.std %>%
  filter(TrtType == "Cover" | TrtType == "Cover/TILL")

# List data for Stan model
som.notstd.list <- list(
  ## Data dimensions
  N=nrow(data.sub.cc),
  K=5,
  J=length(unique(data.sub.cc$year.farm)),
  ## Random effect
  farmYears=as.integer(as.factor(data.sub.cc$year.farm)),
  ## Independent variables
  trtByYrs=as.numeric(data.sub.cc$trtfinal),
  soy=as.numeric(data.sub.cc$lagCropCat_Soy),
  wheat=as.numeric(data.sub.cc$lagCropCat_Wheat),
  clay=data.sub.cc$Clay,
  silt=data.sub.cc$Silt,
  ## Dependent variable
  y=data.sub.cc$som.final
)
som.list <- list(
  ## Data dimensions
  N=nrow(data.sub.std),
  K=5,
  J=length(unique(data.sub.std$year.farm)),
  ## Random effect
  farmYears=as.integer(as.factor(data.sub.std$year.farm)),
  ## Independent variables
  trtByYrs=as.numeric(data.sub.std$s.trtfinal),
  soy=as.numeric(data.sub.std$c.Soy),
  wheat=as.numeric(data.sub.std$c.Wheat),
  clay=data.sub.std$s.clay,
  silt=data.sub.std$s.silt,
  ## Dependent variable
  y=data.sub.std$som.final
)
som.cc.list <- list(
  ## Data dimensions
  N=nrow(data.sub.cc),
  K=5,
  J=length(unique(data.sub.cc$year.farm)),
  ## Random effect
  farmYears=as.integer(as.factor(data.sub.cc$year.farm)),
  ## Independent variables
  trtByYrs=as.numeric(data.sub.cc$s.trtfinal),
  soy=as.numeric(data.sub.cc$c.Soy),
  wheat=as.numeric(data.sub.cc$c.Wheat),
  clay=data.sub.cc$s.clay,
  silt=data.sub.cc$s.silt,
  ## Dependent variable
  y=data.sub.cc$som.final
)

## Prior on clay for logged relationship: 0.001 (0.0002 = SD)
som <- stan(file = "code/stan/model_inter.stan",
               data = som.list,
               iter = 4000,
               control=list(max_treedepth=15),
               chains = 3) 

som.cc <- stan(file = "code/stan/model_inter.stan",
            data = som.cc.list,
            iter = 4000,
            control=list(max_treedepth=15),
            chains = 3) 

som.cc.notstd <- stan(file = "code/stan/model_inter.stan",
               data = som.notstd.list,
               iter = 4000,
               control=list(max_treedepth=15),
               chains = 3) 

summary(som.cc.notstd,pars=c('betaTrtByYrs','betaSoy','betaWheat','betaClay','betaSilt'))
summary(som.cc,pars=c('betaTrtByYrs','betaSoy','betaWheat','betaClay','betaSilt'))
print(som.cc,pars=c('betaTrtByYrs','betaSoy','betaWheat','betaClay','betaSilt'))

summary(som,pars=c('betaTrtByYrs','betaSoy','betaWheat','betaClay','betaSilt'))

## Bayesian model plotting
model.cc.plot <- tidy(som.cc,pars=c('betaTrtByYrs','betaSoy','betaWheat','betaClay','betaSilt')) %>%
  mutate(term = c(
    betaTrtByYrs = "Treatment (1/0)\n x Years of treatment",
    betaSoy = "Soy (1/0)",
    betaWheat = "Wheat (1/0)",
    betaClay = "Clay",
    betaSilt = "Silt"
  ))

dotwhisker::dwplot(model.cc.plot,
       vline = geom_vline(xintercept = 0, colour = "#7e6a65", linetype = 2)
) + 
  xlab("\nCoefficient Estimate") + ylab("") +
  ggtitle("Soil organic matter") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none")

y_pred_som <- rstan::extract(som.cc,pars='y_tilde')
y_pred_som <- unlist(y_pred_som, use.names=FALSE)

som.pp.data <- data.frame(
  c(y_pred_som,som.list$y),
  c(rep("y_pred",length(y_pred_som)),
    rep("y_obs",length(som.cc.list$y)))
)
names(som.pp.data) <- c("y","type")

ggplot(som.pp.data, aes(x=y)) + 
  geom_density(aes(group=type, fill=type), alpha=0.75) + 
  xlab("Soil organic matter") + ylab("Density") +
  theme_classic() +
  scale_fill_manual(values=c("#00703c","#f3901d")) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.85,0.55),
    panel.grid = element_blank(),
    panel.background = element_rect(fill="white"),
    plot.background = element_rect(fill="white")
  )
