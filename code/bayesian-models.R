library(rstan)        # For interfacing with stan
library(tidyverse)
library(broom.mixed)
library(tidybayes)

load("shp-data.RData")

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
    s.yr = (year - mean(year))/2*sd(year),
    s.yrsInvolved = (YrTrt - mean(YrTrt)) / 2 * sd(YrTrt),
    s.clay = (Clay - mean(Clay))/2*sd(Clay),
    s.silt = (Silt - mean(Silt))/2*sd(Silt),
    trtYrs = treat_final_AMS * year,
    s.trtYrs = (trtYrs - mean(trtYrs))/2*sd(trtYrs),
    farm = field_name
  )

# Subset to cover crops
data.sub.cc <- data.sub.std %>%
  filter(TrtType == "Cover" | TrtType == "Cover/TILL")

# List data for Stan model
som.cc.qr.list <- list(
  ## Data dimensions
  N=nrow(data.sub.cc),
  K=7,
  J=length(unique(as.factor(data.sub.cc$farm))),
  ## Data
  y=data.sub.cc$som.final,
  x=data.sub.cc[,c('c.AMS','s.yr','s.yrsInvolved','s.trtYrs','c.Soy','s.clay','s.silt')],
  ## Random effect
  farm=as.numeric(as.factor(data.sub.cc$farm))
)

## Prior on clay for logged relationship: 0.001 (0.0002 = SD)
som.cc.qr <- stan(file = "code/stan/model_re_qr.stan",
               data = som.cc.qr.list,
               iter = 4000,
               control=list(max_treedepth=15),
               chains = 4) 

summary(som.cc.qr,pars=c('betaTrt','betaYrs','betaYrsInv','betaTrtYrs','betaSoy','betaClay','betaSilt'))
print(som.cc.qr,pars=c('betaTrt','betaYrs','betaYrsInv','betaTrtYrs','betaSoy','betaClay','betaSilt'))

rm(data.2019); rm(data.diff); rm(data.last_yr); rm(data.sub); rm(data.sub.cc)
rm(data.sub.std); rm(data); rm(data.2018.new)


## Bayesian model plotting
model.cc.plot <- tidy(som.cc.qr,pars=c('betaTrt','betaYrs','betaYrsInv','betaTrtYrs','betaSoy','betaClay','betaSilt')) %>%
  mutate(term = c(
    betaTrt="Treatment",
    betaYrs="Years",
    betaYrsInv="Years involved",
    betaTrtYrs="Treatment years",
    betaSoy = "Soy (1/0)",
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

y_pred_som <- rstan::extract(som.cc.qr,pars='y_tilde')
y_pred_som <- unlist(y_pred_som, use.names=FALSE)

som.pp.data <- data.frame(
  c(y_pred_som,som.cc.qr.list$y),
  c(rep("y_pred",length(y_pred_som)),
    rep("y_obs",length(som.cc.qr.list$y)))
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
