library(rstan)        # For interfacing with stan
library(tidyverse)
library(broom.mixed)
library(tidybayes)

load("shp-data.RData")

# Global options for running model
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Create dummy
final <- final %>%
  mutate(
    CC = ifelse(`Cover Crop`=="Yes",1,0),
  )

# Standardize and center data
final <- final %>%
  mutate(
    smpl_yr = as.numeric(smpl_yr),
    soil_texture_clay = as.numeric(soil_texture_clay),
    soil_texture_silt = as.numeric(soil_texture_silt),
    TrtYrs = CC * yrsTrt
  )

# List data for Stan model
model.data <- list(
  ## Data dimensions
  N=nrow(final),
  K=6,
  J=length(unique(as.factor(final$IDtoUse))),
  ## Data
  y=final$final_OM,
  x=final[,c('CC','smpl_yr','yrsTrt','TrtYrs','soil_texture_clay','soil_texture_silt')],
  ## Random effect
  farm=as.numeric(as.factor(final$IDtoUse))
)

model.data.2 <- list(
  ## Data dimensions
  N=nrow(final),
  K=6,
  J=length(unique(as.factor(final$IDtoUse))),
  ## Data
  y=final$final_OM,
  trt=final$CC,
  yrs=final$smpl_yr,
  yrsInv=final$yrsTrt,
  trtYrs=final$TrtYrs,
  clay=final$soil_texture_clay,
  silt=final$soil_texture_silt,
  ## Random effect
  farm=as.numeric(as.factor(final$IDtoUse))
)

## Prior on clay for logged relationship: 0.001 (0.0002 = SD)
som.cc.qr <- stan(file = "code/stan/model_re_qr.stan",
               data = model.data,
               iter = 5000,
               control=list(max_treedepth=15),
               chains = 3)

som.cc <- stan(file = "code/stan/model_re.stan",
                  data = model.data.2,
                  iter = 5000,
                  control=list(max_treedepth=15),
                  chains = 4)


summary(som.cc,pars=c('betaTrt','betaYrs','betaYrsInv','betaTrtYrs','betaClay','betaSilt'))
print(som.cc,pars=c('betaTrt','betaYrs','betaYrsInv','betaTrtYrs','betaClay','betaSilt'))

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
