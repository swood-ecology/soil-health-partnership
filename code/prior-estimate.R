library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn)


# Read in data
bc <- read_csv("raw-data/beyond_clay_midwest_subset.csv")

# Select the proper variables
bc <- bc %>%
  select(
    site_key:state_code,`hzn_top (cm)`:`Final SOC Data (%)`
  )

# Filter out samples that are deeper than 30 cm
bc <- bc %>%
  filter(
    `hzn_top (cm)` < 40
  )

# Difference between horizons
bc <- bc %>%
  mutate(
    hzn_size = `hzn_bot (cm)`-`hzn_top (cm)`
  )

# Aggregate with a weighted mean
library(data.table)
DT <- data.table(bc)
soc.agg <- DT[,list(wret = weighted.mean(`Final SOC Data (%)`,hzn_size)),by=pedon_key]
text.agg <- DT[,list(wret = weighted.mean(`clay_tot_psa (%)`,hzn_size)),by=pedon_key]
names(soc.agg)[2] <- "soc"
names(text.agg)[2] <- "text"

# Join data sets
bc.filt <- bc %>% dplyr::select(pedon_key,state_code) %>% unique()
bc.join <- full_join(soc.agg,text.agg) %>%
  full_join(bc.filt)
rm(bc.filt)

# Convert SOC to SOM
# Standardize predictor
bc.join <- bc.join %>%
  mutate(
    som = soc*1.8,
    s.text = (text - mean(text))/2*sd(text)
  )

# Fit model for prior estimate
lmer(som ~ s.text + (1|state_code), data=bc.join) %>%
  summary()
## Prior: 0.00347 (0.0004 = SD) 

  
