# Load packages
library(tidyverse)    # encodes ggplot2
library(soiltexture)  # for plotting texture triangles
library(readxl)
library(dwplot)       # For regression dot and whisker plots
library(dotwhisker)   # For regression dot and whisker plots
library(broom.mixed)
library(interactions) # For interaction plots

# Load data
load("~/Box Sync/Work/Code/shp/shp-data.RData")
load("~/Box Sync/Work/Code/shp/shp-models.RData")
spatial <- read_excel("raw-data/Coordinates of SHP sites_9_7_2019.xlsx")

# Figure 1
inner_join(spatial, 
           data.sub %>% 
             select(field_name,TrtType) %>% 
             unique()
           ) %>%
  write.csv("synthesized-data/mapping_data.csv")

# Figure 2
# Labels for y-axis
pred.labs <- c(
  `c.AMS:s.yrs` = "Treatment (1/0)\nx Years of treatment",
  c.Soy = "Soy (1/0)",
  c.Wheat = "Wheat (1/0)",
  s.clay = "Clay",
  s.silt = "Silt"
)

## Aggregate stability  
as.plot <- tidy(as.cc) %>%
  relabel_predictors(pred.labs) %>%
  slice(1:7) %>%
  filter(term != "(Intercept)",
         term != "sd__(Intercept)")

dwplot(as.plot,
       vline = geom_vline(xintercept = 0, colour = "#7e6a65", linetype = 2)
) + 
  xlab("\nCoefficient Estimate") + ylab("") +
  ggtitle("Aggregate stability") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none")

## Active Carbon  
ac.plot <- tidy(ac.cc) %>%
  relabel_predictors(pred.labs) %>%
  slice(1:7) %>%
  filter(term != "(Intercept)",
         term != "sd__(Intercept)")

dwplot(ac.plot,
       vline = geom_vline(xintercept = 0, colour = "#7e6a65", linetype = 2)
) + 
  xlab("\nCoefficient Estimate") + ylab("") +
  ggtitle("Active carbon") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none")

## Water holding capacity 
whc.plot <- tidy(whc.cc) %>%
  relabel_predictors(pred.labs) %>%
  slice(1:7) %>%
  filter(term != "(Intercept)",
         term != "sd__(Intercept)")

dwplot(whc.plot,
       vline = geom_vline(xintercept = 0, colour = "#7e6a65", linetype = 2)
) + 
  xlab("\nCoefficient Estimate") + ylab("") +
  ggtitle("Water holding capacity") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none")

# Figure 3
interact_plot(as.cc.inter,pred=YrTrtFINAL,modx=treat_final_AMS,
              interval=TRUE,width=0.8,
              partial.residuals=TRUE,jitter=0.3,colors=c("#00703c","#f3901d"),
              x.label = "\nYears of treatment", y.label = "Partial Residuals\n",
              main.title = "Aggregate stability",  legend.main = "",modx.labels=c("Control","Treatment")) +
  theme_classic()

interact_plot(ac.cc.inter,pred=YrTrtFINAL,modx=treat_final_AMS,
              interval=TRUE,width=0.8,
              partial.residuals=TRUE,jitter=0.3,colors=c("#00703c","#f3901d"),
              x.label = "\nYears of treatment", y.label = "Partial Residuals\n",
              main.title = "Active carbon",  legend.main = "",modx.labels=c("Control","Treatment")) +
  theme_classic()

interact_plot(whc.cc.inter,pred=YrTrtFINAL,modx=treat_final_AMS,
              interval=TRUE,width=0.8,
              partial.residuals=TRUE,jitter=0.3,colors=c("#00703c","#f3901d"),
              x.label = "\nYears of treatment", y.label = "Partial Residuals\n",
              main.title = "Water holding capacity",  legend.main = "",modx.labels=c("Control","Treatment")) +
  theme_classic()

# Figure SX
## Respiration  
resp.plot <- tidy(resp.cc) %>%
  relabel_predictors(pred.labs) %>%
  slice(1:7) %>%
  filter(term != "(Intercept)",
         term != "sd__(Intercept)")

dwplot(resp.plot,
       vline = geom_vline(xintercept = 0, colour = "#7e6a65", linetype = 2)
) + 
  xlab("\nCoefficient Estimate") + ylab("") +
  ggtitle("Respiration") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none")

## Protein 
pro.plot <- tidy(pro.cc) %>%
  relabel_predictors(pred.labs) %>%
  slice(1:7) %>%
  filter(term != "(Intercept)",
         term != "sd__(Intercept)")

dwplot(pro.plot,
       vline = geom_vline(xintercept = 0, colour = "#7e6a65", linetype = 2)
) + 
  xlab("\nCoefficient Estimate") + ylab("") +
  ggtitle("Soil Protein Index") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none")


#################### NOT USED ##########################

# Figure 3
data.sub.cc <- data.sub %>%
  filter(TrtType == "Cover" | TrtType == "Cover/TILL") %>%
  group_by(SHPID_Strip)

data.diff <- data.sub.cc %>%
  mutate(
    year.min = min(year),
    year.max = max(year)
  ) %>%
  filter(
    year == year.min | year == year.max
  ) %>%
  mutate(
    som.diff = (SOMnew - lag(SOMnew))/lag(SOMnew),
    resp.diff = (Respiration - lag(Respiration))/lag(Respiration),
    activeC.diff = (ActiveCarbon - lag(ActiveCarbon))/lag(ActiveCarbon),
    aggstab.diff = (AggStab - lag(AggStab))/lag(AggStab)
  ) %>%
  filter(
    !is.na(som.diff)
  )

ggplot(data.diff, aes(x=lagCrop,y=resp.diff)) + 
  geom_violin(trim=FALSE) + 
  geom_jitter(position=position_jitter(0.2)) +
  coord_flip() +
  xlab("") +
  ylab("\nRespiration (%)")


ggplot(data.sub.cc %>% filter(SHPID_Strip != 'SHP2017IA005_2'), aes(x=Clay,y=Respiration)) + 
  geom_point() + 
  geom_jitter(position=position_jitter(0.2)) +
  coord_flip() +
  xlab("") +
  ylab("\nRespiration (%)")



# Overlapping histograms
colors <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628",
            "#f781bf","#999999")

# SOM
som.yearmax %>%
  ggplot(aes(SOMnew, fill=State, color=State)) +
  geom_density(alpha=0.25) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  ylab("Density\n") +
  xlab("Soil organic matter") +
  theme_minimal()

# Treatment type
som.yearmax %>%
  ggplot(aes(TrtType)) +
  geom_histogram(stat="count",alpha=0.75) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 25)) +
  ylab("Count\n") +
  xlab("\nTreatment type") +
  theme_minimal()

# Soil texture diagrams
## Generate function for plot where you can pass state to filter
tt.plot <- function(data,state){
  # Filter the data to the chosen state and for the relevant variables
  text <- data %>%
    filter(State==state) %>%
    select("SAND"=Sand,
           "SILT"=Silt,
           "CLAY"=Clay,
           SOMnew) %>%
    drop_na() %>%
    as.data.frame()
  
  # Generate the plot
  plot <- TT.plot(class.sys = "USDA-NCSS.TT",
                  class.p.bg.col = TRUE,
          tri.data=text,
          pch=20,
          z.name="SOMnew",
          main=state
  )
  
  rm(text)
  return(plot)
}

## Illinois
tt.plot(som.yearmax, "Illinois")
## Indiana
tt.plot(som.yearmax, "Indiana")
## Iowa
tt.plot(som.yearmax, "Iowa")
## Minnesota
tt.plot(som.yearmax, "Minnesota")
## Missouri
tt.plot(som.yearmax, "Missouri")
## Nebraska
tt.plot(som.yearmax, "Nebraska")
## Ohio
tt.plot(som.yearmax, "Ohio")
