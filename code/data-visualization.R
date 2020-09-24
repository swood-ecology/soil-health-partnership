#### Load packages ####
library(tidyverse)    # encodes ggplot2
library(soiltexture)  # for plotting texture triangles
library(readxl)       # For reading .xls
library(dwplot)       # For regression dot and whisker plots
library(dotwhisker)   # For regression dot and whisker plots
library(broom.mixed)
library(interactions) # For interaction plots

library(sjPlot)
library(sjmisc)


#### Load data ####
load("~/Box Sync/Work/Code/shp/shp-data.RData")
load("~/Box Sync/Work/Code/shp/shp-models.RData")
spatial <- read_excel("raw-data/Coordinates of SHP sites_9_7_2019.xlsx")

#### Figure 1 ####
inner_join(spatial, 
           data.sub %>% 
             select(field_name,TrtType) %>% 
             unique()
) %>%
  write.csv("synthesized-data/mapping_data.csv")


#### Figure 2 ####
plot.dw <- function(model.data,title){
  x <- tibble()
  for(i in 2015:2019){
    model.data[[i]] %>% broom.mixed::tidy() %>% filter(term == "CC") %>% mutate(model = i) -> temp
    x <- rbind(x,temp)
  }

  dotwhisker::dwplot(x) + theme_classic() + ggtitle(title) +
    scale_color_manual(values=c("#00703c","#23487a","#90214a","#f3901d","#49a942"),
                       name="Year",
                       breaks=c("2019","2018","2017","2016","2015")) +
    xlab("\nCover crop effect") +
    geom_vline(xintercept=0,linetype="dotted") +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(size=12),
      axis.title.y = element_text(size=13),
      axis.title.x = element_text(size=13),
      legend.text = element_text(size = 11)
    )
}

plot.dw(ac.yr, "Active Carbon")
ggsave("figures/Figure 2/fig2_ac.tiff", width = 10, height = 10, units = "cm", dpi= "print")
plot.dw(resp.yr, "Respiration")
ggsave("figures/Figure 2/fig2_resp.tiff", width = 10, height = 10, units = "cm", dpi= "print")
plot.dw(as.yr, "Aggregate Stability")
ggsave("figures/Figure 2/fig2_as.tiff", width = 10, height = 10, units = "cm", dpi= "print")
plot.dw(som.yr, "Soil organic matter")
ggsave("figures/Figure 2/fig2_som.tiff", width = 10, height = 10, units = "cm", dpi= "print")

#### Figure 3 ####
# Labels for y-axis
pred.labs <- c(
  CC = "Cover crop\n(Yes/No)",
  `as.numeric(smpl_yr)` = "Year",
  yrsTrt = "Years of cover crops",
  `as.numeric(soil_texture_clay)` = "Clay\n(%)",
  `as.numeric(soil_texture_silt)` = "Silt\n(%)",
  `CC:yrsTrt` = "Cover crop x\nYears of cover crops"
)

#### Aggregate stability ####
as.plot <- tidy(as) %>%
  relabel_predictors(pred.labs) %>%
  slice(1:7) %>%
  filter(term != "(Intercept)",
         term != "sd__(Intercept)")

dwplot(as.plot,
       vline = geom_vline(xintercept = 0, colour = "#7e6a65", linetype = 2)
) + 
  xlab("\nCoefficient estimate") + ylab("") +
  ggtitle("Aggregate stability") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none")
ggsave("figures/Figure S4/figs4_as.tiff", width = 15, height = 10, units = "cm", dpi= "print")

#### Active Carbon ####
ac.plot <- tidy(ac) %>%
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
ggsave("figures/Figure S4/figs4_ac.tiff", width = 15, height = 10, units = "cm", dpi= "print")

#### Water holding capacity ####
whc.plot <- tidy(whc) %>%
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
ggsave("figures/Figure S4/figs4_whc.tiff", width = 15, height = 10, units = "cm", dpi= "print")

#### Protein ####
pro.plot <- tidy(pro) %>%
  relabel_predictors(pred.labs) %>%
  slice(1:7) %>%
  filter(term != "(Intercept)",
         term != "sd__(Intercept)")

dwplot(pro.plot,
       vline = geom_vline(xintercept = 0, colour = "#7e6a65", linetype = 2)
) + 
  xlab("\nCoefficient Estimate") + ylab("") +
  ggtitle("ACE Soil Protein Index") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none")
ggsave("figures/Figure S4/figs4_pro.tiff", width = 15, height = 10, units = "cm", dpi= "print")

#### Respiration ####
resp.plot <- tidy(resp) %>%
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
ggsave("figures/Figure S4/figs4_resp.tiff", width = 15, height = 10, units = "cm", dpi= "print")

#### Soil organic matter ####
som.plot <- tidy(som) %>%
  relabel_predictors(pred.labs) %>%
  slice(1:7) %>%
  filter(term != "(Intercept)",
         term != "sd__(Intercept)")

dwplot(som.plot,
       vline = geom_vline(xintercept = 0, colour = "#7e6a65", linetype = 2)
) + 
  xlab("\nCoefficient Estimate") + ylab("") +
  ggtitle("Soil organic matter") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none")
ggsave("figures/Figure S4/figs4_som.tiff", width = 15, height = 10, units = "cm", dpi= "print")


## Figure 4 ##
ggplot(final %>% filter(`Cover Crop`=="Yes"), aes(x=as.diff)) + 
  geom_density(color="#00703c", fill="#49a942", size=.7, alpha=0.5) + 
  geom_vline(xintercept=0, linetype="dotted", size=0.5) + 
  ylab("Density\n") + xlab("\nAggregate stability") + theme_classic()
ggsave("figures/Figure 4/fig4_as.tiff", width = 15, height = 10, units = "cm", dpi= "print")

ggplot(final %>% filter(`Cover Crop`=="Yes"), aes(x=ac.diff)) + 
  geom_density(color="#00703c", fill="#49a942", size=.7, alpha=0.5) + 
  geom_vline(xintercept=0, linetype="dotted", size=0.5) + 
  ylab("Density\n") + xlab("\nActive carbon") + theme_classic()
ggsave("figures/Figure 4/fig4_ac.tiff", width = 15, height = 10, units = "cm", dpi= "print")

ggplot(final %>% filter(`Cover Crop`=="Yes"), aes(x=pro.diff)) + 
  geom_density(color="#00703c", fill="#49a942", size=.7, alpha=0.5) + 
  geom_vline(xintercept=0, linetype="dotted", size=0.5) + 
  ylab("Density\n") + xlab("\nACE soil protein index") + theme_classic()
ggsave("figures/Figure 4/fig4_pro.tiff", width = 15, height = 10, units = "cm", dpi= "print")

ggplot(final %>% filter(`Cover Crop`=="Yes"), aes(x=resp.diff)) + 
  geom_density(color="#00703c", fill="#49a942", size=.7, alpha=0.5) + 
  geom_vline(xintercept=0, linetype="dotted", size=0.5) + 
  ylab("Density\n") + xlab("\nRespiration") + theme_classic()
ggsave("figures/Figure 4/fig4_resp.tiff", width = 15, height = 10, units = "cm", dpi= "print")

ggplot(final %>% filter(`Cover Crop`=="Yes"), aes(x=whc.diff)) + 
  geom_density(color="#00703c", fill="#49a942", size=.7, alpha=0.5) + 
  geom_vline(xintercept=0, linetype="dotted", size=0.5) + 
  ylab("Density\n") + xlab("Available water capacity") + theme_classic()
ggsave("figures/Figure 4/fig4_whc.tiff", width = 15, height = 10, units = "cm", dpi= "print")

ggplot(final %>% filter(`Cover Crop`=="Yes"), aes(x=som.diff)) + 
  geom_density(color="#00703c", fill="#49a942", size=.7, alpha=0.5) + 
  geom_vline(xintercept=0, linetype="dotted", size=0.5) + 
  ylab("Density\n") + xlab("Organic matter") + theme_classic()
ggsave("figures/Figure 4/fig4_som.tiff", width = 15, height = 10, units = "cm", dpi= "print")


## Figure 5 ##

interactions::interact_plot(ac, pred=yrsTrt, modx=`Cover Crop`,x.label = "\nYears in the Soil Health Partnership", 
                            y.label = "Active Carbon\n", colors = 'Set1', partial.residuals=T) + theme_classic()
ggsave("figures/Figure 4/fig4_ac.tiff", width = 15, height = 10, units = "cm", dpi= "print")
interactions::interact_plot(as, pred=yrsTrt, modx=`Cover Crop`,x.label = "\nYears in the Soil Health Partnership", 
                            y.label = "Organic matter\n", colors = 'Set1') + theme_classic()
interactions::interact_plot(pro, pred=yrsTrt, modx=`Cover Crop`,x.label = "\nYears in the Soil Health Partnership", 
                            y.label = "Organic matter\n", colors = 'Set1') + theme_classic()
interactions::interact_plot(som, pred=yrsTrt, modx=`Cover Crop`,x.label = "\nYears in the Soil Health Partnership", 
                            y.label = "Organic matter\n", colors = 'Set1') + theme_classic()



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
