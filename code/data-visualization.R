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
#spatial <- read_excel("raw-data/Coordinates of SHP sites_9_7_2019.xlsx")

#### Figure 1 ####
# inner_join(spatial, 
#            data.sub %>% 
#              select(field_name,TrtType) %>% 
#              unique()
# ) %>%
#   write.csv("synthesized-data/mapping_data.csv")


#### Figure 3 ####
plot.dw <- function(model.data,title){
  x <- tibble()
  for(i in 2015:2019){
    model.data[[i]] %>% broom.mixed::tidy() %>% filter(term == "CC") %>% mutate(model = i) -> temp
    x <- rbind(x,temp)
  }

  dotwhisker::dwplot(x) + theme_classic() + ggtitle(title) +
    scale_x_continuous(
      labels = scales::number_format(accuracy = 0.01)) +
    scale_color_manual(values=c("#00703c","#23487a","#90214a","#f3901d","#49a942"),
                       name="Year",
                       breaks=c("2019","2018","2017","2016","2015")) +
    xlab("\nCover crop effect") +
    geom_vline(xintercept=0,linetype="dotted") +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(size=11),
      axis.title.y = element_text(size=13),
      axis.title.x = element_text(size=13),
      legend.text = element_text(size = 11)
    )
}

plot.dw(ac.yr, "Active Carbon")
ggsave("figures/Figure 3/fig3_ac.tiff", width = 7, height = 7, units = "cm", dpi= "print")
plot.dw(resp.yr, "Respiration")
ggsave("figures/Figure 3/fig3_resp.tiff", width = 7, height = 7, units = "cm", dpi= "print")
plot.dw(as.yr, "Aggregate Stability")
ggsave("figures/Figure 3/fig3_as.tiff", width = 7, height = 7, units = "cm", dpi= "print")
plot.dw(som.yr, "Soil organic matter")
ggsave("figures/Figure 3/fig3_som.tiff", width = 7, height = 7, units = "cm", dpi= "print")

#### Figure 4 ####
# Labels for y-axis
pred.labs <- c(
  CC = "Cover crop\n(Yes/No)",
  `as.numeric(smpl_yr)` = "Year",
  yrsTrt = "Yrs. of cover crop",
  `as.numeric(soil_texture_clay)` = "Clay\n(%)",
  `as.numeric(soil_texture_silt)` = "Silt\n(%)",
  `CC:yrsTrt` = "Cover crop x\nYrs. of cover crop"
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
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  ggtitle("Aggregate stability") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=12))
ggsave("figures/Figure 4/fig4_as.tiff", width = 10, height = 8, units = "cm", dpi= "print")

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
        legend.position = "none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=12))
ggsave("figures/Figure 4/fig4_ac.tiff", width = 10, height = 8, units = "cm", dpi= "print")

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
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  ggtitle("Water holding capacity") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=12))
ggsave("figures/Figure 4/fig4_whc.tiff", width = 10, height = 8, units = "cm", dpi= "print")

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
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  ggtitle("ACE Soil Protein Index") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=12))
ggsave("figures/Figure 4/fig4_pro.tiff", width = 10, height = 8, units = "cm", dpi= "print")

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
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  ggtitle("Respiration") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=12))
ggsave("figures/Figure 4/fig4_resp.tiff", width = 10, height = 8, units = "cm", dpi= "print")

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
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  ggtitle("Soil organic matter") +
  scale_color_manual(values=c("#00703c")) +
  theme_classic() + 
  theme(plot.title = element_text(face="bold"),
        legend.position = "none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=12))
ggsave("figures/Figure 4/fig4_som.tiff", width = 10, height = 8, units = "cm", dpi= "print")


## Figure 2 ##
ggplot(final %>% filter(`Cover Crop`=="Yes"), aes(x=as.diff)) + 
  geom_density(color="#00703c", fill="#49a942", size=.7, alpha=0.5) + 
  geom_vline(xintercept=0, linetype="dotted", size=0.5) + 
  ylab("Density\n") + xlab("\nAggregate stability") + theme_classic()
ggsave("figures/Figure 2/fig2_as.tiff", width = 6, height = 6, units = "cm", dpi= "print")

ggplot(final %>% filter(`Cover Crop`=="Yes"), aes(x=ac.diff)) + 
  geom_density(color="#00703c", fill="#49a942", size=.7, alpha=0.5) + 
  geom_vline(xintercept=0, linetype="dotted", size=0.5) + 
  ylab("Density\n") + xlab("\nActive carbon") + theme_classic()
ggsave("figures/Figure 2/fig2_ac.tiff", width = 6, height = 6, units = "cm", dpi= "print")

ggplot(final %>% filter(`Cover Crop`=="Yes"), aes(x=pro.diff)) + 
  geom_density(color="#00703c", fill="#49a942", size=.7, alpha=0.5) + 
  geom_vline(xintercept=0, linetype="dotted", size=0.5) + 
  ylab("Density\n") + xlab("\nACE soil protein index") + theme_classic()
ggsave("figures/Figure 2/fig2_pro.tiff", width = 6, height = 6, units = "cm", dpi= "print")

ggplot(final %>% filter(`Cover Crop`=="Yes"), aes(x=resp.diff)) + 
  geom_density(color="#00703c", fill="#49a942", size=.7, alpha=0.5) + 
  geom_vline(xintercept=0, linetype="dotted", size=0.5) + 
  ylab("Density\n") + xlab("\nRespiration") + theme_classic()
ggsave("figures/Figure 2/fig2_resp.tiff", width = 6, height = 6, units = "cm", dpi= "print")

ggplot(final %>% filter(`Cover Crop`=="Yes"), aes(x=whc.diff)) + 
  geom_density(color="#00703c", fill="#49a942", size=.7, alpha=0.5) + 
  geom_vline(xintercept=0, linetype="dotted", size=0.5) + 
  ylab("Density\n") + xlab("Available water capacity") + theme_classic()
ggsave("figures/Figure 2/fig2_whc.tiff", width = 6, height = 6, units = "cm", dpi= "print")

ggplot(final %>% filter(`Cover Crop`=="Yes"), aes(x=som.diff)) + 
  geom_density(color="#00703c", fill="#49a942", size=.7, alpha=0.5) + 
  geom_vline(xintercept=0, linetype="dotted", size=0.5) + 
  ylab("Density\n") + xlab("Organic matter") + theme_classic()
ggsave("figures/Figure 2/fig2_som.tiff", width = 6, height = 6, units = "cm", dpi= "print")
