library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(readr)
library(stringr)
library(clipr)
library(lubridate)
library(reshape2)
library(vegan)
library(ggplot2)
library(ggvegan)
library(ggpubr)
# 
# install.packages("remotes")
# remotes::install_github("gavinsimpson/ggvegan")

CRAM_envData <- read_csv("CRAM_envData.csv")

TaxizedPlants3_11 <- read_csv("TaxizedPlants3-11.csv")


CRAM_Sites_Species <- TaxizedPlants3_11 %>% 
  select(ecramid, Final_ID)

SpeciesEnv <- TaxizedPlants3_11 %>% 
  select(Final_ID, invasive) %>% 
  distinct()

test <- table(CRAM_Sites_Species[1:2])

test <- as.data.frame(test)

# test <- test %>% 
#   mutate(PresAbs = (Freq / Freq))
# 
# test <- test %>% 
#   replace(is.na(.), 0)

SpeciesMatrix <- test %>%
  pivot_wider(
    id_cols = ecramid,
    names_from = Final_ID,
    values_from = Freq,
    values_fn = sum,
    values_fill = 0
  )


SpeciesMatrix <- SpeciesMatrix %>% 
  select(-ecramid)

Veg.hel <- decostand(SpeciesMatrix, method = "pa")

nmds1 <- metaMDS(Veg.hel, autotransform = F,trymax=100)


ordiplot(nmds1, type = "t") 


autoplot(nmds1)

fort <- fortify(nmds1)

ggplot()+ 
  geom_point(data = subset(fort, score == "sites"),
             mapping = aes(x = NMDS1, y = NMDS2),
             color = "black",
             alpha = 0.5)+
  geom_segment(data = subset(fort, score == "species"),
                             mapping = aes (x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                             arrow = arrow(length = unit(0.015, "npc"),
                                           type = "closed"),
                             color = "darkgray",
                             size = 0.8)+
  geom_text(data = subset(fort, score == 'species'),
            mapping = aes(label = label, x = NMDS1 *1.1, y = NMDS2 *1.1))+
  geom_abline(intercept = 0, slope = 0, linetype="dashed", size = 0.8, color = "gray")+
  geom_vline(aes(xintercept = 0), linetype = "dashed",  size = 0.8, color = "gray")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))


p1 <- ggplot()+ 
  geom_point(data = subset(fort, score == "sites"),
             mapping = aes(x = NMDS1, y = NMDS2),
             color = "black",
             alpha = 0.5)+
  geom_segment(data = subset(fort, score == "species"),
               mapping = aes (x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.015, "npc"),
                             type = "closed"),
               color = "darkgray",
               size = 0,
               alpha = 0)+
  geom_text(data = subset(fort, score == 'species'),
            mapping = aes(label = label, x = NMDS1 *1.1, y = NMDS2 *1.1),
            alpha = 0)+
  geom_abline(intercept = 0, slope = 0, linetype="dashed", size = 0.8, color = "gray")+
  geom_vline(aes(xintercept = 0), linetype = "dashed",  size = 0.8, color = "gray")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
p1


p2 <- ggplot()+ 
  geom_point(data = subset(fort, score == "sites"),
             mapping = aes(x = NMDS1, y = NMDS2),
             color = "black",
             alpha = 0)+
  geom_segment(data = subset(fort, score == "species"),
               mapping = aes (x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.015, "npc"),
                             type = "closed"),
               color = "darkgray",
               size = 0.8)+
  geom_text(data = subset(fort, score == 'species'),
            mapping = aes(label = label, x = NMDS1 *1.1, y = NMDS2 *1.1))+
  geom_abline(intercept = 0, slope = 0, linetype="dashed", size = 0.8, color = "gray")+
  geom_vline(aes(xintercept = 0), linetype = "dashed",  size = 0.8, color = "gray")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))

p2


ggarrange(p1, p2, ncol = 1)

summary(CRAM_envData)


x <- adonis2(Veg.hel~DaysSinceFire*FireDist, data = CRAM_envData)

z <- adonis2(Veg.hel~DaysSinceFire*FireDist*Elevation*FireSize, data = CRAM_envData)

y <- adonis2(Veg.hel~FireDist, data = CRAM_envData)


p3 <-  ggplot()+ 
  geom_point(data = subset(fort, score == "sites"),
             mapping = aes(x = NMDS1, y = NMDS2, color = CRAM_envData$PrePost ),
             alpha = 0.5)+
  geom_segment(data = subset(fort, score == "species"),
               mapping = aes (x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.015, "npc"),
                             type = "closed"),
               color = "darkgray",
               size = 0,
               alpha = 0)+
  geom_text(data = subset(fort, score == 'species'),
            mapping = aes(label = label, x = NMDS1 *1.1, y = NMDS2 *1.1),
            alpha = 0)+
  geom_abline(intercept = 0, slope = 0, linetype="dashed", size = 0.8, color = "gray")+
  geom_vline(aes(xintercept = 0), linetype = "dashed",  size = 0.8, color = "gray")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))+
  guides(color = guide_legend(title = "Pre or Post Fire")) 
p3


p4 <- ggplot()+ 
  geom_point(data = subset(fort, score == "sites"),
             mapping = aes(x = NMDS1, y = NMDS2),
             color = "black",
             alpha = 0)+
  geom_segment(data = subset(fort, score == "species"),
               mapping = aes (x = 0, y = 0, xend = NMDS1, yend = NMDS2,color = SpeciesEnv$invasive),
               arrow = arrow(length = unit(0.015, "npc"),
                             type = "closed"),
               size = 0.8)+
  geom_text(data = subset(fort, score == 'species'),
            mapping = aes(label = label, x = NMDS1 *1.1, y = NMDS2 *1.1, color = SpeciesEnv$invasive))+
  geom_abline(intercept = 0, slope = 0, linetype="dashed", size = 0.8, color = "gray")+
  geom_vline(aes(xintercept = 0), linetype = "dashed",  size = 0.8, color = "gray")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
p4



ggarrange(p3, p4, ncol = 1)


# jpeg("streamSites.jpg", width = 150, height = 250, units = "mm", res = 600)
# ggarrange(p3, p4, ncol = 1)
# dev.off()


ggplot()+
  geom_point(data = CRAM_envData, aes(x = DaysSinceFire, y = FireDist, color = PrePost))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
