rm(list = ls())
options(scipen=999)


library(tidyverse)

spatial<- read.csv(file.choose()) #Strawn_PP.csv"
biface <- read.csv(file.choose()) #biface_join.csv
dates  <- read.csv(file.choose()) #dates.csv

spatial$Thermal.Alteration<-as.factor(spatial$Thermal.Alteration)

spatial<-spatial%>%
  filter(z<100.4) 

planview.figure<-ggplot() +
  geom_point(data = spatial, aes(x = x, y = y, color = Thermal.Alteration), size = 1) +
  geom_point(data = biface, aes(x = x, y = y, shape = Type), size = 3) +
  labs(title = "Planview",
       x = "Easting",
       y = "Northing") +
  scale_color_manual(values = c("TRUE" = "darkgray", "FALSE" = "lightgray")) +
  theme_classic() +
  labs(color = "Thermally Altered")

planview.figure

ggsave(planview.figure, file="planview.figure.png", dpi = 300)


spatial<-spatial%>%
  filter(z<100.4) %>%
  filter (y<548)

profile.figure<-ggplot() +
  geom_point(data = spatial, aes(x = x, y = z, color = Thermal.Alteration), size = 1) +
  geom_point(data = biface, aes(x = x, y = z, shape = Type), size = 3) +
  labs(title = "Profile View",
       x = "Easting",
       y = "Elevation (m)") +
  scale_color_manual(values = c("TRUE" = "darkgray", "FALSE" = "lightgray")) +
  theme_classic() +
  labs(color = "Thermally Altered")

profile.figure

ggsave(profile.figure, file="profile.figure.png", dpi = 300)


