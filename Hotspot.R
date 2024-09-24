rm(list = ls())
options(scipen=999)


library(tidyverse)
library(sf)
library(sfdep)
library(spdep)
library(spatialEco)


data<-read.csv(file.choose()) #Strawn_PP.csv"

ggplot(data, aes(x=z)) + geom_histogram(binwidth=.05)
  
data %>%
  filter(z <100.4) %>%
  ggplot(., aes(x=z)) + geom_histogram(binwidth=.05)


data.ea <-data %>%
  filter(z <100.4)

plot(data$y~data$x)

data.shape<-st_as_sf(data, 
                     coords = c("x", "y"))

data.shape<- data.shape %>%
  mutate(TA = as.numeric(Thermal.Alteration))



poly<-st_read(file.choose()) #count.shp


tes_data<-poly %>%
  select(id, NUMPOINTS)%>%
  drop_na()

tes_nb <- poly2nb(tes_data, queen = TRUE)

tes_w_binary <- nb2listw(tes_nb, style="B")

tes_lag <- lag.listw(tes_w_binary, tes_data$NUMPOINTS)

globalG.test(tes_data$NUMPOINTS, tes_w_binary)


tes_nbs <- tes_data |> 
  mutate(
    nb = st_contiguity(geometry),        # neighbors share border/vertex
    wt = st_weights(nb),                 # row-standardized weights
    tes_lag = st_lag(NUMPOINTS, nb, wt)    # calculate spatial lag of TreEqty
  ) 

tes_hot_spots <- tes_nbs |> 
  mutate(
    Gi = local_g_perm(NUMPOINTS, nb, wt, nsim = 999)
    # nsim = number of Monte Carlo simulations (999 is default)
  ) |> 
  # The new 'Gi' column itself contains a dataframe 
  # We can't work with that, so we need to 'unnest' it
  unnest(Gi) 


figure.hotspot.gi<-tes_hot_spots |> 
  ggplot((aes(fill = gi))) +
  theme_void() +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_gradient(low = "blue", high = "red", na.value = NA)

figure.hotspot.gi

ggsave(figure.hotspot.gi, file="figure.hotspot.gi.png", dpi = 300)



figure.hotspot<-tes_hot_spots |> 
  # with the columns 'gi' and 'p_folded_sim"
  # 'p_folded_sim' is the p-value of a folded permutation test
  select(gi, p_folded_sim) |> 
  mutate(
    # Add a new column called "classification"
    classification = case_when(
      # Classify based on the following criteria:
      gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
      gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
      gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
      gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
      gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
      gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
      TRUE ~ "Insignificant"
    ),
    # Convert 'classification' into a factor for easier plotting
    classification = factor(
      classification,
      levels = c("Very hot", "Hot", "Somewhat hot",
                 "Insignificant",
                 "Somewhat cold", "Cold", "Very cold")
    )
  ) |> 
  # Visualize the results with ggplot2
  ggplot(aes(fill = classification)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  theme_void() +
  labs(
    fill = "Hot Spot Classification",
  )

figure.hotspot

ggsave(figure.hotspot, file="figure.hotspot.png", dpi = 300)
