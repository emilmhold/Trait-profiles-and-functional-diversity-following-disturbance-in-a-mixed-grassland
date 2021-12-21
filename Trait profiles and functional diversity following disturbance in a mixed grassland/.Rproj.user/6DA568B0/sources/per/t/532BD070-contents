setwd("~/Documents/GitHub/Trait-profiles-and-functional-diversity-following-disturbance-in-a-mixed-grassland/Trait profiles and functional diversity following disturbance in a mixed grassland")

install.packages("vegan")
library(vegan)
library(tidyverse)

#Load 2020 community data
community_2020 <- read.csv("~/Documents/GitHub/Trait-profiles-and-functional-diversity-following-disturbance-in-a-mixed-grassland/Trait profiles and functional diversity following disturbance in a mixed grassland/ComProp2020.csv")
rownames(community_2020) <- community_2020$Species
community_2020$Species <- NULL
community_2020 <- as.data.frame(t(community_2020)) #transpose data
View(community_2020)

#Default specaccum
sp1 <- specaccum(community_2020)

plot(sp1, xlab = "Plot", ylab = "Expected mean species richness")
