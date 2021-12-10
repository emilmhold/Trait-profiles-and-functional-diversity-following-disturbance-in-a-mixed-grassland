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

# 2020 rarefaction - code from https://rdrr.io/rforge/vegan/man/rarefy.html
S.2020 <- specnumber(community_2020) # observed number of species
raremax.2020 <- min(rowSums(community_2020))
Srare.2020 <- rarefy(community_2020, raremax.2020)
#plot
plot(S.2020, Srare.2020, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(community_2020, step = 20, sample = raremax.2020, col = "blue", cex = 0.6)

#Load 2018 community data
community_2018 <- read.csv("~/Documents/GitHub/Trait-profiles-and-functional-diversity-following-disturbance-in-a-mixed-grassland/Trait profiles and functional diversity following disturbance in a mixed grassland/ComProp2018.csv")
rownames(community_2018) <- community_2018$X
community_2018$X <- NULL
community_2018 <- as.data.frame(t(community_2018)) #transpose data
View(community_2018)

# 2018 rarefaction
S.2018 <- specnumber(community_2018) # observed number of species
raremax.2018 <- min(rowSums(community_2018))
Srare.2018 <- rarefy(community_2018, raremax.2018)
#plot
plot(S.2018, Srare.2018, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(community_2018, step = 20, sample = raremax.2018, col = "blue", cex = 0.6)
