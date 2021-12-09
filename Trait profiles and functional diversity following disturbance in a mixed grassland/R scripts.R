install.packages(c("tidyverse", "dslabs", "vegan","ggplot2", "readxl"))

library("tidyverse")
library("dslabs")
library("vegan")
library("ggplot2")

####2020 & 2018 Evenness####
### importing data for evenness

ecommunity_2020 <- read.csv("~/Documents/GitHub/Trait-profiles-and-functional-diversity-following-disturbance-in-a-mixed-grassland/Trait profiles and functional diversity following disturbance in a mixed grassland/ComProp2020_Transposed.csv")

### taking evenness (from https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf)

ecommunity_2020 <- ecommunity_2020[, 2:64]

H <- diversity(ecommunity_2020)
J <- H/log(specnumber(ecommunity_2020))

### separating ANR and NNR2 evenness

ANR_Evenness2020<- J[seq(1, length(J), 2)]
NNR2_Evenness2020 <- J[seq(2, length(J), 2)]
print(NNR2_Evenness2020)

### making an anr evenness data frame

ANR_Evenness_2020Frame <- data.frame(Evenness = ANR_Evenness2020, 
                                     Subplot = c("ANR_2", "ANR_3", "ANR_4", "ANR_5", "ANR_6", "ANR_7", "ANR_11", "ANR_12", "ANR_14", "ANR_15", "ANR_16", "ANR_17"))

### taking average and standard deviation for anr evenness

Avg_ANR_E2020 <- mean(ANR_Evenness2020)

StDev_ANR_E2020 <- sd(ANR_Evenness2020)


### making a nnr2 evenness data frame

NNR2_Evenness2020Frame <- data.frame(Evenness = NNR2_Evenness2020,
                                    Subplot = c("NNR2_2", "NNR2_3", "NNR2_4", "NNR2_5", "NNR2_6", "NNR2_7", "NNR2_11", "NNR2_12", "NNR2_14", "NNR2_15", "NNR2_16", "NNR2_17"))

### taking average and standard deviation for nnr2 evenness

Avg_NNR2_E2020 <- mean(NNR2_Evenness2020)
StDev_NNR2_E2020 <- sd(NNR2_Evenness2020)


### importing august 2018 nnr2 community proportions data
ecommunity_2018 <- read.csv("~/Documents/GitHub/Trait-profiles-and-functional-diversity-following-disturbance-in-a-mixed-grassland/Trait profiles and functional diversity following disturbance in a mixed grassland/ComProp2018_Transposed.csv")


### taking evenness (from https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf)

ecommunity_2018 <- ecommunity_2018[, 2:40]

H_2 <- diversity(ecommunity_2018)
NNR2_Evenness2018 <- H_2/log(specnumber(ecommunity_2018))

### making a nnr2 evenness data frame

NNR2_Evenness2018Frame <- data.frame(Evenness = NNR2_Evenness2018,
                                    Subplot = c("NNR2_2", "NNR2_3", "NNR2_4", "NNR2_5", "NNR2_6", "NNR2_7", "NNR2_11", "NNR2_12", "NNR2_14", "NNR2_15", "NNR2_16", "NNR2_17"))

### taking average and standard deviation for nnr2 evenness

Avg_NNR2_E2018 <- mean(NNR2_Evenness2018)

StDev_NNR2_E2018 <- sd(NNR2_Evenness2018)


### making an average eveness data frame

Avg_Evenness <- data.frame(Evenness = c(Avg_ANR_E2020, Avg_NNR2_E2020, 0, Avg_NNR2_E2018), 
                           Plot = c("Disturbed", "Undisturbed", "Disturbed", "Undisturbed"), 
                           StDev= c(StDev_ANR_E2020, StDev_NNR2_E2020, 0, StDev_NNR2_E2018), 
                           StError = c(StDev_ANR_E2020/sqrt(12), StDev_NNR2_E2020/sqrt(12), 0, StDev_NNR2_E2018/sqrt(12)), 
                           Year=(c("2020", "2020", "2018" ,"2018")))
print(Avg_Evenness)
### graphing evenness

Evenness.graph <- ggplot(Avg_Evenness, aes(x= Year, y= Evenness, fill = Plot)) +
  geom_bar(stat = "identity", width=0.9, position=position_dodge()) +
  geom_errorbar(aes(x=Year, ymin=Evenness-StError, ymax=Evenness+StError), width=0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray28", "gray")) +
  scale_y_continuous() +
  labs(x = "Plot by year", y = "Average evenness") +
  theme_classic()+ 
  theme(axis.text = element_text(color = "black"), legend.position = "none")

####2020 & 2018 Richness####
### importing data for richness

community_2020 <- read.csv("~/Documents/GitHub/Trait-profiles-and-functional-diversity-following-disturbance-in-a-mixed-grassland/Trait profiles and functional diversity following disturbance in a mixed grassland/ComProp2020.csv")

###extract richness for all plots

community_2020$Treatment <- NULL
apply(community_2020, 2, specnumber)
totalplotrich <- data.frame(apply(community_2020, 2, specnumber))

###extract richness for each anr subplot

ANR2_R <- specnumber(community_2020$ANR..Dist..2)
ANR3_R <- specnumber(community_2020$ANR..Dist..3)
ANR4_R <- specnumber(community_2020$ANR..Dist..4)
ANR5_R <- specnumber(community_2020$ANR..Dist..5)
ANR6_R <- specnumber(community_2020$ANR..Dist..6)
ANR7_R <- specnumber(community_2020$ANR..Dist..7)
ANR11_R <- specnumber(community_2020$ANR..Dist..11)
ANR12_R <- specnumber(community_2020$ANR..Dist..12)
ANR14_R <- specnumber(community_2020$ANR..Dist..14)
ANR15_R <- specnumber(community_2020$ANR..Dist..15)
ANR16_R <- specnumber(community_2020$ANR..Dist..16)
ANR17_R <- specnumber(community_2020$ANR..Dist..17)

### making a anr richness data frame

ANR_Richness_2020 <- data.frame(Richness = c(ANR2_R, ANR3_R, ANR4_R, ANR5_R, ANR6_R, ANR7_R, ANR11_R, ANR12_R, ANR14_R, ANR15_R, ANR16_R, ANR17_R), 
                                Subplot = c("ANR_2", "ANR_3", "ANR_4", "ANR_5", "ANR_6", "ANR_7", "ANR_11", "ANR_12", "ANR_14", "ANR_15", "ANR_16", "ANR_17"))

### taking average and standard deviation for anr richness

Avg_ANR_R2020 <- mean(c(ANR2_R, ANR3_R, ANR4_R, ANR5_R, ANR6_R, ANR7_R, ANR11_R, ANR12_R, ANR14_R, ANR15_R, ANR16_R, ANR17_R))

StDev_ANR_R2020 <- sd(c(ANR2_R, ANR3_R, ANR4_R, ANR5_R, ANR6_R, ANR7_R, ANR11_R, ANR12_R, ANR14_R, ANR15_R, ANR16_R, ANR17_R))

### trying to extract richness for each nnr2 subplot

NNR2_R <- specnumber(community_2020$NNR2..Undist..2)
NNR3_R <- specnumber(community_2020$NNR2..Undist..3)
NNR4_R <- specnumber(community_2020$NNR2..Undist..4)
NNR5_R <- specnumber(community_2020$NNR2..Undist..5)
NNR6_R <- specnumber(community_2020$NNR2..Undist..6)
NNR7_R <- specnumber(community_2020$NNR2..Undist..7)
NNR11_R <- specnumber(community_2020$NNR2..Undist..11)
NNR12_R <- specnumber(community_2020$NNR2..Undist..12)
NNR14_R <- specnumber(community_2020$NNR2..Undist..14)
NNR15_R <- specnumber(community_2020$NNR2..Undist..15)
NNR16_R <- specnumber(community_2020$NNR2..Undist..16)
NNR17_R <- specnumber(community_2020$NNR2..Undist..17)

### making a nnr2 richness data frame

NNR2_Richness_2020 <- data.frame(Richness = c(NNR2_R, NNR3_R, NNR4_R, NNR5_R, NNR6_R, NNR7_R, NNR11_R, NNR12_R, NNR14_R, NNR15_R, NNR16_R, NNR17_R), 
                                 Subplot = c("NNR2_2", "NNR2_3", "NNR2_4", "NNR2_5", "NNR2_6", "NNR2_7", "NNR2_11", "NNR2_12", "NNR2_14", "NNR2_15", "NNR2_16", "NNR2_17"))

### taking average and standard deviation for nnr2 richness

Avg_NNR2_R2020 <- mean(c(NNR2_R, NNR3_R, NNR4_R, NNR5_R, NNR6_R, NNR7_R, NNR11_R, NNR12_R, NNR14_R, NNR15_R, NNR16_R, NNR17_R))

StDev_NNR2_R2020 <- sd(c(NNR2_R, NNR3_R, NNR4_R, NNR5_R, NNR6_R, NNR7_R, NNR11_R, NNR12_R, NNR14_R, NNR15_R, NNR16_R, NNR17_R))


# importing august 2018 nnr2 community proportions data
community_2018 <- read.csv("~/Documents/GitHub/Trait-profiles-and-functional-diversity-following-disturbance-in-a-mixed-grassland/Trait profiles and functional diversity following disturbance in a mixed grassland/ComProp2018.csv")

# taking richness for each nnr2 subplot

NNR2_R2018 <- specnumber(community_2018$NNR2.2)
NNR3_R2018 <- specnumber(community_2018$NNR2.3)
NNR4_R2018 <- specnumber(community_2018$NNR2.4)
NNR5_R2018 <- specnumber(community_2018$NNR2.5)
NNR6_R2018 <- specnumber(community_2018$NNR2.6)
NNR7_R2018 <- specnumber(community_2018$NNR2.7)
NNR11_R2018 <- specnumber(community_2018$NNR2.11)
NNR12_R2018 <- specnumber(community_2018$NNR2.12)
NNR14_R2018 <- specnumber(community_2018$NNR2.14)
NNR15_R2018 <- specnumber(community_2018$NNR2.15)
NNR16_R2018 <- specnumber(community_2018$NNR2.16)
NNR17_R2018 <- specnumber(community_2018$NNR2.17)

### making a nnr2 richness data frame

NNR2_Richness_2018 <- data.frame(Richness = c(NNR2_R2018, NNR3_R2018, NNR4_R2018, NNR5_R2018, NNR6_R2018, NNR7_R2018, NNR11_R2018, NNR12_R2018, NNR14_R2018, NNR15_R2018, NNR16_R2018, NNR17_R2018), 
                                 Subplot = c("NNR2_2", "NNR2_3", "NNR2_4", "NNR2_5", "NNR2_6", "NNR2_7", "NNR2_11", "NNR2_12", "NNR2_14", "NNR2_15", "NNR2_16", "NNR2_17"))

### taking average and standard deviation for nnr2 richness

Avg_NNR2_R2018 <- mean(c(NNR2_R2018, NNR3_R2018, NNR4_R2018, NNR5_R2018, NNR6_R2018, NNR7_R2018, NNR11_R2018, NNR12_R2018, NNR14_R2018, NNR15_R2018, NNR16_R2018, NNR17_R2018))

StDev_NNR2_R2018 <- sd(c(NNR2_R2018, NNR3_R2018, NNR4_R2018, NNR5_R2018, NNR6_R2018, NNR7_R2018, NNR11_R2018, NNR12_R2018, NNR14_R2018, NNR15_R2018, NNR16_R2018, NNR17_R2018))


### making an average richness data frame

Avg_Richness <- data.frame(Richness = c(Avg_ANR_R2020, Avg_NNR2_R2020, 0, Avg_NNR2_R2018), 
                           Plot = c("Disturbed", "Undisturbed", "Disturbed", "Undisturbed"), 
                           StDev= c(StDev_ANR_R2020, StDev_NNR2_R2020, 0, StDev_NNR2_R2018),
                           StError = c(StDev_ANR_R2020/sqrt(12), StDev_NNR2_R2020/sqrt(12), 0, StDev_NNR2_R2018/sqrt(12)), 
                           Year=(c("2020", "2020", "2018" ,"2018")))

print(Avg_Richness)

### graphing average richness

Richness.graph <- ggplot(Avg_Richness, aes(x= Year, y= Richness, fill = Plot)) +
  geom_bar(stat = "identity", width=0.9, position=position_dodge()) +
  geom_errorbar(aes(x=Year, ymin=Richness-StError, ymax=Richness+StError), width=0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray28", "gray")) +
  scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20)) +
  labs(x = "Plot by year", y = "Average richness") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"))

install.packages(cowplot)
library(cowplot)
ggdraw() +
  draw_plot(Evenness.graph, x = 0, y = 0, width = .4, height = 1) +
  draw_plot(Richness.graph, x = 0.4, y = 0, width = .6, height = 1) 

####FUNCTIONAL DIVERSITY ####
library(FD)

is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))}

####2018 Data prep####
# a) Import trait information
data_traits <- read.csv("~/Documents/GitHub/Trait-profiles-and-functional-diversity-following-disturbance-in-a-mixed-grassland/Trait profiles and functional diversity following disturbance in a mixed grassland/Traits_Raw.csv",na.strings=c("NA",""))
data_traits[,c("SpecimenID", "SppCode", "Collector", "Date.collected", "MicrositeID", "flag", "Notes")] <- NULL # Remove metadata I don't need
data_traits[,c("Family","Genus","Species")] <- NULL # Remove taxonomy data I don't need (saved separately anyway)
data_traits[,c("CN.sampleID", "Soil.pH", "pH.Temp", "pH.SoilWeight")] <- NULL # Remove?? extra data that's not really a trait
data_traits[,c("X","X.1","X.2","X.3","X.4","X.5","X.6")] <- NULL

data_traits$Vegetative.Height <- as.numeric(as.character(data_traits$Vegetative.Height))

# Calculate means for leaf area, leaf biomass, and SLA.Leaf
data_traits <- data_traits %>%
  dplyr::mutate(SA.Leaf = rowMeans(dplyr::select(., "SA.Leaf1", "SA.Leaf2", "SA.Leaf3"), na.rm=TRUE),
                .after=SA.Leaf3) %>%
  dplyr::mutate(Biomass.Leaf = rowMeans(dplyr::select(., "Biomass.Leaf1", "Biomass.Leaf2", "Biomass.Leaf3"), na.rm=TRUE),
                .after=Biomass.Leaf3) %>%
  dplyr::mutate(SLA.Leaf = rowMeans(dplyr::select(., "SLA.Leaf1", "SLA.Leaf2", "SLA.Leaf3"), na.rm=TRUE),
                .after=SLA.Leaf3) %>%
  tibble::column_to_rownames("ShortID") %>%
  as.data.frame()

data_traits[,c("SA.Leaf1", "SA.Leaf2", "SA.Leaf3", "Biomass.Leaf1", "Biomass.Leaf2", "Biomass.Leaf3",
               "SLA.Leaf1", "SLA.Leaf2", "SLA.Leaf3")] <- NULL
data_traits[is.nan(data_traits)] <- NA


#Import 2018 abundance data
abundance_data.2018 <- read.csv("~/Documents/GitHub/Trait-profiles-and-functional-diversity-following-disturbance-in-a-mixed-grassland/Trait profiles and functional diversity following disturbance in a mixed grassland/AbundanceData2018.csv", na.strings=c("NA",""))
abundance_data.2018 <- abundance_data.2018 %>% filter(Community != "N/A")
abundance_data.2018["Community"] <- NULL
View(abundance_data.2018)

# Subset the trait information to only those species present in the 2018 data
data_traits <- subset(data_traits, Epithey %in% colnames(abundance_data.2018))
data_traits$Epithey <- factor(data_traits$Epithey)
View(data_traits)

sites <- as.character(data_traits$SiteName)
data_traits$SiteName <- NULL


# -Calculate Z-scores and remove outliers. #
# Calculate Z-scores for each measurement for each species.
data_traits.zscore <- data_traits %>%
  dplyr::group_by(Epithey) %>%  # Group by species ID
  dplyr::mutate(dplyr::across(where(is.numeric), scale)) %>% # Calculate Z-score
  dplyr::ungroup() %>% # Ungroup
  tibble::add_column(., SiteName = sites, .before=1) %>%
  as.data.frame()
rownames(data_traits.zscore) <- rownames(data_traits)

data_traits.zscore[is.nan(data_traits.zscore)] <- NA

data_traits <- tibble::add_column(data_traits, SiteName = sites, .before=1)


# Replace all trait values that have Z-scores > |3| with NA.
data_traits.outlier.rm <- data_traits

for(i in 3:ncol(data_traits.zscore)){
  for(j in 1:nrow(data_traits.zscore)){
    if(is.na(data_traits.zscore[j,i])=="FALSE" & abs(data_traits.zscore[j,i]) > 3){
      data_traits.outlier.rm[j,i] <- NA
    }
  }
}



# b) Calculate average values for each trait for each species, across [1] all sites and [2] just Kinsella #

data_traits.avg.all <- data_traits.outlier.rm %>%
  dplyr::group_by(Epithey) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  as.data.frame() 
data_traits.avg.all[is.nan(data_traits.avg.all)] <- NA

# Are there any species in the cover data that are NOT in the trait data?
df <- data.frame(Epithey = colnames(abundance_data.2018),
                 Trait = colnames(abundance_data.2018) %in% data_traits.avg.all$Epithey)
print(as.character(subset(df, Trait=="FALSE")$Epithey)) # There is no trait data for these taxa.

rm(df)

# Calculate average trait values at Kinsella only.
data_traits.avg.Kin <- subset(data_traits.outlier.rm, SiteName=="Kin")
data_traits.avg.Kin <- data_traits.avg.Kin %>%
  dplyr::group_by(Epithey) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  as.data.frame()
data_traits.avg.Kin[is.nan(data_traits.avg.Kin)] <- NA


# What species are present in the cover data, but do not have any trait information at Kinsella?
df <- data.frame(Epithey = colnames(abundance_data.2018),
                 Kinsella = colnames(abundance_data.2018) %in% data_traits.avg.Kin$Epithey)
as.character(subset(df, Kinsella=="FALSE")$Epithey) # Kinsella has no trait data for these taxa.
rm(df)


# What species are present in the total averages but not the Kinsella averages? 

# Making rownames into species for all and kinsella avg traits
rownames(data_traits.avg.all) <- data_traits.avg.all[,"Epithey"]
data_traits.avg.all[,"Epithey"] <- NULL

rownames(data_traits.avg.Kin) <- data_traits.avg.Kin[,"Epithey"]
data_traits.avg.Kin[,"Epithey"] <- NULL


####2018 FUNCTIONAL DIVERSITY ####
# Check for highly correlated traits.
corr <- Hmisc::rcorr(as.matrix(data_traits.avg.all), type = "spearman")
corr <- corr[["r"]]
View(corr)

### match up trait and weight data frames
traits <- data_traits.avg.Kin # trait data: species are rows and traits are columns
traits[,c("Vegetative.Height", "Canopy.W2", "Stem.Volume","Epithey")] <- NULL # Drop traits that I don't want in my Fdiv calculation
weights <- abundance_data.2018 # rows are sites and species are columns
traits <- subset(traits, rownames(traits) %in% colnames(abundance_data.2018)) # Subset traits to only species present in abundance data
traits <- traits[order(match(rownames(traits), colnames(weights))), ] # Make sure the two data frames are in the same order

### calculate fdiv
fdiv_results.2018 <- dbFD(traits, weights, stand.x=TRUE, corr="none") # Calculate diversity. "stand.x=TRUE" standardizes traits to mean 0 and unit variance.
View(fdiv_results.2018)

qual.Fric.2018 <- fdiv_results.2018$qual.FRic
View(qual.Fric.2018)


fdiv.2018 <- data.frame( # Extract the measurements that I want to use in downstream analyses.
  no.spp = fdiv_results.2018$nbsp,
  FRic = fdiv_results.2018$FRic,
  FEve = fdiv_results.2018$FEve,
  FDiv = fdiv_results.2018$FDiv,
  FDis = fdiv_results.2018$FDis,
  RaoQ = fdiv_results.2018$RaoQ
)
view(fdiv.2018)


### Take community weighted means
CWM.2018 = fdiv_results.2018$CWM # Extract a separate data frame of community-weighted means
View(CWM.2018)

#### 2020 Data Prep ####
# Data prep
# a) Import trait information
data_traits <- read.csv("~/Documents/GitHub/Trait-profiles-and-functional-diversity-following-disturbance-in-a-mixed-grassland/Trait profiles and functional diversity following disturbance in a mixed grassland/Traits_Raw.csv",na.strings=c("NA",""))
data_traits[,c("SpecimenID", "SppCode", "Collector", "Date.collected", "MicrositeID", "flag", "Notes")] <- NULL # Remove metadata I don't need
data_traits[,c("Family","Genus","Species")] <- NULL # Remove taxonomy data I don't need (saved separately anyway)
data_traits[,c("CN.sampleID", "Soil.pH", "pH.Temp", "pH.SoilWeight")] <- NULL # Remove?? extra data that's not really a trait
data_traits[,c("X","X.1","X.2","X.3","X.4","X.5","X.6")] <- NULL

data_traits$Vegetative.Height <- as.numeric(as.character(data_traits$Vegetative.Height))

# Calculate means for leaf area, leaf biomass, and SLA.Leaf
data_traits <- data_traits %>%
  dplyr::mutate(SA.Leaf = rowMeans(dplyr::select(., "SA.Leaf1", "SA.Leaf2", "SA.Leaf3"), na.rm=TRUE),
                .after=SA.Leaf3) %>%
  dplyr::mutate(Biomass.Leaf = rowMeans(dplyr::select(., "Biomass.Leaf1", "Biomass.Leaf2", "Biomass.Leaf3"), na.rm=TRUE),
                .after=Biomass.Leaf3) %>%
  dplyr::mutate(SLA.Leaf = rowMeans(dplyr::select(., "SLA.Leaf1", "SLA.Leaf2", "SLA.Leaf3"), na.rm=TRUE),
                .after=SLA.Leaf3) %>%
  tibble::column_to_rownames("ShortID") %>%
  as.data.frame()

data_traits[,c("SA.Leaf1", "SA.Leaf2", "SA.Leaf3", "Biomass.Leaf1", "Biomass.Leaf2", "Biomass.Leaf3",
               "SLA.Leaf1", "SLA.Leaf2", "SLA.Leaf3")] <- NULL
data_traits[is.nan(data_traits)] <- NA

#Import abundance data
abundance_data.2020 <- read.csv("~/Documents/GitHub/Trait-profiles-and-functional-diversity-following-disturbance-in-a-mixed-grassland/Trait profiles and functional diversity following disturbance in a mixed grassland/AbundanceData2020.csv", na.strings=c("NA",""))
abundance_data.2020 <- abundance_data.2020 %>% filter(Community != "N/A")
abundance_data.2020["Community"] <- NULL
View(abundance_data.2020)

# Subset the trait information to only those species present in the data that I'm using.
data_traits <- subset(data_traits, Epithey %in% colnames(abundance_data.2020))
data_traits$Epithey <- factor(data_traits$Epithey)

sites <- as.character(data_traits$SiteName)
data_traits$SiteName <- NULL

# -Calculate Z-scores and remove outliers. #
# Calculate Z-scores for each measurement for each species.
data_traits.zscore <- data_traits %>%
  dplyr::group_by(Epithey) %>%  # Group by species ID
  dplyr::mutate(dplyr::across(where(is.numeric), scale)) %>% # Calculate Z-score
  dplyr::ungroup() %>% # Ungroup
  tibble::add_column(., SiteName = sites, .before=1) %>%
  as.data.frame()
rownames(data_traits.zscore) <- rownames(data_traits)

data_traits.zscore[is.nan(data_traits.zscore)] <- NA


data_traits <- tibble::add_column(data_traits, SiteName = sites, .before=1)


# Replace all trait values that have Z-scores > |3| with NA.
data_traits.outlier.rm <- data_traits

for(i in 3:ncol(data_traits.zscore)){
  for(j in 1:nrow(data_traits.zscore)){
    if(is.na(data_traits.zscore[j,i])=="FALSE" & abs(data_traits.zscore[j,i]) > 3){
      data_traits.outlier.rm[j,i] <- NA
    }
  }
}

# b) Calculate average values for each trait for each species, across [1] all sites and [2] just Kinsella #

data_traits.avg.all <- data_traits.outlier.rm %>%
  dplyr::group_by(Epithey) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  as.data.frame() 
data_traits.avg.all[is.nan(data_traits.avg.all)] <- NA

# Are there any species in the cover data that are NOT in the trait data? 
df <- data.frame(Epithey = colnames(abundance_data.2020),
                 Trait = colnames(abundance_data.2020) %in% data_traits.avg.all$Epithey)
print(as.character(subset(df, Trait=="FALSE")$Epithey)) # There is no trait data for these taxa: Collin, Genama, Silvery.hairy.mustard, Weed.1


rm(df)


# Calculate average trait values at Kinsella only. 

data_traits.avg.Kin <- subset(data_traits.outlier.rm, SiteName=="Kin")
data_traits.avg.Kin <- data_traits.avg.Kin %>%
  dplyr::group_by(Epithey) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  as.data.frame()
data_traits.avg.Kin[is.nan(data_traits.avg.Kin)] <- NA

# What species are present in the cover data, but do not have any trait information at Kinsella? 
df <- data.frame(Epithey = colnames(abundance_data.2020),
                 Kinsella = colnames(abundance_data.2020) %in% data_traits.avg.Kin$Epithey)
as.character(subset(df, Kinsella=="FALSE")$Epithey) # Kinsella has no trait data for these taxa: Cirvul, Collin, Genama, Silvery.hairy.mustard, Sonarv, Weed.1
rm(df)


# What species are present in the total averages but not the Kinsella averages? Cirvul and Sonarv
setdiff(data_traits.avg.all$Epithey, data_traits.avg.Kin$Epithey)

# Making rownames into species for all and kinsella avg traits
rownames(data_traits.avg.all) <- data_traits.avg.all[,"Epithey"]
data_traits.avg.all[,"Epithey"] <- NULL

rownames(data_traits.avg.Kin) <- data_traits.avg.Kin[,"Epithey"]
data_traits.avg.Kin[,"Epithey"] <- NULL


#### 2020 FUNCTIONAL DIVERSITY ####

# Check for highly correlated traits.
corr <- Hmisc::rcorr(as.matrix(data_traits.avg.all), type = "spearman")
corr <- corr[["r"]]

### match up trait and weight data frames
traits <- data_traits.avg.Kin # trait data: species are rows and traits are columns
traits[,c("Vegetative.Height", "Canopy.W2", "Stem.Volume","Epithey")] <- NULL # Drop traits that I don't want in my Fdiv calculation
weights <- abundance_data.2020 # rows are sites and species are columns
traits <- subset(traits, rownames(traits) %in% colnames(abundance_data.2020)) # Subset traits to only species present in abundance data
traits <- traits[order(match(rownames(traits), colnames(weights))), ] # Make sure the two data frames are in the same order

### removed species with no kinsella trait data from weights #not working yet
weights <- weights %>% select (-c(Cirvul, Collin, Genama, Silvery.hairy.mustard, Sonarv, Weed.1))

### calculate fdiv
fdiv_results.2020 <- dbFD(traits, weights, stand.x=TRUE, corr="none") # Calculate diversity. "stand.x=TRUE" standardizes traits to mean 0 and unit variance.
View(fdiv_results.2020)


### adding columns to the data frame for plot and subplot and normalized FFric, and for the rest of the FD indices

site <- c("Site2", "Site2", "Site3", "Site3", "Site4", "Site4","Site5", "Site5", "Site6", "Site6", "Site7", "Site7", "Site11", "Site11", "Site12", "Site12", "Site14", "Site14", "Site15", "Site15", "Site16", "Site16", "Site17", "Site17")

disturbance <- c("ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2")

fric_tansformed <- log(fdiv_results.2020$FRic)

fdiv.2020 <- data.frame( # Extract the measurements that I want to use in downstream analyses.
  no.spp = fdiv_results.2020$nbsp,
  FRic = fdiv_results.2020$FRic,
  Fric_Tansformed = fric_tansformed, 
  FEve = fdiv_results.2020$FEve,
  FDiv = fdiv_results.2020$FDiv,
  FDis = fdiv_results.2020$FDis,
  RaoQ = fdiv_results.2020$RaoQ,
  Disturbance = disturbance, 
  Site = site
)
View(fdiv.2020)




### Take community weighted means
CWM.2020 = fdiv_results.2020$CWM # Extract a separate data frame of community-weighted means
View(CWM.2020)
### Adding site and disturbance level to the CWM traits
site <- c("Site2", "Site2", "Site3", "Site3", "Site4", "Site4","Site5", "Site5", "Site6", "Site6", "Site7", "Site7", "Site11", "Site11", "Site12", "Site12", "Site14", "Site14", "Site15", "Site15", "Site16", "Site16", "Site17", "Site17")
disturbance <- c("ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2")

Canopy.W1.transformed <- log(CWM.2020$Canopy.W1)
Root.avgdiam.transformed <- log(CWM.2020$Root.AvgDiam)

CWM.2020 = data.frame(fdiv_results.2020$CWM, Site = site, Disturbance = disturbance, Canopy.W1.Transformed = Canopy.W1.transformed, Root.AvgDiam.Transformed = Root.avgdiam.transformed)

#### LMM's ####
install.packages("lme4")
install.packages("MASS")
install.packages("lmerTest")
install.packages("gridExtra")
install.packages("egg")

library(lmerTest)
library(lme4)
library(MASS)
library(ggplot2)
library(gridExtra)
library(egg)

### 2020 Functional Diversity Indices ###

### RUNNING LMM FOR NORMALIZED FRIC ###
FRic.LMM.2020 <- lmer(Fric_Tansformed~Disturbance+(1|Site), data=fdiv.2020)
summary(FRic.LMM.2020)
# Checking residuals are normally distributed
qqnorm(resid(FRic.LMM.2020))
qqline(resid(FRic.LMM.2020))

### RUNNING LMM FOR FDIV ###
FDiv.LMM.2020 <- lmer(FDiv~Disturbance+(1|Site), data=fdiv.2020)
summary(FDiv.LMM.2020)
# Checking residuals are normally distributed
qqnorm(resid(FDiv.LMM.2020))
qqline(resid(FDiv.LMM.2020))

### RUNNING LMM FOR FDIS ###
FDis.LMM.2020 <- lmer(FDis~Disturbance+(1|Site), data=fdiv.2020)
summary(FDis.LMM.2020)
# Checking residuals are normally distributed
qqnorm(resid(FDis.LMM.2020))
qqline(resid(FDis.LMM.2020))

### RUNNING LMM FOR FEVE ###
FEve.LMM.2020 <- lmer(FEve~Disturbance+(1|Site), data=fdiv.2020)
summary(FEve.LMM.2020)
# Checking residuals are normally distributed
qqnorm(resid(FEve.LMM.2020))
qqline(resid(FEve.LMM.2020))

### RUNNING LMM FOR RAOQ ###
RaoQ.LMM.2020 <- lmer(RaoQ~Disturbance+(1|Site), data=fdiv.2020)
summary(RaoQ.LMM.2020)
# Checking residuals are normally distributed
qqnorm(resid(RaoQ.LMM.2020))
qqline(resid(RaoQ.LMM.2020))

### 2020 Community Weighted Mean Traits ###

### RUNNING LMM FOR MAX HEIGHT ###
Max.Height.LMM.2020 <- lmer(Max.Height~Disturbance+(1|Site), data=CWM.2020)
summary(Max.Height.LMM.2020)
# Checking residuals are normally distributed
qqnorm(resid(Max.Height.LMM.2020))
qqline(resid(Max.Height.LMM.2020))

### RUNNING LMM FOR SPECIFIC LEAF AREA ###
SLA.Leaf.LMM.2020 <- lmer(SLA.Leaf~Disturbance+(1|Site), data=CWM.2020)
summary(SLA.Leaf.LMM.2020)
# Checking residuals are normally distributed
qqnorm(resid(SLA.Leaf.LMM.2020))
qqline(resid(SLA.Leaf.LMM.2020))

### RUNNING LMM FOR SPECIFIC ROOT LENGTH ###
SRL.LMM.2020 <- lmer(SRL~Disturbance+(1|Site), data=CWM.2020)
summary(SRL.LMM.2020)
# Checking residuals are normally distributed
qqnorm(resid(SRL.LMM.2020))
qqline(resid(SRL.LMM.2020))

### RUNNING LMM FOR CANOPY WIDTH ###
Canopy.W1.LMM.2020 <- lmer(Canopy.W1.Transformed~Disturbance+(1|Site), data=CWM.2020)
summary(Canopy.W1.LMM.2020)
# Checking residuals are normally distributed
qqnorm(resid(Canopy.W1.LMM.2020))
qqline(resid(Canopy.W1.LMM.2020))

### RUNNING LMM FOR LEAF NITROGEN CONTENT ###  
N.shoot.LMM.2020 <- lmer(N.shoot~Disturbance+(1|Site), data=CWM.2020)
summary(N.shoot.LMM.2020)
# Checking residuals are normally distributed
qqnorm(resid(N.shoot.LMM.2020))
qqline(resid(N.shoot.LMM.2020))

### RUNNING LMM FOR ROOT NITROGEN CONTENT ###  
N.root.LMM.2020 <- lmer(N.root~Disturbance+(1|Site), data=CWM.2020)
summary(N.root.LMM.2020)
# Checking residuals are normally distributed
qqnorm(resid(N.root.LMM.2020))
qqline(resid(N.root.LMM.2020))

#### Graphs of the 2020 FD Metrics ####

### Making an FEve data frame
FEve_ANR <- fdiv.2020$FEve[seq(1, 24, 2)]
FEve_NNR2 <- fdiv.2020$FEve[seq(2, 24, 2)]
SD_FEveANR <- sd(FEve_ANR)
SD_FEveNNR2 <- sd(FEve_NNR2)
FEve_DataFrame.2020 <- data.frame(AvgFEve = c(mean(FEve_ANR), mean(FEve_NNR2)),
                                  Subplot = c("Disturbed", "Undisturbed"),
                                  FEveError = c(SD_FEveANR/sqrt(12), SD_FEveNNR2/sqrt(12)))
print(FEve_DataFrame.2020)
### Making an FEve graph
pEve <- ggplot(FEve_DataFrame.2020, aes(x= Subplot, y= AvgFEve, fill = Subplot)) +
  geom_bar(stat = "identity", width=0.9, position=position_dodge()) +
  geom_errorbar(aes(x=Subplot, ymin=AvgFEve-FEveError, ymax=AvgFEve+FEveError), width=0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray28", "gray")) +
  labs(x = "Disturbed vs. undisturbed plot type", y = "Functional evenness") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"))+
  theme(legend.position = "none")

### Making an FDiv data frame
FDiv_ANR <- fdiv.2020$FDiv[seq(1, 24, 2)]
FDiv_NNR2 <- fdiv.2020$FDiv[seq(2, 24, 2)]
SD_FDivANR <- sd(FDiv_ANR)
SD_FDivNNR2 <- sd(FDiv_NNR2)
FDiv_DataFrame.2020 <- data.frame(AvgFDiv = c(mean(FDiv_ANR), mean(FDiv_NNR2)),
                                  Subplot = c("Disturbed", "Undisturbed"),
                                  FDivError = c(SD_FDivANR/sqrt(12), SD_FDivNNR2/sqrt(12)))
print(FDiv_DataFrame.2020)

### Making an FDiv graph
pDiv <- ggplot(FDiv_DataFrame.2020, aes(x= Subplot, y= AvgFDiv, fill = Subplot)) +
  geom_bar(stat = "identity", width=0.9, position=position_dodge()) +
  geom_errorbar(aes(x=Subplot, ymin=AvgFDiv-FDivError, ymax=AvgFDiv+FDivError), width=0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray28", "gray")) +
  labs(x = "Disturbed vs. undisturbed plot type", y = "Functional divergence") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"))+
  theme(legend.position = "none")


### Making an FRic data frame (transformed)
FRic_ANR <- fdiv.2020$Fric_Tansformed[seq(1, 24, 2)]
FRic_NNR2 <- fdiv.2020$Fric_Tansformed[seq(2, 24, 2)]
SD_FRicANR <- sd(FRic_ANR)
SD_FRicNNR2 <- sd(FRic_NNR2)
FRic_DataFrame.2020 <- data.frame(AvgFRic = c(mean(FRic_ANR)*-1, mean(FRic_NNR2)*-1),
                                  Subplot = c("Disturbed", "Undisturbed"),
                                  FRicError = c(SD_FRicANR/sqrt(12), SD_FRicNNR2/sqrt(12)))
print(FRic_DataFrame.2020)

### Making an FRic transformed graph
pRic <- ggplot(FRic_DataFrame.2020, aes(x= Subplot, y= AvgFRic, fill = Subplot)) +
  geom_bar(stat = "identity", width=0.9, position=position_dodge()) +
  geom_errorbar(aes(x=Subplot, ymin=AvgFRic-FRicError, ymax=AvgFRic+FRicError), width=0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray28", "gray")) +
  labs(x = "Disturbed vs. undisturbed plot type", y = "Functional richness") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"))+
  theme(legend.position = "none")


### Making an Rao's Q data frame
Q_ANR <- fdiv.2020$RaoQ[seq(1, 24, 2)]
Q_NNR2 <- fdiv.2020$RaoQ[seq(2, 24, 2)]
SD_QANR <- sd(Q_ANR)
SD_QNNR2 <- sd(Q_NNR2)
Q_DataFrame.2020 <- data.frame(AvgQ = c(mean(Q_ANR), mean(Q_NNR2)),
                               Subplot = c("Disturbed", "Unisturbed"),
                               QError = c(SD_QANR/sqrt(12), SD_QNNR2/sqrt(12)))
print(Q_DataFrame.2020)

### Making an Q transformed graph
pQ<- ggplot(Q_DataFrame.2020, aes(x= Subplot, y= AvgQ, fill = Subplot)) +
  geom_bar(stat = "identity", width=0.9, position=position_dodge()) +
  geom_errorbar(aes(x=Subplot, ymin=AvgQ-QError, ymax=AvgQ+QError), width=0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray28", "gray")) +
  labs(x = "Disturbed vs. undisturbed plot type", y = "Rao's quadratic entropy") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"))+
  theme(legend.position = "none")

#### Arranging an array of the FD graphs
ggarrange(pRic + 
            theme(axis.title.x = element_blank()),
          pEve + 
            theme(axis.title.x = element_blank()), 
          pDiv + 
            theme(axis.title.x = element_blank()),
          pQ + 
            theme(axis.title.x = element_blank()),
          bottom = "Disturbed vs. undisturbed plot type",
          nrow = 2)

### Graphs of CWM's (SLA, Leaf N, Root N, height, SRL) ###
### Making an SLA  data frame
SLA_ANR <- CWM.2020$SLA.Leaf[seq(1, 24, 2)]
SLA_NNR2 <- CWM.2020$SLA.Leaf[seq(2, 24, 2)]
SD_SLA.ANR <- sd(SLA_ANR)
SD_SLA.NNR2 <- sd(SLA_NNR2)
SLA_DataFrame.2020 <- data.frame(AvgSLA = c(mean(SLA_ANR), mean(SLA_NNR2)),
                                 Subplot = c("Disturbed", "Undisturbed"),
                                 SLA.Error = c(SD_SLA.ANR/sqrt(12), SD_SLA.NNR2/sqrt(12)))
print(SLA_DataFrame.2020)
### Making an SLA graph
pSLA<- ggplot(SLA_DataFrame.2020, aes(x= Subplot, y= AvgSLA, fill = Subplot)) +
  geom_bar(stat = "identity", width=0.9, position=position_dodge()) +
  geom_errorbar(aes(x=Subplot, ymin=AvgSLA-SLA.Error, ymax=AvgSLA+SLA.Error), width=0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray28", "gray")) +
  labs(y = "Specific leaf area\n(cm^2/g)") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"))+
  theme(legend.position = "none")


### Making an Leaf N  data frame
N.shoot_ANR <- CWM.2020$N.shoot[seq(1, 24, 2)]
N.shoot_NNR2 <- CWM.2020$N.shoot[seq(2, 24, 2)]
SD_Nshoot.ANR <- sd(N.shoot_ANR)
SD_Nshoot.NNR2 <- sd(N.shoot_NNR2)
Nshoot_DataFrame.2020 <- data.frame(AvgNshoot = c(mean(N.shoot_ANR), mean(N.shoot_NNR2)),
                                    Subplot = c("Disturbed", "Undisturbed"),
                                    Nshoot.Error = c(SD_Nshoot.ANR/sqrt(12), SD_Nshoot.NNR2/sqrt(12)))
print(Nshoot_DataFrame.2020)

### Making an N.shoot graph
pNshoot<- ggplot(Nshoot_DataFrame.2020, aes(x= Subplot, y= AvgNshoot, fill = Subplot)) +
  geom_bar(stat = "identity", width=0.9, position=position_dodge()) +
  geom_errorbar(aes(x=Subplot, ymin=AvgNshoot-Nshoot.Error, ymax=AvgNshoot+Nshoot.Error), width=0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray28", "gray")) +
  labs(y = "Leaf nitrogen\n(%)") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"))+
  theme(legend.position = "none")

### Making an Root N  data frame
N.root_ANR <- CWM.2020$N.root[seq(1, 24, 2)]
N.root_NNR2 <- CWM.2020$N.root[seq(2, 24, 2)]
SD_Nroot.ANR <- sd(N.root_ANR)
SD_Nroot.NNR2 <- sd(N.root_NNR2)
Nroot_DataFrame.2020 <- data.frame(AvgNroot = c(mean(N.root_ANR), mean(N.root_NNR2)),
                                   Subplot = c("Disturbed", "Undisturbed"),
                                   Nroot.Error = c(SD_Nroot.ANR/sqrt(12), SD_Nroot.NNR2/sqrt(12)))
print(Nroot_DataFrame.2020)
### Making an N.root graph
pNroot<- ggplot(Nroot_DataFrame.2020, aes(x= Subplot, y= AvgNroot, fill = Subplot)) +
  geom_bar(stat = "identity", width=0.9, position=position_dodge()) +
  geom_errorbar(aes(x=Subplot, ymin=AvgNroot-Nroot.Error, ymax=AvgNroot+Nroot.Error), width=0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray28", "gray")) +
  labs(y = "Root nitrogen\n(%)") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"))+
  theme(legend.position = "none")

# making a height data frame
Height_ANR <- CWM.2020$Max.Height[seq(1, 24, 2)]
Height_NNR2 <- CWM.2020$Max.Height[seq(2, 24, 2)]
SD_Height.ANR <- sd(N.root_ANR)
SD_Height.NNR2 <- sd(N.root_NNR2)
Height_DataFrame.2020 <- data.frame(AvgHeight = c(mean(Height_ANR), mean(Height_NNR2)),
                                    Subplot = c("Disturbed", "Undisturbed"),
                                    Height.Error = c(SD_Height.ANR/sqrt(12), SD_Height.NNR2/sqrt(12)))
print(Height_DataFrame.2020)
### Making a height graph
pHeight <- ggplot(Height_DataFrame.2020, aes(x= Subplot, y= AvgHeight, fill = Subplot)) +
  geom_bar(stat = "identity", width=0.9, position=position_dodge()) +
  geom_errorbar(aes(x=Subplot, ymin=AvgHeight-Height.Error, ymax=AvgHeight+Height.Error), width=0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray28", "gray")) +
  labs(y = "Max height\n(cm)") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"))+
  theme(legend.position = "none")


# making a specific root length data frame
SRL_ANR <- CWM.2020$SRL[seq(1, 24, 2)]
SRL_NNR2 <- CWM.2020$SRL[seq(2, 24, 2)]
SD_SRL.ANR <- sd(SRL_ANR)
SD_SRL.NNR2 <- sd(SRL_NNR2)
SRL_DataFrame.2020 <- data.frame(AvgSRL = c(mean(SRL_ANR), mean(SRL_NNR2)),
                                 Subplot = c("Disturbed", "Undisturbed"),
                                 SRL.Error = c(SD_SRL.ANR/sqrt(12), SD_SRL.NNR2/sqrt(12)))
print(SRL_DataFrame.2020)
### Making an SRL graph
pSRL<- ggplot(SRL_DataFrame.2020, aes(x= Subplot, y= AvgSRL, fill = Subplot)) +
  geom_bar(stat = "identity", width=0.9, position=position_dodge()) +
  geom_errorbar(aes(x=Subplot, ymin=AvgSRL-SRL.Error, ymax=AvgSRL+SRL.Error), width=0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray28", "gray")) +
  labs(y = "Specific root length\n(cm/g)") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"))+
  theme(legend.position = "none")

#### Arranging an array of the trait graphs
ggarrange(pSRL + 
            theme(axis.title.x = element_blank()),
          pHeight + 
            theme(axis.title.x = element_blank()), 
          pSLA + 
            theme(axis.title.x = element_blank()),
          pNroot + 
            theme(axis.title.x = element_blank()),
          pNshoot + 
            theme(axis.title.x = element_blank()),
          bottom = "Disturbed vs. undisturbed plot type",
          nrow = 2)

