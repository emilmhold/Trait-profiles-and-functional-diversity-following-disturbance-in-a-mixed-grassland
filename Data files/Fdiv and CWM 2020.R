#### FUNCTIONAL DIVERSITY ####
install.packages("dplyr")
library(FD)
library(ggplot2)
library(tidyverse)
library(dslabs)
library(dplyr)


is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))}

####Data prep####
# a) Import trait information
data_traits <- read.csv("~/Desktop/Cahill 2020 Reseach/Traits_Raw.csv", na.strings=c("NA",""))
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
abundance_data <- read.csv("~/Desktop/Cahill 2020 Reseach/AbundanceData2020.csv", na.strings=c("NA",""))
abundance_data <- abundance_data %>% filter(Community != "N/A")
abundance_data["Community"] <- NULL
View(abundance_data)



#If Fdiv is not working and returns error that "no species are present in some communitites":
#write.csv(weights,"~/Documents/Thesis/Winter 2021 Analyses/02.12.21.abundance_data.csv", row.names = TRUE)
##look for zero column/row totals

# Subset the trait information to only those species present in the data that I'm using.
data_traits <- subset(data_traits, Epithey %in% colnames(abundance_data))
data_traits$Epithey <- factor(data_traits$Epithey)


sites <- as.character(data_traits$SiteName)
data_traits$SiteName <- NULL


# -Calculate Z-scores and remove outliers. ####
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



# b) Calculate average values for each trait for each species, across [1] all sites and [2] just Kinsella ####

data_traits.avg.all <- data_traits.outlier.rm %>%
  dplyr::group_by(Epithey) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  as.data.frame() 
data_traits.avg.all[is.nan(data_traits.avg.all)] <- NA

# Are there any species in the cover data that are NOT in the trait data? Agscom, Collin, Genama, Penpro, Silvery.hairy.mustard, "Weed.1" 
df <- data.frame(Epithey = colnames(abundance_data),
                 Trait = colnames(abundance_data) %in% data_traits.avg.all$Epithey)
print(as.character(subset(df, Trait=="FALSE")$Epithey)) # There is no trait data for these taxa.


rm(df)


# Calculate average trait values at Kinsella only. 

data_traits.avg.Kin <- subset(data_traits.outlier.rm, SiteName=="Kin")
data_traits.avg.Kin <- data_traits.avg.Kin %>%
  dplyr::group_by(Epithey) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  as.data.frame()
data_traits.avg.Kin[is.nan(data_traits.avg.Kin)] <- NA


# What species are present in the cover data, but do not have any trait information at Kinsella? Cirvul, Collin, Genama, Silvery hairy mustard, Sonarv, Weed 1
df <- data.frame(Epithey = colnames(abundance_data),
                 Kinsella = colnames(abundance_data) %in% data_traits.avg.Kin$Epithey)
as.character(subset(df, Kinsella=="FALSE")$Epithey) # Kinsella has no trait data for these taxa.
rm(df)


# What species are present in the total averages but not the Kinsella averages? Cirvul and Sonarv
setdiff(data_traits.avg.all$Epithey, data_traits.avg.Kin$Epithey)

# Making rownames into species for all and kinsella avg traits
rownames(data_traits.avg.all) <- data_traits.avg.all[,"Epithey"]
data_traits.avg.all[,"Epithey"] <- NULL

rownames(data_traits.avg.Kin) <- data_traits.avg.Kin[,"Epithey"]
data_traits.avg.Kin[,"Epithey"] <- NULL


#### FUNCTIONAL DIVERSITY ####

# Check for highly correlated traits.
corr <- Hmisc::rcorr(as.matrix(data_traits.avg.all), type = "spearman")
corr <- corr[["r"]]

### match up trait and weight data frames
traits <- data_traits.avg.Kin # trait data: species are rows and traits are columns
traits[,c("Vegetative.Height", "Canopy.W2", "Stem.Volume","Epithey")] <- NULL # Drop traits that I don't want in my Fdiv calculation
weights <- abundance_data # rows are sites and species are columns
traits <- subset(traits, rownames(traits) %in% colnames(abundance_data)) # Subset traits to only species present in abundance data
traits <- traits[order(match(rownames(traits), colnames(weights))), ] # Make sure the two data frames are in the same order


### check what is missing trait data again
df <- data.frame(Epithey = colnames(weights),
                 Trait = colnames(weights) %in% rownames(traits))
as.character(subset(df, Trait=="FALSE")$Epithey) # There is no trait data for these taxa.

### removed species with no kinsella trait data from weights
weights = select(weights, -"Cirvul", -"Collin", -"Genama", -"Silvery.hairy.mustard", -"Sonarv", -"Weed.1")

### calculate fdiv
fdiv_results <- dbFD(traits, weights, stand.x=TRUE, corr="none") # Calculate diversity. "stand.x=TRUE" standardizes traits to mean 0 and unit variance.
View(fdiv_results)


### adding columns to the data frame for plot and subplot and normalized FFric, and for the rest of the FD indices

site <- c("Site2", "Site2", "Site3", "Site3", "Site4", "Site4","Site5", "Site5", "Site6", "Site6", "Site7", "Site7", "Site11", "Site11", "Site12", "Site12", "Site14", "Site14", "Site15", "Site15", "Site16", "Site16", "Site17", "Site17")

disturbance <- c("ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2")

fric_tansformed <- log(fdiv_results$FRic)

fdiv <- data.frame( # Extract the measurements that I want to use in downstream analyses.
  no.spp = fdiv_results$nbsp,
  FRic = fdiv_results$FRic,
  Fric_Tansformed = fric_tansformed, 
  FEve = fdiv_results$FEve,
  FDiv = fdiv_results$FDiv,
  FDis = fdiv_results$FDis,
  RaoQ = fdiv_results$RaoQ,
  Disturbance = disturbance, 
  Site = site
)
View(fdiv)




### Take community weighted means
CWM = fdiv_results$CWM # Extract a separate data frame of community-weighted means
View(CWM)
### Adding site and disturbance level to the CWM traits
site <- c("Site2", "Site2", "Site3", "Site3", "Site4", "Site4","Site5", "Site5", "Site6", "Site6", "Site7", "Site7", "Site11", "Site11", "Site12", "Site12", "Site14", "Site14", "Site15", "Site15", "Site16", "Site16", "Site17", "Site17")
disturbance <- c("ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2", "ANR", "NNR2")

Canopy.W1.transformed <- log(CWM$Canopy.W1)
Root.avgdiam.transformed <- log(CWM$Root.AvgDiam)

CWM = data.frame(fdiv_results$CWM, Site = site, Disturbance = disturbance, Canopy.W1.Transformed = Canopy.W1.transformed, Root.AvgDiam.Transformed = Root.avgdiam.transformed)


