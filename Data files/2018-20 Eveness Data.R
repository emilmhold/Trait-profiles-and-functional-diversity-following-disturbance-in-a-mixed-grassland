install.packages(c("tidyverse", "dslabs", "vegan","ggplot2", "readxl"))

library("tidyverse")
library("dslabs")
library("vegan")
library("readxl")
library("ggplot2")

### importing data for evenness

ecommunity_2020 <- read_excel("~/Desktop/Cahill 2020 Reseach/ComProp2020_Transposed.xlsx")

### taking evenness (from https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf)

ecommunity_2020 <- ecommunity_2020[, 2:64]

H <- diversity(ecommunity_2020)
J <- H/log(specnumber(ecommunity_2020))

### seperateing ANR and NNR2 eveness

ANR_Eveness2020<- J[seq(1, length(J), 2)]
NNR2_Eveness2020 <- J[seq(2, length(J), 2)]
print(NNR2_Eveness2020)

### making an anr evenness data frame

ANR_Evenness_2020Frame <- data.frame(Evenness = ANR_Eveness2020, 
                                Subplot = c("ANR_2", "ANR_3", "ANR_4", "ANR_5", "ANR_6", "ANR_7", "ANR_11", "ANR_12", "ANR_14", "ANR_15", "ANR_16", "ANR_17"))

### taking average and standard deviation for anr evenness

Avg_ANR_E2020 <- mean(ANR_Eveness2020)

StDev_ANR_E2020 <- sd(ANR_Eveness2020)


### making a nnr2 evenness data frame

NNR2_Eveness2020Frame <- data.frame(Evenness = NNR2_Eveness2020,
                                 Subplot = c("NNR2_2", "NNR2_3", "NNR2_4", "NNR2_5", "NNR2_6", "NNR2_7", "NNR2_11", "NNR2_12", "NNR2_14", "NNR2_15", "NNR2_16", "NNR2_17"))

### taking average and standard deviation for nnr2 evenness

Avg_NNR2_E2020 <- mean(NNR2_Eveness2020)
StDev_NNR2_E2020 <- sd(NNR2_Eveness2020)


### importing august 2018 nnr2 community proportions data
ecommunity_2018 <- read_excel("~/Desktop/Cahill 2020 Reseach/ComProp2018_Transposed.xlsx")


### taking evenness (from https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf)

ecommunity_2018 <- ecommunity_2018[, 2:40]

H_2 <- diversity(ecommunity_2018)
NNR2_Eveness2018 <- H_2/log(specnumber(ecommunity_2018))

### making a nnr2 evenness data frame

NNR2_Eveness2018Frame <- data.frame(Evenness = NNR2_Eveness2018,
                                    Subplot = c("NNR2_2", "NNR2_3", "NNR2_4", "NNR2_5", "NNR2_6", "NNR2_7", "NNR2_11", "NNR2_12", "NNR2_14", "NNR2_15", "NNR2_16", "NNR2_17"))

### taking average and standard deviation for nnr2 evenness

Avg_NNR2_E2018 <- mean(NNR2_Eveness2018)

StDev_NNR2_E2018 <- sd(NNR2_Eveness2018)


### making an average eveness data frame

Avg_Evenness <- data.frame(Evenness = c(Avg_ANR_E2020, Avg_NNR2_E2020, 0, Avg_NNR2_E2018), 
                           Plot = c("Disturbed", "Undisturbed", "Disturbed", "Undisturbed"), 
                           StDev= c(StDev_ANR_E2020, StDev_NNR2_E2020, 0, StDev_NNR2_E2018), 
                           StError = c(StDev_ANR_E2020/sqrt(12), StDev_NNR2_E2020/sqrt(12), 0, StDev_NNR2_E2018/sqrt(12)), 
                           Year=(c("2020", "2020", "2018" ,"2018")))
print(Avg_Evenness)
### graphing evenness

ggplot(Avg_Evenness, aes(x= Year, y= Evenness, fill = Plot)) +
  geom_bar(stat = "identity", width=0.9, position=position_dodge()) +
  geom_errorbar(aes(x=Year, ymin=Evenness-StError, ymax=Evenness+StError), width=0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray28", "gray")) +
  scale_y_continuous() +
  labs(x = "Plot by year", y = "Average evenness") +
  theme_classic()+ 
  theme(axis.text = element_text(color = "black"))
  ### extra code to include exact values in chart: geom_text(aes(label = round(Richness, digits = 2)), vjust=15, position = position_dodge(width = .9)) +



