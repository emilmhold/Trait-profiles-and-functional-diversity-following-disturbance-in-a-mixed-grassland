### installing packages i might need

install.packages(c("tidyverse", "dslabs", "vegan","ggplot2", "readxl"))

library("tidyverse")
library("dslabs")
library("vegan")
library("readxl")
library("ggplot2")

### importing data for richness

community_2020 <- read_excel("~/Desktop/Cahill 2020 Reseach/ComProp2020.xlsx")

### trying to extract richness for all plots

community_2020$Treatment <- NULL
apply(community_2020, 2, specnumber)
totalplotrich <- data.frame(apply(community_2020, 2, specnumber))

### trying to extract richness for each anr subplot

ANR2_R <- specnumber(community_2020$`ANR (Dist) 2`)
ANR3_R <- specnumber(community_2020$`ANR (Dist) 3`)
ANR4_R <- specnumber(community_2020$`ANR (Dist) 4`)
ANR5_R <- specnumber(community_2020$`ANR (Dist) 5`)
ANR6_R <- specnumber(community_2020$`ANR (Dist) 6`)
ANR7_R <- specnumber(community_2020$`ANR (Dist) 7`)
ANR11_R <- specnumber(community_2020$`ANR (Dist) 11`)
ANR12_R <- specnumber(community_2020$`ANR (Dist) 12`)
ANR14_R <- specnumber(community_2020$`ANR (Dist) 14`)
ANR15_R <- specnumber(community_2020$`ANR (Dist) 15`)
ANR16_R <- specnumber(community_2020$`ANR (Dist) 16`)
ANR17_R <- specnumber(community_2020$`ANR (Dist) 17`)

### making a anr richness data frame

ANR_Richness_2020 <- data.frame(Richness = c(ANR2_R, ANR3_R, ANR4_R, ANR5_R, ANR6_R, ANR7_R, ANR11_R, ANR12_R, ANR14_R, ANR15_R, ANR16_R, ANR17_R), 
                                Subplot = c("ANR_2", "ANR_3", "ANR_4", "ANR_5", "ANR_6", "ANR_7", "ANR_11", "ANR_12", "ANR_14", "ANR_15", "ANR_16", "ANR_17"))

### taking average and standard deviation for anr richness

Avg_ANR_R2020 <- mean(c(ANR2_R, ANR3_R, ANR4_R, ANR5_R, ANR6_R, ANR7_R, ANR11_R, ANR12_R, ANR14_R, ANR15_R, ANR16_R, ANR17_R))

StDev_ANR_R2020 <- sd(c(ANR2_R, ANR3_R, ANR4_R, ANR5_R, ANR6_R, ANR7_R, ANR11_R, ANR12_R, ANR14_R, ANR15_R, ANR16_R, ANR17_R))

### trying to extract richness for each nnr2 subplot

NNR2_R <- specnumber(community_2020$`NNR2 (Undist) 2`)
NNR3_R <- specnumber(community_2020$`NNR2 (Undist) 3`)
NNR4_R <- specnumber(community_2020$`NNR2 (Undist) 4`)
NNR5_R <- specnumber(community_2020$`NNR2 (Undist) 5`)
NNR6_R <- specnumber(community_2020$`NNR2 (Undist) 6`)
NNR7_R <- specnumber(community_2020$`NNR2 (Undist) 7`)
NNR11_R <- specnumber(community_2020$`NNR2 (Undist) 11`)
NNR12_R <- specnumber(community_2020$`NNR2 (Undist) 12`)
NNR14_R <- specnumber(community_2020$`NNR2 (Undist) 14`)
NNR15_R <- specnumber(community_2020$`NNR2 (Undist) 15`)
NNR16_R <- specnumber(community_2020$`NNR2 (Undist) 16`)
NNR17_R <- specnumber(community_2020$`NNR2 (Undist) 17`)

### making a nnr2 richness data frame

NNR2_Richness_2020 <- data.frame(Richness = c(NNR2_R, NNR3_R, NNR4_R, NNR5_R, NNR6_R, NNR7_R, NNR11_R, NNR12_R, NNR14_R, NNR15_R, NNR16_R, NNR17_R), 
                                Subplot = c("NNR2_2", "NNR2_3", "NNR2_4", "NNR2_5", "NNR2_6", "NNR2_7", "NNR2_11", "NNR2_12", "NNR2_14", "NNR2_15", "NNR2_16", "NNR2_17"))

### taking average and standard deviation for nnr2 richness

Avg_NNR2_R2020 <- mean(c(NNR2_R, NNR3_R, NNR4_R, NNR5_R, NNR6_R, NNR7_R, NNR11_R, NNR12_R, NNR14_R, NNR15_R, NNR16_R, NNR17_R))

StDev_NNR2_R2020 <- sd(c(NNR2_R, NNR3_R, NNR4_R, NNR5_R, NNR6_R, NNR7_R, NNR11_R, NNR12_R, NNR14_R, NNR15_R, NNR16_R, NNR17_R))


# importing august 2018 nnr2 community proportions data
community_2018 <- read_excel("~/Desktop/Cahill 2020 Reseach/ComProp2018.xlsx")

# taking richness for each nnr2 subplot

NNR2_R2018 <- specnumber(community_2018$'NNR2 2')
NNR3_R2018 <- specnumber(community_2018$'NNR2 3')
NNR4_R2018 <- specnumber(community_2018$'NNR2 4')
NNR5_R2018 <- specnumber(community_2018$'NNR2 5')
NNR6_R2018 <- specnumber(community_2018$'NNR2 6')
NNR7_R2018 <- specnumber(community_2018$'NNR2 7')
NNR11_R2018 <- specnumber(community_2018$'NNR2 11')
NNR12_R2018 <- specnumber(community_2018$'NNR2 12')
NNR14_R2018 <- specnumber(community_2018$'NNR2 14')
NNR15_R2018 <- specnumber(community_2018$'NNR2 15')
NNR16_R2018 <- specnumber(community_2018$'NNR2 16')
NNR17_R2018 <- specnumber(community_2018$'NNR2 17')

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

ggplot(Avg_Richness, aes(x= Year, y= Richness, fill = Plot)) +
  geom_bar(stat = "identity", width=0.9, position=position_dodge()) +
  geom_errorbar(aes(x=Year, ymin=Richness-StError, ymax=Richness+StError), width=0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray28", "gray")) +
  scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20)) +
  labs(x = "Plot by year", y = "Average richness") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"))

  ## extra code taken out:   geom_text(aes(label = round(Richness, digits = 2)), vjust=15, position = position_dodge(width = .9)) +





