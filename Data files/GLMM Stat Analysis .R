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

### RUNNING GLMM FOR NORMALIZED FRIC ###
FRic.GLMA <- lmer(Fric_Tansformed~Disturbance+(1|Site), data=fdiv)
summary(FRic.GLMA)
# Checking ANOVA assumption 2: residuals are normal
qqnorm(resid(FRic.GLMA))
qqline(resid(FRic.GLMA))
# Checking ANOVA assumption number 2
ks.test(resid(FRic.GLMA),"pnorm",mean=mean(resid(FRic.GLMA)),sd=sd(resid(FRic.GLMA))) 
shapiro.test(resid(FRic.GLMA))


### RUNNING GLMM FOR FDIV ###
FDiv.GLMA <- lmer(FDiv~Disturbance+(1|Site), data=fdiv)
summary(FDiv.GLMA)
# Checking ANOVA assumption 2: how normal the residuals are
qqnorm(resid(FDiv.GLMA))
qqline(resid(FDiv.GLMA))
# Checking ANOVA assumption number 2
ks.test(resid(FDiv.GLMA),"pnorm",mean=mean(resid(FDiv.GLMA)),sd=sd(resid(FDiv.GLMA))) 
# Another normalcy test of populations to check ANOVA assumption 1 
shapiro.test(resid(FDiv.GLMA))


### RUNNING GLMM FOR FDIS ###
FDis.GLMA <- lmer(FDis~Disturbance+(1|Site), data=fdiv)
summary(FDis.GLMA)
# Checking ANOVA assumption 2: how normal the residuals are
qqnorm(resid(FDis.GLMA))
qqline(resid(FDis.GLMA))
# Checking ANOVA assumption number 2
ks.test(resid(FDis.GLMA),"pnorm",mean=mean(resid(FDis.GLMA)),sd=sd(resid(FDis.GLMA))) 
# Another normalcy test of populations to check ANOVA assumption 1 
shapiro.test(resid(FDis.GLMA))


### RUNNING GLMM FOR FEVE ###
FEve.GLMA <- lmer(FEve~Disturbance+(1|Site), data=fdiv)
summary(FEve.GLMA)
# Checking ANOVA assumption 2: how normal the residuals are
qqnorm(resid(FEve.GLMA))
qqline(resid(FEve.GLMA))
# Checking ANOVA assumption number 2
ks.test(resid(FEve.GLMA),"pnorm",mean=mean(resid(FEve.GLMA)),sd=sd(resid(FEve.GLMA))) 
# Another normalcy test of populations to check ANOVA assumption 1


### RUNNING GLMM FOR RAOQ ###
RaoQ.GLMA <- lmer(RaoQ~Disturbance+(1|Site), data=fdiv)
summary(RaoQ.GLMA)
# Checking ANOVA assumption 2: how normal the residuals are
qqnorm(resid(RaoQ.GLMA))
qqline(resid(RaoQ.GLMA))
# Checking ANOVA assumption number 2
ks.test(resid(RaoQ.GLMA),"pnorm",mean=mean(resid(RaoQ.GLMA)),sd=sd(resid(RaoQ.GLMA))) 
# Another normalicy test of populations to check ANOVA assumption 1 
shapiro.test(resid(RaoQ.GLMA))






                                               ### 2020 Community Weighted Mean Traits ###

### RUNNING GLMM FOR MAX HEIGHT ###
Max.Height.GLMA <- lmer(Max.Height~Disturbance+(1|Site), data=CWM)
summary(Max.Height.GLMA)
# Checking ANOVA assumption 2: how normal the residuals are
qqnorm(resid(Max.Height.GLMA))
qqline(resid(Max.Height.GLMA))
# Checking ANOVA assumption number 2
ks.test(resid(Max.Height.GLMA),"pnorm",mean=mean(resid(Max.Height.GLMA)),sd=sd(resid(Max.Height.GLMA))) 
# Another normalicy test of populations to check ANOVA assumption 1 


### RUNNING GLMM FOR SPECIFIC LEAF AREA ###
SLA.Leaf.GLMA <- lmer(SLA.Leaf~Disturbance+(1|Site), data=CWM)
summary(SLA.Leaf.GLMA)
# Checking ANOVA assumption 2: how normal the residuals are
qqnorm(resid(SLA.Leaf.GLMA))
qqline(resid(SLA.Leaf.GLMA))
# Checking ANOVA assumption number 2: 
ks.test(resid(SLA.Leaf.GLMA),"pnorm",mean=mean(resid(SLA.Leaf.GLMA)),sd=sd(resid(SLA.Leaf.GLMA))) 
# Another normalicy test of populations to check ANOVA assumption 1
shapiro.test(resid(SLA.Leaf.GLMA))


### RUNNING GLMM FOR SPECIFIC ROOT LENGTH ###
SRL.GLMA <- lmer(SRL~Disturbance+(1|Site), data=CWM)
summary(SRL.GLMA)
# Checking ANOVA assumption 2: how normal the residuals are
qqnorm(resid(SRL.GLMA))
qqline(resid(SRL.GLMA))
# Checking ANOVA assumption number 2
ks.test(resid(SRL.GLMA),"pnorm",mean=mean(resid(SRL.GLMA)),sd=sd(resid(SRL.GLMA))) 
# Another normalicy test of populations to check ANOVA assumption 1 
shapiro.test(resid(SRL.GLMA))



### RUNNING GLMM FOR CANOPY WIDTH ###
Canopy.W1.GLMA <- lmer(Canopy.W1.Transformed~Disturbance+(1|Site), data=CWM)
summary(Canopy.W1.GLMA)
# Checking ANOVA assumption 2: how normal the residuals are 
qqnorm(resid(Canopy.W1.GLMA))
qqline(resid(Canopy.W1.GLMA))
# Checking ANOVA assumption number 2
ks.test(resid(Canopy.W1.GLMA),"pnorm",mean=mean(resid(Canopy.W1.GLMA)),sd=sd(resid(Canopy.W1.GLMA))) 
# Another normalicy test of populations to check ANOVA assumption 1 
shapiro.test(resid(Canopy.W1.GLMA))


### RUNNING GLMM FOR LEAF NITROGEN CONTENT ###  
N.shoot.GLMA <- lmer(N.shoot~Disturbance+(1|Site), data=CWM)
summary(N.shoot.GLMA)
# Checking ANOVA assumption 2: how normal the residuals are
qqnorm(resid(N.shoot.GLMA))
qqline(resid(N.shoot.GLMA))
# Checking ANOVA assumption number 2
ks.test(resid(N.shoot.GLMA),"pnorm",mean=mean(resid(N.shoot.GLMA)),sd=sd(resid(N.shoot.GLMA))) 
# Another normalicy test of populations to check ANOVA assumption 1 
shapiro.test(resid(N.shoot.GLMA))


### RUNNING GLMM FOR ROOT NITROGEN CONTENT ###  
N.root.GLMA <- lmer(N.root~Disturbance+(1|Site), data=CWM)
summary(N.root.GLMA)
# Checking ANOVA assumption 2: how normal the residuals are
qqnorm(resid(N.root.GLMA))
qqline(resid(N.root.GLMA))
# Checking ANOVA assumption number 2
ks.test(resid(N.root.GLMA),"pnorm",mean=mean(resid(N.root.GLMA)),sd=sd(resid(N.root.GLMA))) 
# Another normalicy test of populations to check ANOVA assumption 1 
shapiro.test(resid(N.root.GLMA))







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
