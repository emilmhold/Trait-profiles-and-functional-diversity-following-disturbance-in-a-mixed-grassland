setwd("~/Documents/Academic/Side Projects/Elly")
library(vegan)
library(RVAideMemoire)


data<-read.csv('EllyPaper_FinData.csv',header=T)

spp.mat<-data[,8:70]

mds.com<-metaMDS(spp.mat,distance="bray",k=2,trymax=1000,trace=F,autotransform = F)
mds.com
stressplot(mds.com)

plot.des<-data[,1:7]%>%
  dplyr::rename('Plot'=Subplot,'Block'=Plot)
mds.df<-cbind(plot.des,mds.com$points)

ggplot(mds.df,aes(x=MDS1,y=MDS2,col=Plot))+
  scale_colour_manual(values = c('Disturbed'='grey33','Undisturbed'='grey75'))+
  #geom_text(aes(label=Block))+
  geom_point()+
  theme_classic()+
  theme(panel.border = element_rect(fill=NA))

diss.mat.A<-vegdist(spp.mat, method="bray")
adonis2(diss.mat.A~Subplot,strata = Plot, data=data)
