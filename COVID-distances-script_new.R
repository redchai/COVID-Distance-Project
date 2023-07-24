`###########
#Source code for
#Recommended distances for physical distancing
#during COVID-19 pandemics reveal cultural links between countries
###########

#Code information (as of 19/07/23) - table of contents

#I.Setup
#0.Setting the environment, data input for the recommended distance, and changing into metric values
#1.Distribution of the recommended distance
#2.Visualization of the recommended distances on a world map

#II. Data retrieval for each category & visualization
#3.Data for interpersonal distances
#4.Data for population density
#5.Data for colonization history
#6.Adding distances of the legal systems adopted in each country
#7.Distances of the first official language used in each country
#8.Distancing measures of different continents
#9.Recommended distances correlation with currency union
#10.Recommended distances in former SARS spiked countries
#11.Transmissibility data for the recommended distance from Arroyo-Marioli et al. (May and Aug 2020)
#12.Smoothened new case per population & Reproduction rate data from Ritchie et al (May and Aug 2020)

#III. Statistical analysis
#13.Performing one-way anova tests for each factors 
#14.Making a table including all the factors for the binomial generalized mixed model
#15.Binomial GLM for numerical variables
#16.Performing Binomial GLMM (mixed model) for all variables

#0 Environment setting
# install packages, fonts, setting up the working directory
#install.packages("sp")
#install.packages("rworldmap")
#install.packages("gridGraphics")
#install.packages("ggpubr")
#install.packages("ggplot2")
#install.packages("cowplot")
#install.packages("readr")
#install.packages("gridExtra")
#install.packages("extrafont")
#install.packages("extrafontdb")
#install.packages("remotes")
#install.packages("ggsignif")
#install.packages("DescTools")
#install.packages("effects")
#install.packages("car")
#install.packages("poolr")

library(sp)
library(gplots)
library(ggplot2)
library(rworldmap)
library(gridGraphics)
library(cowplot)
library(readr)
library(gridExtra)
library(remotes)
library(ggpubr)
library(ggsignif)
library(extrafont)
library(corrplot)
library(DescTools)
library(effects)
library(gplots)
library(car)
library(poolr)

par(mar=c(5,5,4,2))

remotes::install_version("Rttf2pt1", version = "1.3.12")
font_import() 
loadfonts(device="postscript")
loadfonts()
setwd("/Users/mac/Desktop/Research_Projects")
DistData=read.csv("COVID-Distances8.csv",header=T,sep=",",stringsAsFactors=FALSE)
DistData=as.data.frame(DistData)

#Processing the input file of all recommended distances 
###########
#remove all the non-sovereign countries
DistData=DistData[DistData$Average.dist!="NAP",]
#nrow(DistData)
#there are 197 sovereign countries in our list.

#according to https://www.worldometers.info/geography/how-many-countries-are-there-in-the-world/
#there are currently 195 countries.
#View(DistData$Region.subregion.country.area)
#We have them all and our list also includes Hong Kong and Taiwan.
#nrow(DistData[DistData$Metric.average.dist=="none"|DistData$Metric.average.dist=="ND",])
# 13 countries do not have a recommended distance
#we collected distances for 195 countries (193 sovereign countries + Hong Kong and Taiwan).

#changing the imperical systems to the metric scale
for (n in 1:nrow(DistData)) {
  DistData$Metric.average.dist[n]="nd"}
for (n in 1:nrow(DistData)) {
  metric=DistData$Average.dist[n]
  if (metric==1.8) {metric=2}
  if (metric==1.35) {metric=1.5}
  DistData$Metric.average.dist[n]=metric
}


#this data is with countries with no recommended distances, 
#so we make another list not including the unidentified values
DistDataWithoutND=DistData[DistData$Metric.average.dist!="none",]
DistDataWithoutND=DistDataWithoutND[DistDataWithoutND$Metric.average.dist!="ND",]

#for binomial generalized mixed models, the distances should be binomial
#changing metric distances into binomial distances (over 1m into 1, 1m into 0)
for (n in 1:nrow(DistDataWithoutND)) {
  DistDataWithoutND$Binomial.dist[n]="nd"}
for (n in 1:nrow(DistDataWithoutND)) {
  metricdist=DistDataWithoutND$Metric.average.dist[n]
  if (metricdist==2) {metricdist=1}
  else if (metricdist==1.5) {metricdist=1}
  else if (metricdist==1.25) {metricdist=1}
  else if (metricdist==1.75) {metricdist=1}
  else if (metricdist==1){metricdist=0}
  DistDataWithoutND$Binomial.dist[n]=metricdist}

#turning characters into numeric values
DistDataWithoutND$Binomial.dist=as.numeric(DistDataWithoutND$Binomial.dist)
DistDataWithoutND$Metric.average.dist=as.numeric(DistDataWithoutND$Metric.average.dist)

#we also make a list not including the data with a recommended distance of 1.25 and 1.75
DistDataSimple=DistDataWithoutND[DistDataWithoutND$Metric.average.dist!="1.25",]
DistDataSimple=DistDataSimple[DistDataSimple$Metric.average.dist!="1.75",]

#countries for which we are missing the recommended distances:
DistData[DistData$Average.dist=="ND",]$Region.subregion.country.area
#Nepal and Tuvalu needs distance data

#countries which did not recommend social distancing:
DistData[DistData$Recommended.distance=="no physical distanciation",]$Region.subregion.country.area
#Afghanistan and Eritrea did not recommend social distancing
#xxxCheck Eritrea&Afgahnistan&Nepal&Tuvalu for exact social distancing 



#1.Distribution of the recommended distances 
##########
#View(table(DistData$Rec.distance.m))
#The most recommended distances are 1m and then 2m.
#remove countries for which the recommended distance was not obtained:
DistData=DistData[DistData$Average.dist!="ND",]
#View(prop.table(table(DistData$Rec.distance.m)))
#The 1-m recommendation represents 44% of the countries and 2-m 30% of the countries.
DistList=as.data.frame(table(DistData$Rec.distance.m))
#View(DistList)

DistList=arrange(DistList,-row_number())
panelA=
  DistList %>%
#  arrange(Freq) %>%
  mutate(Var1=factor(Var1,Var1)) %>%
  ggplot(aes(x=Var1, y=Freq) ) +
  geom_segment(aes(x=Var1,xend=Var1, y=0, yend=Freq), color="grey") +
  geom_point(size=2, color="antiquewhite4") +
  #coord_flip() +
#  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title=element_text(size=11
 #                               ,face="bold"
                                ), 
        axis.text.x=element_text(size=9),
        axis.text.y=element_text(size=9),
        axis.line.y = element_line(colour = "grey", 
                                 size = 0.5, linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
  ) +
  #theme(text=element_text(family="Times"))+
  #when preparing a paper figure
  theme(text=element_text(family="Arial"))+
  xlab("Recommended Distances")+
  ylab("Number of countries")+
  rotate_x_text(angle=45)
  

ggdraw() +  draw_plot(panelA, x = 0.66, y = 0, width = 0.3, height = 0.417) 
#figure 1A

#2.Visualization of the recommended distances on a world map
###########

#check the names of the countries stored in rworldmap package to make sure they are the same as in our input file
require(rworldmap)
data(countryExData)
countries=countryExData[, 2]
#View(countries)
#View(countryExData)
#matched=joinCountryData2Map(DistData, joinCode="ISO3", nameJoinColumn="Country.code")
#matched=joinCountryData2Map(DistData, joinCode="NAME", nameJoinColumn="Country.names.rworldmap", verbose=TRUE)
matched=joinCountryData2Map(DistData, joinCode="UN", nameJoinColumn="Country.code", verbose=TRUE)

#197 codes from your data successfully matched countries in the map
#0 codes from your data failed to match with a country code in the map
# 45 codes from the map weren't represented in your data


mapParams=mapCountryData(subset(matched, continent != "Antarctica"), nameColumnToPlot="Dist.rworldmap", 
               mapTitle="Recommended distances", 
               catMethod ="categorical", addLegend=FALSE, 
               colourPalette = c("lightpink","orange", "violet", "purple","cornflowerblue","blue","darkblue","grey"))
#"azure" removed because no "ND" values
do.call(addMapLegendBoxes, c(mapParams,title="distances"
                             #,x='top',horiz=TRUE
                             ))
#Export>Save as eps and then open in Inkscape to combine with Fig. 1A

#drawmap= function(matched) {
 # function() {mapParams=mapCountryData(subset(matched, continent != "Antarctica"), nameColumnToPlot="Rec.distance.m", 
 #                                      mapTitle="Recommended distances", 
 #                                      catMethod ="categorical", addLegend=FALSE, 
 #                                      colourPalette = c("lightpink","orange", "violet", "purple","cornflowerblue","blue","darkblue","azure","grey"))
 #           do.call(addMapLegendBoxes, c(mapParams,title="distances"
                                      #  ,x='top',horiz=TRUE
  #                  ))
 # }
#}
#drawmap(matched)
#


mapCountryData(matched, nameColumnToPlot="Rec.distance.m",
               mapTitle="Recommended distances in Europe", mapRegion="Europe", 
               colourPalette=c("lightpink","orange", "violet", "purple","cornflowerblue","blue","darkblue","azure","grey"), catMethod="categorical")




#3. Data for interpersonal distances
##########


#Data from 42 countries originating from  Sorokowska, A., Sorokowski, P., 
#Hilpert, P., Cantarero, K., Frackowiak, T., Ahmadi, K., ... & Blumen, S. (2017). 
#Preferred interpersonal distances: a global comparison. Journal of Cross-Cultural Psychology, 48(4), 577-592.

PersDist=read.csv("Table1-Sorokowska_revised.txt",header=T,sep=",",stringsAsFactors=FALSE)
PersDist=as.data.frame(PersDist)
PersDistRecDist=merge(DistDataWithoutND,PersDist, by.x="Region.subregion.country.area", by.y="Country",all=FALSE)
#all 42 countries matched 
#16 countries have 1m recommendation, 8 countries with 1.5m, and 18 countries who had a 2m recommendation

social.distances<-as.numeric(PersDistRecDist$Social.Distance.mean)
personal.distances<-as.numeric(PersDistRecDist$Personal.Distance.mean)
intimate.distances<-as.numeric(PersDistRecDist$Intimate.distance.mean)
MeanSocial<-mean(as.numeric(PersDistRecDist$Social.Distance.mean))
MeanPersonal<-mean(as.numeric(PersDistRecDist$Personal.Distance.mean))
MeanIntimate<-mean(as.numeric(PersDistRecDist$Intimate.distance.mean))


#The average of social distance of all countries were 105.07cm, the personal distance was 80.60cm, the intimate distance were 56.33cm

#Boxplot of each personal distances, by COVID-19 distances
#social distance
gg_data_s = data.frame(s_dist = PersDistRecDist$Social.Distance.mean,
                     dist = PersDistRecDist$Metric.average.dist)

gg_data_s$dist=as.character(gg_data_s$dist)

s1<-ggplot(data = gg_data_s, aes(x = dist, y = s_dist)) +
    geom_boxplot()+
    geom_point(shape=1,color="black", size=2)+
    theme_classic()+
    theme(
      axis.text=element_text(size=12,family="Arial",color="black"))+
    theme(
      text=element_text(size=12,family="Arial",color="black"))+
    xlab("Recommended distance (m)")+
     ylab("Social distance (cm)")

  
s1

#personal distance
gg_data_p = data.frame(p_dist = PersDistRecDist$Personal.Distance.mean,
                      dist = PersDistRecDist$Metric.average.dist)

gg_data_p$dist=as.character(gg_data_p$dist)

p1<-ggplot(data=gg_data_p,aes(x=dist,y=p_dist))+
  geom_boxplot()+
  geom_point(shape=1,color="black",size=2)+
  theme_classic()+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(
    text=element_text(size=12,family="Arial",color="black"))+
  xlab("Recommended distance (m)")+
  ylab("Personal distance (cm)")

p1

#intimate distance
gg_data_i = data.frame(i_dist = PersDistRecDist$Intimate.distance.mean,
                     dist = PersDistRecDist$Metric.average.dist)

gg_data_i$dist=as.character(gg_data_i$dist)

i1<-ggplot(data=gg_data_i,aes(x=dist,y=i_dist))+
  geom_boxplot()+
  geom_point(shape=1,color="black",size=2)+
  theme_classic()+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(
    text=element_text(size=12,family="Arial",color="black"))+
  xlab("Recommended distance (m)")+
  ylab("Intimate distance (cm)")

i1
#checking correlation between each interpersonal distances
interpersonalcor1<-cor(PersDistRecDist$Intimate.distance.mean,PersDistRecDist$Social.Distance.mean,method='pearson')
interpersonalcor1
interpersonalcor1$p.value
interpersonalcor2<-cor(PersDistRecDist$Intimate.distance.mean,PersDistRecDist$Personal.Distance.mean,method='pearson')
interpersonalcor2
interpersonalcor3<-cor(PersDistRecDist$Social.Distance.mean,PersDistRecDist$Personal.Distance.mean,method='pearson')
interpersonalcor3


#4. Data for population density
##########
#remove cases with no values
#we use the DistDataSimple without 1.25 and 1.75m, because the explanatory variable is the recommended distance
DensityData=DistDataSimple

#Remove outliers, which are above 1000
DensityData=DensityData[DensityData$Population.density.2020<=1000,]

#Boxplot of the population density
pd_data = data.frame(dist = DensityData$Metric.average.dist,
                       density = DensityData$Population.density.2020)
pd_data$dist=as.character(pd_data$dist)

pd1<-ggplot(data=pd_data,aes(x=dist,y=density))+
  geom_boxplot()+
  geom_point(shape=1,color="black",size=2)+
  theme_classic()+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(
    text=element_text(size=12,family="Arial",color="black"))+
  xlab("Recommended distance (m)")+
  ylab(expression(paste("Population density (inhab/",km^2,")")))

pd1

#5. Data for colonization history
##########
GeoDist=read.csv("geo_cepii2_revised.csv",header=T,sep=",",stringsAsFactors=FALSE)
GeoDist=as.data.frame(GeoDist)
DistDataGeoDist=merge(GeoDist,DistDataWithoutND, by.x="cnum", by.y="Country.code")
#178 countries

DistDataColonies=DistDataGeoDist[DistDataGeoDist$colonizer1!=".",]
#Removing the countries that hasn't been colonized 


#add a column with the recommended distance of the colonizer
for (n in 1:nrow(DistDataColonies)) {
  DistDataColonies$distcolonizer1[n]="nd"}

for (n in 1:nrow(DistDataColonies)) {
  colonizer=DistDataColonies$colonizer1[n]
  distcolonizer1=DistDataGeoDist[DistDataGeoDist$iso3==colonizer,]$Metric.average.dist
  DistDataColonies$distcolonizer1[n]=as.character(distcolonizer1)
  }

#we used data of countries which colonized more than 5 countries for ANOVA test - total of 133 countries
DistDataColonies2<-DistDataGeoDist
DistDataColonies2<-DistDataColonies2[is.element(DistDataColonies2$colonizer1, c('FRA','ESP','RUS','PRT','GBR','TUR')),]

#assigning numerical varaibles to each colonizers for the binomial GLM
DistDataColonies3<-DistDataGeoDist
for (n in 1:nrow(DistDataColonies3)) {
  DistDataColonies3$colonyfactor[n]="nd"}

DistDataColonies3$colonyfactor=as.numeric(DistDataColonies3$colonyfactor)

for (i in 1:nrow(DistDataColonies3)){
  if(DistDataColonies3$colonizer1[i]=='FRA')
  {
    DistDataColonies3$colonyfactor[i]=-3
  }
  else if (DistDataColonies3$colonizer1[i]=='PRT')
  {
    DistDataColonies3$colonyfactor[i]=-2
  }
  else if (DistDataColonies3$colonizer1[i]=='ESP')
  {
    DistDataColonies3$colonyfactor[i]=-1
  }
  else if (DistDataColonies3$colonizer1[i]=='RUS')
  {
    DistDataColonies3$colonyfactor[i]=1
  }
  else if (DistDataColonies3$colonizer1[i]=='GBR')
  {
    DistDataColonies3$colonyfactor[i]=2
  }
  else if (DistDataColonies3$colonizer1[i]=='TUR')
  {
    DistDataColonies3$colonyfactor[i]=3
  }
  else 
  {
    DistDataColonies3$colonyfactor[i]=0
  }
}
for (i in 1:nrow(DistDataColonies3)){
  DistDataColonies3$colonyfactortemp[i]=DistDataColonies3$colonizer1[i]
}

for (i in 1:nrow(DistDataColonies3)){
  if(DistDataColonies3$colonyfactortemp[i]!='RUS'&DistDataColonies3$colonyfactortemp[i]!='TUR'&DistDataColonies3$colonyfactortemp[i]!='GBR'&DistDataColonies3$colonyfactortemp[i]!='ESP'&DistDataColonies3$colonyfactortemp[i]!='FRA'&DistDataColonies3$colonyfactortemp[i]!='PRT')
  {DistDataColonies3$colonyfactortemp[i]="Other Colony"}
}

DistDataColonies3$colonyfactortemp=as.factor(DistDataColonies3$colonyfactortemp)
DistDataColonies3$colonyfactortemp=relevel(DistDataColonies3$colonyfactortemp,ref="Other Colony")
data_colonies=data.frame(cnum=DistDataColonies3$cnum,colonyfactor=DistDataColonies3$colonyfactor)

#barplot of each colonizers
mesp<-mean(DistDataColonies2[DistDataColonies2$colonizer1=="ESP",]$Metric.average.dist)
eesp<-sd(DistDataColonies2[DistDataColonies2$colonizer1=="ESP",]$Metric.average.dist)/sqrt(length(DistDataColonies2[DistDataColonies2$colonizer1=="ESP",]$Metric.average.dist))
mfra<-mean(DistDataColonies2[DistDataColonies2$colonizer1=="FRA",]$Metric.average.dist)
efra<-sd(DistDataColonies2[DistDataColonies2$colonizer1=="FRA",]$Metric.average.dist)/sqrt(length(DistDataColonies2[DistDataColonies2$colonizer1=="FRA",]$Metric.average.dist))
mgbr<-mean(DistDataColonies2[DistDataColonies2$colonizer1=="GBR",]$Metric.average.dist)
egbr<-sd(DistDataColonies2[DistDataColonies2$colonizer1=="GBR",]$Metric.average.dist)/sqrt(length(DistDataColonies2[DistDataColonies2$colonizer1=="GBR",]$Metric.average.dist))
mprt<-mean(DistDataColonies2[DistDataColonies2$colonizer1=="PRT",]$Metric.average.dist)
eprt<-sd(DistDataColonies2[DistDataColonies2$colonizer1=="PRT",]$Metric.average.dist)/sqrt(length(DistDataColonies2[DistDataColonies2$colonizer1=="PRT",]$Metric.average.dist))
mrus<-mean(DistDataColonies2[DistDataColonies2$colonizer1=="RUS",]$Metric.average.dist)
erus<-sd(DistDataColonies2[DistDataColonies2$colonizer1=="RUS",]$Metric.average.dist)/sqrt(length(DistDataColonies2[DistDataColonies2$colonizer1=="RUS",]$Metric.average.dist))
mtur<-mean(DistDataColonies2[DistDataColonies2$colonizer1=="TUR",]$Metric.average.dist)
etur<-sd(DistDataColonies2[DistDataColonies2$colonizer1=="TUR",]$Metric.average.dist)/sqrt(length(DistDataColonies2[DistDataColonies2$colonizer1=="TUR",]$Metric.average.dist))



colonization_plot_table=data.frame(X=c("ESP","FRA","GBR","PRT","RUS","TUR"),Y=c(mesp,mfra,mgbr,mprt,mrus,mtur),error=c(eesp,efra,egbr,eprt,erus,etur))


colonization_plot=ggplot(data=colonization_plot_table,aes(x=X,y=Y,fill=X))+
  geom_errorbar(aes(ymin=Y,ymax=Y+error,width=0.2),position=position_dodge(width=0.8))+
  geom_bar(stat="summary",position=position_dodge(width=0.8),colour="white")+
  geom_bar(stat="summary",position=position_dodge(width=0.8),colour="black")+
  scale_fill_manual(values=c("grey70","white","black","grey70","white","black"))+
  scale_x_discrete("Colonizing country") +
  scale_y_continuous("Recommended distance (m)",expand=c(0,0),limits=c(0,2.05),breaks=seq(0,2,by=0.2))+
  theme_classic()+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(text=element_text(size=12,family="Arial",color="black"))+
  theme(legend.position="none")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_text(angle=90,hjust=0.5),
        #legend.title=element_blank()
  )

colonization_plot

#6. Adding distances of the legal systems adopted in each country
##########
#Table of each legal systems was extracted from http://www.juriglobe.ca/eng/langues/index-alpha.php
#'Brunei' was replaced by 'Brunei Darussalam'
#'Korea, South was replaced by 'Republic of Korea'
#'Korea, North was replaced by 'Dem.People's Republic of Korea'
#'Congo, popular democratic republic of' was replaced by 'Democratic Republic of the Congo'
#We added one line - 'South Sudan; mixed; arabic' because South Sudan was not represented as a legal entity in the file
#xxFor South Sudan, we have to confirm what legal systems are in South Sudan
#xxcheck exactly how the file was made
LegalDist=read.csv("LegalSystems2.txt",header=T,sep=";",stringsAsFactors=FALSE)
LegalDist=as.data.frame(LegalDist)
LegalDistRecDist=merge(LegalDist,DistDataWithoutND, by.x="Political.entities", by.y="Country.names.rworldmap")


# 75 civilist countries, 25 common law countries, 1 customary country, 85 mixed countries, 2 muslim countries
#add a column with synchronized distances between metric and imperical systems
LegalDistRecDist=as.data.frame(LegalDistRecDist)



#Performing ANOVA for the legal systems

#GLM_Legal=glm(BinomialDist~legal.systems,family=binomial,data=table_175_countries)
#summary(GLM_Legal)
#we remove the muslim & customary countries, which lack sufficient data 

legal_table=LegalDistRecDist
legal_table=legal_table[legal_table$legal.systems!=" muslim",]
legal_table=legal_table[legal_table$legal.systems!=" customary",]


#adding numeric values for legal systems for Binomial GLM
for (i in 1:nrow(legal_table)){
 legal_table$legalfactor[i]="nd"
}

for (i in 1:nrow(legal_table)){
  if(legal_table$legal.systems[i]==" mixed") 
  {
    legal_table$legalfactor[i]=-2
  }
  else if(legal_table$legal.systems[i]==" common law")
  {
    legal_table$legalfactor[i]=2
  }
  else if (legal_table$legal.systems[i]==" civilist")
    {
    legal_table$legalfactor[i]=0
  }
}
legal_table$legalfactor=as.numeric(legal_table$legalfactor)

for(i in 1:nrow(legal_table)){
  legal_table$legalfactortemp[i]=legal_table$legal.systems[i]
}

legal_table$legalfactortemp=as.factor(legal_table$legalfactortemp)
legal_table$legalfactortemp=relevel(legal_table$legalfactortemp,ref=" mixed")

#drawing a barplot, including significant values from the TukeyHSD test
mcivil<-mean(legal_table[legal_table$legal.systems==" civilist",]$Metric.average.dist)
ecivil<-sd(legal_table[legal_table$legal.systems==" civilist",]$Metric.average.dist)/sqrt(length(legal_table[legal_table$legal.systems==" civilist",]$Metric.average.dist))
mcommon<-mean(legal_table[legal_table$legal.systems==" common law",]$Metric.average.dist)
ecommon<-sd(legal_table[legal_table$legal.systems==" common law",]$Metric.average.dist)/sqrt(length(legal_table[legal_table$legal.systems==" common law",]$Metric.average.dist))
mmixed<-mean(legal_table[legal_table$legal.systems==" mixed",]$Metric.average.dist)
emixed<-sd(legal_table[legal_table$legal.systems==" mixed",]$Metric.average.dist)/sqrt(length(legal_table[legal_table$legal.systems==" mixed",]$Metric.average.dist))

legal_plot_table=data.frame(X=c("civilist","common law","mixed"),Y=c(mcivil,mcommon,mmixed),error=c(ecivil,ecommon,emixed))


legal_plot=ggplot(data=legal_plot_table,aes(x=X,y=Y,fill=X))+
  geom_errorbar(aes(ymin=Y,ymax=Y+error,width=0.2),position=position_dodge(width=0.8))+
  geom_bar(stat="summary",position=position_dodge(width=0.8),colour="white")+
  geom_bar(stat="summary",position=position_dodge(width=0.8),colour="black")+
  scale_fill_manual(values=c("grey70","white","black"))+
  scale_x_discrete("Legal system") +
  scale_y_continuous("Recommended distance (m)",expand=c(0,0),limits=c(0,2.2),breaks=seq(0,2,by=0.2))+
  theme_classic()+
  theme(legend.position="none")+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(
    text=element_text(size=12,family="Arial",color="black"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_text(angle=90,hjust=0.5),
        #legend.title=element_blank()
  )

legal_plot + geom_segment(aes(x=1, y=2.1, xend=2, yend=2.1), col = "gray80")+
  geom_segment(aes(x=1, y=2.05, xend=1, yend=2.1), col = "gray80")+
  geom_segment(aes(x=2, y=2.05, xend=2, yend=2.1), col = "gray80") +
  annotate("text", x=1.5, y=2.15, label="**")+
  geom_segment(aes(x=2, y=1.9, xend=2, yend=1.95), col = "gray80")+
  geom_segment(aes(x=3, y=1.9, xend=3, yend=1.95), col = "gray80")+
  geom_segment(aes(x=2, y=1.95, xend=3, yend=1.95), col = "gray80") +
  annotate("text", x=2.5, y=2, label="***")


#7. Distances of the first official language used in each country
##########
GeoDist2=read.csv("geo_cepii2_revised.csv",header=T,sep=",",stringsAsFactors=FALSE)
GeoDist2=as.data.frame(GeoDist2)
DistDataGeoDist2=merge(GeoDist2,DistDataWithoutND, by.x="cnum", by.y="Country.code")

#Holy See, Liechtenstein, Monaco, Serbia, South Sudan, State of Palestine does not have a country code in the Geo_cepii2 file
#xxwe have to check again for the 6 countries

#making a dataframe for the language 
language_table_glm=DistDataGeoDist2[c(35,17)]
language_table_glm=merge(DistDataWithoutND,language_table_glm,by.x="c",by.y="c")

for (i in 1:nrow(language_table_glm)){
  language_table_glm$languagefactor[i]=0
}
for (i in 1:nrow(language_table_glm)){
  if (language_table_glm$langoff_1[i]=="English"){
    language_table_glm$languagefactor[i]=2
  }
  else if (language_table_glm$langoff_1[i]=="French"){
    language_table_glm$languagefactor[i]=-3
  }
  else if (language_table_glm$langoff_1[i]=="Spanish"){
    language_table_glm$languagefactor[i]=1
  }
  else if (language_table_glm$langoff_1[i]=="Arabic"){
    language_table_glm$languagefactor[i]=-1
  }
  else if (language_table_glm$langoff_1[i]=="Dutch"){
    language_table_glm$languagefactor[i]=3
  }
  else if (language_table_glm$langoff_1[i]=="Portuguese"){
    language_table_glm$languagefactor[i]=-2
  }
}

for (i in 1:nrow(language_table_glm)){
  language_table_glm$languagefactortemp[i]=language_table_glm$langoff_1[i]
}
for (i in 1:nrow(language_table_glm)){
  if (language_table_glm$langoff_1[i]!="English"&language_table_glm$langoff_1[i]!="French"&language_table_glm$langoff_1[i]!="Spanish"&language_table_glm$langoff_1[i]!="Arabic"&language_table_glm$langoff_1[i]!="Dutch"&language_table_glm$langoff_1[i]!="Portuguese"){
    language_table_glm$languagefactortemp[i]="Other languages"
  }
}
language_table_glm$languagefactortemp=as.factor(language_table_glm$languagefactortemp)
language_table_glm$languagefactortemp=relevel(language_table_glm$languagefactortemp,ref="Other languages")

#we include countries which use French, English, Spanish, Arabic as their first official language, for data clarity
language_table<-subset(DistDataGeoDist2, langoff_1=="French" | langoff_1=="English" | langoff_1=="Arabic" | langoff_1=="Spanish")
#total of 101 countries


#drawing a barplot, including significant values from the TukeyHSD test
mfre<-mean(language_table[language_table$langoff_1=="French",]$Metric.average.dist)
efre<-sd(language_table[language_table$langoff_1=="French",]$Metric.average.dist)/sqrt(length(language_table[language_table$langoff_1=="French",]$Metric.average.dist))
meng<-mean(language_table[language_table$langoff_1=="English",]$Metric.average.dist)
eeng<-sd(language_table[language_table$langoff_1=="English",]$Metric.average.dist)/sqrt(length(language_table[language_table$langoff_1=="English",]$Metric.average.dist))
mara<-mean(language_table[language_table$langoff_1=="Arabic",]$Metric.average.dist)
eara<-sd(language_table[language_table$langoff_1=="Arabic",]$Metric.average.dist)/sqrt(length(language_table[language_table$langoff_1=="Arabic",]$Metric.average.dist))
mspa<-mean(language_table[language_table$langoff_1=="Spanish",]$Metric.average.dist)
espa<-sd(language_table[language_table$langoff_1=="Spanish",]$Metric.average.dist)/sqrt(length(language_table[language_table$langoff_1=="Spanish",]$Metric.average.dist))

language_plot_table=data.frame(X=c("French","English","Arabic","Spanish"),Y=c(mfre,meng,mara,mspa),error=c(efre,eeng,eara,espa))


language_plot=ggplot(data=language_plot_table,aes(x=X,y=Y,fill=X))+
  geom_errorbar(aes(ymin=Y,ymax=Y+error,width=0.2),position=position_dodge(width=0.8))+
  geom_bar(stat="summary",position=position_dodge(width=0.8),colour="white")+
  geom_bar(stat="summary",position=position_dodge(width=0.8),colour="black")+
  scale_fill_manual(values=c("grey70","white","black","grey70"))+
  scale_x_discrete("Language") +
  scale_y_continuous("Recommended distance (m)",expand=c(0,0),limits=c(0,2.05),breaks=seq(0,2,by=0.2))+
  theme_classic()+
  theme(legend.position="none")+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(
    text=element_text(size=12,family="Arial",color="black"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_text(angle=90,hjust=0.5),
        #legend.title=element_blank()
  )

language_plot + geom_segment(aes(x=3, y=1.9, xend=4, yend=1.9), col = "gray80")+
  geom_segment(aes(x=3, y=1.85, xend=3, yend=1.9), col = "gray80")+
  geom_segment(aes(x=4, y=1.85, xend=4, yend=1.9), col = "gray80") +
  annotate("text", x=3.5, y=1.95, label="*")+
  geom_segment(aes(x=2, y=1.7, xend=2, yend=1.75), col = "gray80")+
  geom_segment(aes(x=3, y=1.7, xend=3, yend=1.75), col = "gray80")+
  geom_segment(aes(x=2, y=1.75, xend=3, yend=1.75), col = "gray80") +
  annotate("text", x=2.5, y=1.8, label="**")



#8. Distancing measures of different continents
###########
#making a table for Binomial GLM
continent_table=DistDataGeoDist2

for (i in 1:nrow(continent_table)){
  continent_table$continentfactor[i]=0
}
for (i in 1:nrow(continent_table)){
  if (continent_table$continent[i]=="Europe"){
    continent_table$continentfactor[i]=2
  }
  else if (continent_table$continent[i]=="Africa"){
    continent_table$continentfactor[i]=-2
  }
  else if (continent_table$continent[i]=="Asia"){
    continent_table$continentfactor[i]=-1
  }
  else if (continent_table$continent[i]=="America"){
    continent_table$continentfactor[i]=1
  }
}


mafr<-1.209
eafr<-sd(DistDataGeoDist2[DistDataGeoDist2$continent=="Africa",]$Metric.average.dist)/sqrt(length(DistDataGeoDist2[DistDataGeoDist2$continent=="Africa",]$Metric.average.dist))
meur<-1.5875
eeur<-sd(DistDataGeoDist2[DistDataGeoDist2$continent=="Europe",]$Metric.average.dist)/sqrt(length(DistDataGeoDist2[DistDataGeoDist2$continent=="Europe",]$Metric.average.dist))
mpac<-1.5833
epac<-sd(DistDataGeoDist2[DistDataGeoDist2$continent=="Pacific",]$Metric.average.dist)/sqrt(length(DistDataGeoDist2[DistDataGeoDist2$continent=="Pacific",]$Metric.average.dist))
mame<-1.6015
eame<-sd(DistDataGeoDist2[DistDataGeoDist2$continent=="America",]$Metric.average.dist)/sqrt(length(DistDataGeoDist2[DistDataGeoDist2$continent=="America",]$Metric.average.dist))
continent_table_2=data.frame(X=c("Africa","Europe","Pacific","America"),Y=c(mafr,meur,mpac,mame),error=c(eafr,eeur,epac,eame))


continent_plot=ggplot(data=continent_table_2,aes(x=X,y=Y,fill=X))+
  geom_errorbar(aes(ymin=Y,ymax=Y+error,width=0.2),position=position_dodge(width=0.8))+
  geom_bar(stat="summary",position=position_dodge(width=0.8),colour="white")+
  geom_bar(stat="summary",position=position_dodge(width=0.8),colour="black")+
  scale_fill_manual(values=c("grey70","white","black","grey70"))+
  scale_x_discrete("Continent") +
  scale_y_continuous("Recommended distance (m)",expand=c(0,0),limits=c(0,2.05),breaks=seq(0,2,by=0.2))+
  theme_classic()+
  theme(legend.position="none")+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_text(angle=90,hjust=0.5),
        #legend.title=element_blank()
  )

continent_plot +
  geom_segment(aes(x=1, y=1.7, xend=1, yend=1.75), col = "gray80")+
  geom_segment(aes(x=2, y=1.7, xend=2, yend=1.75), col = "gray80")+
  geom_segment(aes(x=1, y=1.75, xend=2, yend=1.75), col = "gray80") +
  annotate("text", x=1.5, y=1.8, label="***")

#9. Recommended distances correlation with currency union
##########
Currency=read.csv("Currency.csv",header=T,sep=",",stringsAsFactors=FALSE)
Currency=as.data.frame(Currency)
Currency_table=merge(Currency,DistDataWithoutND, by.x="c", by.y="c")

#making another table for glm
currency_table_glm=merge(Currency,DistDataWithoutND,by.x="c",by.y="c",all=TRUE)
currency_table_glm=currency_table_glm[!is.na(currency_table_glm$Metric.average.dist),]

for (i in 1:nrow(currency_table_glm)){
if(is.na(currency_table_glm$Currency[i])==1){
  currency_table_glm$Currency[i]="Other currency"
}}

for (i in 1:nrow(currency_table_glm)){
  currency_table_glm$currencyfactor[i]=0
}

for (i in 1:nrow(currency_table_glm)){
  if(currency_table_glm$Currency[i]=="Euro"){
    currency_table_glm$currencyfactor[i]=2
  }
  else if(currency_table_glm$Currency[i]=="CFA franc"){
    currency_table_glm$currencyfactor[i]=-2
  }
  else if(currency_table_glm$Currency[i]=="United States dollar"){
    currency_table_glm$currencyfactor[i]=-1
  }
  else if(currency_table_glm$Currency[i]=="Eastern Caribbean dollar"){
    currency_table_glm$currencyfactor[i]=1
  }
}

for (i in 1:nrow(currency_table_glm)){
currency_table_glm$currencyfactortemp[i]=currency_table_glm$Currency[i]
}
for (i in 1:nrow(currency_table_glm)){
if(currency_table_glm$currencyfactortemp[i]!="Euro"&currency_table_glm$currencyfactortemp[i]!="CFA franc"&currency_table_glm$currencyfactortemp[i]!="United States dollar"&currency_table_glm$currencyfactortemp[i]!="Eastern Caribbean dollar"){
  currency_table_glm$currencyfactortemp[i]="Other Currency"
}
}
currency_table_glm$currencyfactortemp=as.factor(currency_table_glm$currencyfactortemp)
currency_table_glm$currencyfactortemp=relevel(currency_table_glm$currencyfactortemp,ref="Other Currency")
#assigning numerical values for each currency unions for binomial glm

#excluding countries with insufficient data (less than 5)
Currency_table=Currency_table[Currency_table$Currency!="Australian Dollars",]
Currency_table=Currency_table[Currency_table$Currency!="South African rand",]


#making a barplot for the currency union
mfranc<-mean(Currency_table[Currency_table$Currency=="CFA franc",]$Metric.average.dist)
efranc<-sd(Currency_table[Currency_table$Currency=="CFA franc",]$Metric.average.dist)/sqrt(length(Currency_table[Currency_table$Currency=="CFA franc",]$Metric.average.dist))
meastc<-mean(Currency_table[Currency_table$Currency=="Eastern Caribbean dollar",]$Metric.average.dist)
eeastc<-sd(Currency_table[Currency_table$Currency=="Eastern Caribbean dollar",]$Metric.average.dist)/sqrt(length(Currency_table[Currency_table$Currency=="Eastern Caribbean dollar",]$Metric.average.dist))
meuro<-mean(Currency_table[Currency_table$Currency=="Euro",]$Metric.average.dist)
eeuro<-sd(Currency_table[Currency_table$Currency=="Euro",]$Metric.average.dist)/sqrt(length(Currency_table[Currency_table$Currency=="Euro",]$Metric.average.dist))
musd<-mean(Currency_table[Currency_table$Currency=="United States dollar",]$Metric.average.dist)
eusd<-sd(Currency_table[Currency_table$Currency=="United States dollar",]$Metric.average.dist)/sqrt(length(Currency_table[Currency_table$Currency=="United States dollar",]$Metric.average.dist))
currency_table=data.frame(X=c("CFA\nfranc","Eastern\ncaribbean\ndollar","Euro","United\nstates\ndollar"),Y=c(mfranc,meastc,meuro,musd),error=c(efranc,eeastc,eeuro,eusd))

currency_plot=ggplot(data=currency_table,aes(x=X,y=Y,fill=X))+
  geom_errorbar(aes(ymin=Y,ymax=Y+error,width=0.2),position=position_dodge(width=0.8))+
  geom_bar(stat="summary",position=position_dodge(width=0.8),colour="white")+
  geom_bar(stat="summary",position=position_dodge(width=0.8),colour="black")+
  scale_fill_manual(values=c("grey70","white","black","grey70"))+
  scale_x_discrete("\nCurrency")+
  scale_y_continuous("Recommended distance (m)",expand=c(0,0),limits=c(0,2.1),breaks=seq(0,2,by=0.2))+
  theme_classic()+
  theme(legend.position="none")+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_text(angle=90,hjust=0.5),
        )

currency_plot +
  geom_segment(aes(x=1, y=1.9, xend=1, yend=1.95), col = "gray80")+
  geom_segment(aes(x=2, y=1.9, xend=2, yend=1.95), col = "gray80")+
  geom_segment(aes(x=1, y=1.95, xend=2, yend=1.95), col = "gray80") +
  annotate("text", x=1.5, y=2, label="***")

#10. Recommended distances in former SARS spiked countries
##########
SARS_data=read.csv("SARS.csv",header=T,sep=",",stringsAsFactors = FALSE)
SARS_data=as.data.frame(SARS_data)
SARS_data_2=merge(SARS_data,DistDataWithoutND, by.x="Areas",by.y="Region.subregion.country.area",all=TRUE)

#making another table for the binomial GLM, with all values
SARS_data_2_glm=merge(SARS_data,DistDataWithoutND, by.x="Areas",by.y="Region.subregion.country.area",all=TRUE)

for (i in 1:nrow(SARS_data_2)){
  if(is.na(SARS_data_2[i,2])==1)
  {
    SARS_data_2[i,2]="0"
  }
  else{
    SARS_data_2[i,2]="1"
  }
}
SARS_data_2=SARS_data_2[-c(1),]
colnames(SARS_data_2)[2]=c("Exposure")



#make a table with countries which have not experienced SARS
No_SARS_data=DistData[1,]
#making a temporary table with only one column

for (i in 1:nrow(DistData)){
  z<-1
  for (j in 1:nrow(SARS_data)){
    if(SARS_data$Areas[j]==DistData$Region.subregion.country.area[i])
    z=0
  }
  if(z=="1")
  No_SARS_data[i,]=DistData[i,]
}
#omitting the data which are NA's
SARS_data_2=subset(SARS_data_2,select=-c(contact.noncontact))
SARS_data_2=na.omit(SARS_data_2)

No_SARS_data = subset(No_SARS_data, select = -c(contact.noncontact))
No_SARS_data=na.omit(No_SARS_data)

mean(as.numeric(SARS_data_2$Metric.average.dist))
#1.428m

#making a barplot
mexposed<-mean(SARS_data_2[SARS_data_2$Exposure==1,]$Metric.average.dist)
eexposed<-sd(SARS_data_2[SARS_data_2$Exposure==1,]$Metric.average.dist)/sqrt(length(SARS_data_2[SARS_data_2$Exposure==1,]$Metric.average.dist))
mnotexposed<-mean(SARS_data_2[SARS_data_2$Exposure==0,]$Metric.average.dist)
enotexposed<-sd(SARS_data_2[SARS_data_2$Exposure==0,]$Metric.average.dist)/sqrt(length(SARS_data_2[SARS_data_2$Exposure==0,]$Metric.average.dist))

exposure_table=data.frame(X=c("Exposed","Not exposed"),Y=c(mexposed,mnotexposed),error=c(eexposed,enotexposed))

exposure_plot=ggplot(data=exposure_table,aes(x=X,y=Y,fill=X))+
  geom_errorbar(aes(ymin=Y,ymax=Y+error,width=0.2),position=position_dodge(width=0.8))+
  geom_bar(stat="summary",position=position_dodge(width=0.8),colour="white")+
  geom_bar(stat="summary",position=position_dodge(width=0.8),colour="black")+
  scale_fill_manual(values=c("grey70","white"))+
  scale_x_discrete("\nPrevious exposure to SARS")+
  scale_y_continuous("Recommended distance (m)",expand=c(0,0),limits=c(0,2.1),breaks=seq(0,2,by=0.2))+
  theme_classic()+
  theme(axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(legend.position="none")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_text(angle=90,hjust=0.5),
  )

exposure_plot


#11.Transmissibility data for the recommended distance  (142 countries)
#Total of 147 countries from the paper "Tracking R of COVID-19: A new real-time estimation using the Kalman filter"
##########

urlfile="https://raw.githubusercontent.com/crondonm/TrackingR/main/Estimates-Database/database.csv"
#11-1. Taking data from May 8th, 2020
R_tracking_data_May_2020<-read.csv(url(urlfile))
R_tracking_data_May_2020_2<-R_tracking_data_May_2020[1,]

for (i in 1:nrow(R_tracking_data_May_2020)){
  if(R_tracking_data_May_2020$Date[i]=="2020-05-08")
  {
    R_tracking_data_May_2020_2[i,]=R_tracking_data_May_2020[i,]
  }
}
R_tracking_data_May_2020_2=na.omit(R_tracking_data_May_2020_2)
#taking data only from which people are infectious for 7 days
R_tracking_data_May_2020_3<-R_tracking_data_May_2020_2[1,]
for (i in 1:nrow(R_tracking_data_May_2020_2)){
  if(R_tracking_data_May_2020_2$days_infectious[i]=="7")
  {
    R_tracking_data_May_2020_3[i,]=R_tracking_data_May_2020_2[i,]
  }
}
R_tracking_data_May_2020_3=na.omit(R_tracking_data_May_2020_3)
R_tracking_data_May_2020_3=R_tracking_data_May_2020_3[c(-1),]
#Changing the countries' name according to the DistData file
for (i in 1:nrow(R_tracking_data_May_2020_3)){
  if(R_tracking_data_May_2020_3$Country.Region[i]=="Vietnam")
  {
    R_tracking_data_May_2020_3$Country.Region[i]="Viet Nam"
  }
  else if(R_tracking_data_May_2020_3$Country.Region[i]=="South Korea")
  {
    R_tracking_data_May_2020_3$Country.Region[i]="Republic of Korea"
  }
  else if(R_tracking_data_May_2020_3$Country.Region[i]=="United States")
  {
    R_tracking_data_May_2020_3$Country.Region[i]="United States of America"
  }
  else if(R_tracking_data_May_2020_3$Country.Region[i]=="Russia")
  {
    R_tracking_data_May_2020_3$Country.Region[i]="Russian Federation"
  }
  else if(R_tracking_data_May_2020_3$Country.Region[i]=="Moldova")
  {
    R_tracking_data_May_2020_3$Country.Region[i]="Republic of Moldova"
  }
  else if(R_tracking_data_May_2020_3$Country.Region[i]=="Czechia")
  {
    R_tracking_data_May_2020_3$Country.Region[i]="Czech Rep."
  }
  else if(R_tracking_data_May_2020_3$Country.Region[i]=="Burma")
  {
    R_tracking_data_May_2020_3$Country.Region[i]="Myanmar"
  }
  else if(R_tracking_data_May_2020_3$Country.Region[i]=="Dominican Republic")
  {
    R_tracking_data_May_2020_3$Country.Region[i]="Dominican Rep."
  }
  else if(R_tracking_data_May_2020_3$Country.Region[i]=="Tanzania")
  {
    R_tracking_data_May_2020_3$Country.Region[i]="United Republic of Tanzania"
  }
  else if(R_tracking_data_May_2020_3$Country.Region[i]=="Venezuela")
  {
    R_tracking_data_May_2020_3$Country.Region[i]="Venezuela (Bolivarian Republic of)"
  }
  else if(R_tracking_data_May_2020_3$Country.Region[i]=="Brunei")
  {
    R_tracking_data_May_2020_3$Country.Region[i]="Brunei Darussalam"
  }
}

#only 147 countries are taken into observation in this table
R_tracking_data_May_2020_4=merge(R_tracking_data_May_2020_3,DistDataSimple, by.x="Country.Region",by.y="Region.subregion.country.area")
#We used the distance data without 1.25m, 1.75m because the recommended distance was the explanatory variable 
#merged into 142 countries


# Making a figure for the transmissibility data
R_tracking_df_May = data.frame(distance = R_tracking_data_May_2020_4$Metric.average.dist,
                               Rvalue = R_tracking_data_May_2020_4$R)

R_tracking_df_May$distance=as.character(R_tracking_df_May$distance)

Rdf1_May<-ggplot(data=R_tracking_df_May,aes(x=distance,y=Rvalue))+
  geom_boxplot()+
  geom_point(shape=1,color="black",size=2)+
  theme_classic()+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(
    text=element_text(size=12,family="Arial",color="black"))+
  xlab("Recommended distance (m)")+
  ylab("R value")

Rdf1_May

#11-2. Taking data from Aug 1, 2020
R_tracking_data_Aug_2020<-read.csv(url(urlfile))
R_tracking_data_Aug_2020_2<-R_tracking_data_Aug_2020[1,]

for (i in 1:nrow(R_tracking_data_Aug_2020)){
  if(R_tracking_data_Aug_2020$Date[i]=="2020-08-01")
  {
    R_tracking_data_Aug_2020_2[i,]=R_tracking_data_Aug_2020[i,]
  }
}
R_tracking_data_Aug_2020_2=na.omit(R_tracking_data_Aug_2020_2)
#taking data only from which people are infectious for 7 days
R_tracking_data_Aug_2020_3<-R_tracking_data_Aug_2020_2[1,]
for (i in 1:nrow(R_tracking_data_Aug_2020_2)){
  if(R_tracking_data_Aug_2020_2$days_infectious[i]=="7")
  {
    R_tracking_data_Aug_2020_3[i,]=R_tracking_data_Aug_2020_2[i,]
  }
}
R_tracking_data_Aug_2020_3=na.omit(R_tracking_data_Aug_2020_3)
R_tracking_data_Aug_2020_3=R_tracking_data_Aug_2020_3[c(-1),]
#Changing the countries' name according to the DistData file
for (i in 1:nrow(R_tracking_data_Aug_2020_3)){
  if(R_tracking_data_Aug_2020_3$Country.Region[i]=="Vietnam")
  {
    R_tracking_data_Aug_2020_3$Country.Region[i]="Viet Nam"
  }
  else if(R_tracking_data_Aug_2020_3$Country.Region[i]=="South Korea")
  {
    R_tracking_data_Aug_2020_3$Country.Region[i]="Republic of Korea"
  }
  else if(R_tracking_data_Aug_2020_3$Country.Region[i]=="United States")
  {
    R_tracking_data_Aug_2020_3$Country.Region[i]="United States of America"
  }
  else if(R_tracking_data_Aug_2020_3$Country.Region[i]=="Russia")
  {
    R_tracking_data_Aug_2020_3$Country.Region[i]="Russian Federation"
  }
  else if(R_tracking_data_Aug_2020_3$Country.Region[i]=="Moldova")
  {
    R_tracking_data_Aug_2020_3$Country.Region[i]="Republic of Moldova"
  }
  else if(R_tracking_data_Aug_2020_3$Country.Region[i]=="Czechia")
  {
    R_tracking_data_Aug_2020_3$Country.Region[i]="Czech Rep."
  }
  else if(R_tracking_data_Aug_2020_3$Country.Region[i]=="Burma")
  {
    R_tracking_data_Aug_2020_3$Country.Region[i]="Myanmar"
  }
  else if(R_tracking_data_Aug_2020_3$Country.Region[i]=="Dominican Republic")
  {
    R_tracking_data_Aug_2020_3$Country.Region[i]="Dominican Rep."
  }
  else if(R_tracking_data_Aug_2020_3$Country.Region[i]=="Tanzania")
  {
    R_tracking_data_Aug_2020_3$Country.Region[i]="United Republic of Tanzania"
  }
  else if(R_tracking_data_Aug_2020_3$Country.Region[i]=="Venezuela")
  {
    R_tracking_data_Aug_2020_3$Country.Region[i]="Venezuela (Bolivarian Republic of)"
  }
  else if(R_tracking_data_Aug_2020_3$Country.Region[i]=="Brunei")
  {
    R_tracking_data_Aug_2020_3$Country.Region[i]="Brunei Darussalam"
  }
  }

#only 147 countries are taken into observation in this table
R_tracking_data_Aug_2020_4=merge(R_tracking_data_Aug_2020_3,DistDataSimple, by.x="Country.Region",by.y="Region.subregion.country.area")
#We used the distance data without 1.25m, 1.75m because the recommended distance was the explanatory variable 
#merged into 142 countries


#Making a figure for the transmissibility data
R_tracking_df_Aug = data.frame(distance = R_tracking_data_Aug_2020_4$Metric.average.dist,
                              Rvalue = R_tracking_data_Aug_2020_4$R)

R_tracking_df_Aug$distance=as.character(R_tracking_df_Aug$distance)

Rdf1_Aug<-ggplot(data=R_tracking_df_Aug,aes(x=distance,y=Rvalue))+
  geom_boxplot()+
  geom_point(shape=1,color="black",size=2)+
  theme_classic()+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(
    text=element_text(size=12,family="Arial",color="black"))+
  xlab("Recommended distance (m)")+
  ylab("R value")

Rdf1_Aug


#12. Testing the relation of recommended distance to the smoothended new case per population 
#12-1Taking data from May 2020
Case_data_2020_May=read.csv("owid-covid-data.csv",header=T,sep=",",stringsAsFactors = FALSE)
Case_data_2020_May_1<-Case_data_2020_May[1,]
for (i in 1:nrow(Case_data_2020_May)){
  if(Case_data_2020_May$date[i]=="2020-05-08")
  {
    Case_data_2020_May_1[i,]=Case_data_2020_May[i,]
  }
}
Case_data_2020_May_1=Case_data_2020_May_1[,c('location','new_cases_smoothed_per_million','reproduction_rate')]
Case_data_2020_May_1=na.omit(Case_data_2020_May_1)
for (i in 1:nrow(Case_data_2020_May_1)){
  if(Case_data_2020_May_1$location[i]=="Vietnam")
  {
    Case_data_2020_May_1$location[i]="Viet Nam"
  }
  else if(Case_data_2020_May_1$location[i]=="South Korea")
  {
    Case_data_2020_May_1$location[i]="Republic of Korea"
  }
  else if(Case_data_2020_May_1$location[i]=="United States")
  {
    Case_data_2020_May_1$location[i]="United States of America"
  }
  else if(Case_data_2020_May_1$location[i]=="Russia")
  {
    Case_data_2020_May_1$location[i]="Russian Federation"
  }
  else if(Case_data_2020_May_1$location[i]=="Cape Verde")
  {
    Case_data_2020_May_1$location[i]="Cabo Verde"
  }
  else if(Case_data_2020_May_1$location[i]=="Czechia")
  {
    Case_data_2020_May_1$location[i]="Czech Rep."
  }
  else if(Case_data_2020_May_1$location[i]=="Democratic Republic of Congo")
  {
    Case_data_2020_May_1$location[i]="Democratic Republic of the Congo"
  }
  else if(Case_data_2020_May_1$location[i]=="Dominican Republic")
  {
    Case_data_2020_May_1$location[i]="Dominican Rep."
  }
  else if(Case_data_2020_May_1$location[i]=="Micronesia (country)")
  {
    Case_data_2020_May_1$location[i]="Micronesia (Fed. States of)"
  }
  else if(Case_data_2020_May_1$location[i]=="Syria")
  {
    Case_data_2020_May_1$location[i]="Syrian Arab Republic"
  }
  else if(Case_data_2020_May_1$location[i]=="Brunei")
  {
    Case_data_2020_May_1$location[i]="Brunei Darussalam"
  }
  else if(Case_data_2020_May_1$location[i]=="North Korea")
  {
    Case_data_2020_May_1$location[i]="Dem. People's Republic of Korea"
  }
}

Case_data_2020_May_2=merge(Case_data_2020_May_1,DistDataSimple,by.x="location",by.y="Region.subregion.country.area")
Case_data_2020_May_2$new_cases_smoothed_per_million=as.numeric(Case_data_2020_May_2$new_cases_smoothed_per_million)
Case_data_2020_May_2$reproduction_rate=as.numeric(Case_data_2020_May_2$reproduction_rate)
#making a figure
Cases_df_May = data.frame(distance = Case_data_2020_May_2$Metric.average.dist,
                           Casevalue = Case_data_2020_May_2$new_cases_smoothed_per_million)

Cases_df_May$distance=as.character(Cases_df_May$distance)

Cases1_May<-ggplot(data=Cases_df_May,aes(x=distance,y=Casevalue))+
  geom_boxplot()+
  geom_point(shape=1,color="black",size=2)+
  theme_classic()+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(
    text=element_text(size=12,family="Arial",color="black"))+
  xlab("Recommended distance (m)")+
  ylab("Cases per million (smoothed)")

Cases1_May

Reproduction_df_May = data.frame(distance = Case_data_2020_May_2$Metric.average.dist,
                             Reproduction = Case_data_2020_May_2$reproduction_rate)

Reproduction_df_May$distance = as.character(Reproduction_df_May$distance)

Reproduction1_May<-ggplot(data=Reproduction_df_May,aes(x=distance,y=Reproduction))+
  geom_boxplot()+
  geom_point(shape=1,color="black",size=2)+
  theme_classic()+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(
    text=element_text(size=12,family="Arial",color="black"))+
  xlab("Recommended distance (m)")+
  ylab("Reproduction value")
  

Reproduction1_May

#12-2 Taking data from Aug 2020
Case_data_2020_Aug=read.csv("owid-covid-data.csv",header=T,sep=",",stringsAsFactors = FALSE)
Case_data_2020_Aug_1<-Case_data_2020_Aug[1,]
for (i in 1:nrow(Case_data_2020_Aug)){
  if(Case_data_2020_Aug$date[i]=="2020-08-01")
  {
    Case_data_2020_Aug_1[i,]=Case_data_2020_Aug[i,]
  }
}
Case_data_2020_Aug_1=Case_data_2020_Aug_1[,c('location','new_cases_smoothed_per_million','reproduction_rate')]
Case_data_2020_Aug_1=na.omit(Case_data_2020_Aug_1)
for (i in 1:nrow(Case_data_2020_Aug_1)){
  if(Case_data_2020_Aug_1$location[i]=="Vietnam")
  {
    Case_data_2020_Aug_1$location[i]="Viet Nam"
  }
  else if(Case_data_2020_Aug_1$location[i]=="South Korea")
  {
    Case_data_2020_Aug_1$location[i]="Republic of Korea"
  }
  else if(Case_data_2020_Aug_1$location[i]=="United States")
  {
    Case_data_2020_Aug_1$location[i]="United States of America"
  }
  else if(Case_data_2020_Aug_1$location[i]=="Russia")
  {
    Case_data_2020_Aug_1$location[i]="Russian Federation"
  }
  else if(Case_data_2020_Aug_1$location[i]=="Cape Verde")
  {
    Case_data_2020_Aug_1$location[i]="Cabo Verde"
  }
  else if(Case_data_2020_Aug_1$location[i]=="Czechia")
  {
    Case_data_2020_Aug_1$location[i]="Czech Rep."
  }
  else if(Case_data_2020_Aug_1$location[i]=="Democratic Republic of Congo")
  {
    Case_data_2020_Aug_1$location[i]="Democratic Republic of the Congo"
  }
  else if(Case_data_2020_Aug_1$location[i]=="Dominican Republic")
  {
    Case_data_2020_Aug_1$location[i]="Dominican Rep."
  }
  else if(Case_data_2020_Aug_1$location[i]=="Micronesia (country)")
  {
    Case_data_2020_Aug_1$location[i]="Micronesia (Fed. States of)"
  }
  else if(Case_data_2020_Aug_1$location[i]=="Syria")
  {
    Case_data_2020_Aug_1$location[i]="Syrian Arab Republic"
  }
  else if(Case_data_2020_Aug_1$location[i]=="Brunei")
  {
    Case_data_2020_Aug_1$location[i]="Brunei Darussalam"
  }
  else if(Case_data_2020_Aug_1$location[i]=="North Korea")
  {
    Case_data_2020_Aug_1$location[i]="Dem. People's Republic of Korea"
  }
}

Case_data_2020_Aug_2=merge(Case_data_2020_Aug_1,DistDataSimple,by.x="location",by.y="Region.subregion.country.area")
Case_data_2020_Aug_2$new_cases_smoothed_per_million=as.numeric(Case_data_2020_Aug_2$new_cases_smoothed_per_million)
Case_data_2020_Aug_2$reproduction_rate=as.numeric(Case_data_2020_Aug_2$reproduction_rate)
#making a figure
Cases_df_Aug = data.frame(distance = Case_data_2020_Aug_2$Metric.average.dist,
                          Casevalue = Case_data_2020_Aug_2$new_cases_smoothed_per_million)

Cases_df_Aug$distance=as.character(Cases_df_Aug$distance)

Cases1_Aug<-ggplot(data=Cases_df_Aug,aes(x=distance,y=Casevalue))+
  geom_boxplot()+
  geom_point(shape=1,color="black",size=2)+
  theme_classic()+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(
    text=element_text(size=12,family="Arial",color="black"))+
  xlab("Recommended distance (m)")+
  ylab("Cases per million (smoothed)")

Cases1_Aug

Reproduction_df_Aug = data.frame(distance = Case_data_2020_Aug_2$Metric.average.dist,
                                 Reproduction = Case_data_2020_Aug_2$reproduction_rate)

Reproduction_df_Aug$distance = as.character(Reproduction_df_Aug$distance)

Reproduction1_Aug<-ggplot(data=Reproduction_df_Aug,aes(x=distance,y=Reproduction))+
  geom_boxplot()+
  geom_point(shape=1,color="black",size=2)+
  theme_classic()+
  theme(
    axis.text=element_text(size=12,family="Arial",color="black"))+
  theme(
    text=element_text(size=12,family="Arial",color="black"))+
  xlab("Recommended distance (m)")+
  ylab("Reproduction value")


Reproduction1_Aug


#13.Performing one-way anova tests for each factors 
##############

#13-3 Interpersonal distance
#bartlett test for the one-way ANOVA test
bartlett.test(Personal.Distance.mean~Metric.average.dist,data=PersDistRecDist)
bartlett.test(Social.Distance.mean~Metric.average.dist,data=PersDistRecDist)
bartlett.test(Intimate.distance.mean~Metric.average.dist,data=PersDistRecDist)
#p values are 0.1634, 0.06686, 0.809 each, so all can be tested for ANOVA

#personal
ANOVA_Personal <-aov(Metric.average.dist~Personal.Distance.mean,data=PersDistRecDist)
summary(ANOVA_Personal)
#p value of 0.432

#social
ANOVA_Social <-aov(Metric.average.dist~Social.Distance.mean,data=PersDistRecDist)
summary(ANOVA_Social)
#p value of 0.885

#intimate
ANOVA_Intimate <-aov(Metric.average.dist~Intimate.distance.mean,data=PersDistRecDist)
summary(ANOVA_Intimate)
#p value of 0.0831

#13-4 Population density
#ANOVA test
ANOVA_Population <-aov(Metric.average.dist~Population.density.2020,data=DensityData)
summary(ANOVA_Population)
#p-value of 0.698,which is not significant

#13-5 Colonized countries / colonizers

#we used data of countries which colonized more than 5 countries for ANOVA test - total of 133 countries
DistDataColonies$colonizer1=as.factor(DistDataColonies$colonizer1)
DistDataColonies2=DistDataColonies
DistDataColonies2=DistDataColonies2[is.element(DistDataColonies2$colonizer1, c('FRA','ESP','RUS','GBR','PRT','TUR')),]

data_colonies=data.frame(c=DistDataColonies2$colonizer1,dist=DistDataColonies2$Metric.average.dist)
data_colonies$c=as.character(data_colonies$c)

#bartlett's test for the one-way ANOVA test
bartlett.test(Metric.average.dist~colonizer1,data=DistDataColonies2)

#p-value of 0.9943 - passed the bartlett test
ANOVA_Colonies <- aov(Metric.average.dist~colonizer1,data=DistDataColonies2)
summary(ANOVA_Colonies)

#p value of 0.124, so there is no significance as a whole

#multiple-comparision test
TukeyHSD(ANOVA_Colonies,conf.level=.95)
#no significance between all comparisions


#plot(TukeyHSD(ANOVA_Colonies,conf.level=.95))

#13-6 legal system
#we remove the muslim & customary countries, which lack sufficient data 
legal_table_2=LegalDistRecDist
legal_table_2$legal.systems=as.factor(legal_table_2$legal.systems)
legal_table_2=legal_table_2[legal_table_2$legal.systems!=" muslim",]
legal_table_2=legal_table_2[legal_table_2$legal.systems!=" customary",]

#performing bartlett's test to check suitability for ANOVA
bartlett.test(Metric.average.dist~legal.systems,data=legal_table_2)

#drawing Q-Q plot for normality assumption
#plot(ANOVA_Legal, 2,xlab="Theoretical quantiles",sub="",submain="Normal Q-Q of legal ANOVA")

#p value of 0.3572, we can reject the null hypothesis, and thus we can perform the ANOVA test
ANOVA_Legal<- aov(Metric.average.dist~legal.systems, data = legal_table_2)
summary(ANOVA_Legal)
#p value of 9.24e-05, thus it is significant
#performing Tukey's test for multiple comparisions
TukeyHSD(ANOVA_Legal,conf.level=.95)
TukeyLegal<-TukeyHSD(ANOVA_Legal,conf.level=.95)

#Mixed-common law and common law - civilist comparision produce significant results


#13-7 language
#we include countries which use French, English, Spanish, Arabic as their first official language, for data clarity
language_table<-subset(DistDataGeoDist2, langoff_1=="French" | langoff_1=="English" | langoff_1=="Arabic" | langoff_1=="Spanish")
#total of 101 countries

bartlett.test(Metric.average.dist~langoff_1,data=language_table)
#p-value of 0.24, we can reject the null hypothesis, and thus we can perform the ANOVA test
ANOVA_Language <-aov(Metric.average.dist~langoff_1,data=language_table)
summary(ANOVA_Language)
#p value of 0.00311, thus it is significant
#performing Tukey's test for multiple comparisions

#plot(TukeyHSD(ANOVA_Language,conf.level=.95))
TukeyHSD(ANOVA_Language,conf.level=.95)
#only French - English comparison produce significant results 

#13-8 continent
bartlett.test(Metric.average.dist~continent,data=DistDataGeoDist2)
#p value of 0.6527, we can reject the null hypothesis and can perform the ANOVA
ANOVA_Continent <-aov(Metric.average.dist~continent,data=DistDataGeoDist2)    
summary(ANOVA_Continent)
#p value of 6.08e-05, thus it is significant
TukeyHSD(ANOVA_Continent,conf.level=.95)
#plot(TukeyHSD(ANOVA_Continent,conf.level=.95))
#America-Africa, Europe-Africa 

#13-9 currency
bartlett.test(Metric.average.dist~Currency,data=Currency_table)
#bartlett's test shows p value of 0.0176, thus the dataset is insufficient to perform the ANOVA test
ANOVA_Currency<-aov(Metric.average.dist~Currency,data=Currency_table)
summary(ANOVA_Currency)
#p-value of 8.7e-06, so there is statistical significance to sya the mean is different between the observations
TukeyHSD(ANOVA_Currency,conf.level=.95)

#13-10 previous exposure to SARS
SARS_data_2$Exposure=as.factor(SARS_data_2$Exposure)
bartlett.test(Metric.average.dist~Exposure,data=SARS_data_2)
#p value of 0.8772, so we can perform the ANOVA test
ANOVA_SARS<-aov(Metric.average.dist~Exposure,data=SARS_data_2)
summary(ANOVA_SARS)
#p value of 0.775, so there is no statistical significance between the two measurements
#plot(TukeyHSD(ANOVA_SARS,conf.level=.95))
TukeyHSD(ANOVA_SARS,conf.level=.95)

#13-11 transmission data (with 39 countries)
growthrate_ANOVA_table=GrowthRate_UC_data_2
growthrate_ANOVA_table$Metric.average.dist=as.factor(growthrate_ANOVA_table$Metric.average.dist)
bartlett.test(Rpost_2_MCMC~Metric.average.dist,data=growthrate_ANOVA_table)
#p-value of 0.9748, so we can perform the ANOVA test
ANOVA_Growthrate <- aov(Rpost_2_MCMC~Metric.average.dist,data=growthrate_ANOVA_table)
summary(ANOVA_Growthrate)
#p-value of 0.579, so there is no statistical significance between R values and the recommended distance

#13-12 transmission data (with 142 countries)
#May 2020
bartlett.test(R~Metric.average.dist, data=R_tracking_data_May_2020_4)
#p-value of 0.2979, so we can perform the ANOVA test
ANOVA_Transmission_May <- aov(R~Metric.average.dist,data=R_tracking_data4_May_2020_4)
summary(ANOVA_Transmission_May)
#p-value of 0.156, so there is no statistical significance between R values and the recommended distances 

#Aug 2020
bartlett.test(R~Metric.average.dist,data=R_tracking_data_Aug_2020_4)
#p-value of , so we can perform the ANOVA test
ANOVA_Transmission <-aov(R~Metric.average.dist,data=R_tracking_data_Aug_2020_4)
summary(ANOVA_Transmission)
#p-value of , so there is no statistical significance between R values and the recommended distances

#14. Making a table including all the factors for the binomial generalized mixed model
############


#14-3 interpersonal distances
distancetable<-PersDistRecDist
distancetable2<-DistDataWithoutND
#42 countries 

#14-4 Population density
#already included in the distancetable

#14-5 Colonization 
distancetable=merge(distancetable,DistDataColonies3[c("cnum","colonyfactor","colonyfactortemp")], by.x="Country.code",by.y="cnum")
distancetable2=merge(distancetable2,DistDataColonies3[c("cnum","colonyfactor","colonyfactortemp")],by.x="Country.code",by.y="cnum")
#table1- one country (Serbia) has been removed - country code is different by each data
#table2- 6 countries removed

#14-6 legal system
distancetable=merge(distancetable,legal_table[c("c","legalfactor","legalfactortemp")],by.x="c",by.y="c")
distancetable2=merge(distancetable2,legal_table[c("c","legalfactor","legalfactortemp")],by.x="c",by.y="c")
#table1 - one country (Saudi Arabia) has been removed - Muslim country
#table2 - three countries (2 muslim, 1 customary) removed

#14-7 language
distancetable=merge(distancetable,language_table_glm[c("c","languagefactor","languagefactortemp")],by.x="c",by.y="c")
distancetable2=merge(distancetable2,language_table_glm[c("c","languagefactor","languagefactortemp")],by.x="c",by.y="c")

#14-8 continent
distancetable=merge(distancetable,continent_table[c("c","continentfactor")],by.x="c",by.y="c")
distancetable2=merge(distancetable2,continent_table[c("c","continentfactor")],by.x="c",by.y="c")

#14-9 currency 
distancetable=merge(distancetable,currency_table_glm[c("c","currencyfactor","currencyfactortemp")],by.x="c",by.y="c")
distancetable2=merge(distancetable2,currency_table_glm[c("c","currencyfactor","currencyfactortemp")],by.x="c",by.y="c")

#14-10 Previous exposure to SARS
distancetable=merge(distancetable,SARS_data_2[c("c","Exposure")],by.x="c",by.y="c")
distancetable2=merge(distancetable2,SARS_data_2[c("c","Exposure")],by.x="c",by.y="c")


#15. Binomial GLM for numerical variables, LM to collect P values 
##############
#15-1 Interpersonal distances

#social distance
Socialdistance_GLM=glm(formula=Binomial.dist~Social.Distance.mean,family=binomial,data=PersDistRecDist)
summary(Socialdistance_GLM)
#p-value of 0.71, so there is no statistical significance

#graph for social distance
range(PersDistRecDist$Social.Distance.mean)
xSocial<-seq(75,140,0.25)
xSocialGLM<--1.62079-0.03813*xSocial
ySocial<-predict(Socialdistance_GLM,list(Social.Distance.mean=xSocial),type="response")
plot(PersDistRecDist$Social.Distance.mean,PersDistRecDist$Binomial.dist,pch=16,xlab="Social distance (cm)",ylab="Recommended distance (m)")
lines(xSocial,ySocial)

#personal distance
Personaldistance_GLM=glm(formula=Binomial.dist~Personal.Distance.mean,family=binomial,data=PersDistRecDist)
summary(Personaldistance_GLM)
#p-value of 0.724, so there is no statistical significance

#graph for personal distance
range(PersDistRecDist$Personal.Distance.mean)
xPersonal<-seq(58,110,0.25)
yPersonal<-predict(Personaldistance_GLM,list(Personal.Distance.mean=xPersonal),type="response")
plot(PersDistRecDist$Personal.Distance.mean,PersDistRecDist$Binomial.dist,pch=16,xlab="Personal Distance(cm)",ylab="Recommended distance")
lines(xPersonal,yPersonal)

#intimate distance
Intimatedistance_GLM=glm(formula=Binomial.dist~Intimate.distance.mean,family=binomial,data=PersDistRecDist)
summary(Intimatedistance_GLM)
#p-value of 0,170, so there is no statistical significance

#graph for intimate distance
range(PersDistRecDist$Intimate.distance.mean)
xIntimate<-seq(35,100,0.25)
yIntimate<-predict(Intimatedistance_GLM,list(Intimate.distance.mean=xIntimate),type="response")
plot(PersDistRecDist$Intimate.distance.mean,PersDistRecDist$Binomial.dist,pch=16,xlab="Intimate distance(cm)",ylab="Recommended distance")
lines(xIntimate,yIntimate)

#15-4 Population density
Populationdensity_GLM=glm(formula=Binomial.dist~Population.density.2020,family=binomial,data=DensityData)
summary(Populationdensity_GLM)
range(DensityData$Population.density.2020)
xDensity <- seq(2,2300,10)
yDensity <- predict(Populationdensity_GLM,list(Population.density.2020=xDensity),type="response")
par(mar=c(5,5,4,2))
plot(DensityData$Population.density.2020,DensityData$Binomial.dist,pch=16,xlab=expression(paste("Population density (inhab/",km^2,")")),ylab="Probability to be at the highest\n recommended distance")
lines(xDensity,yDensity)

#15-5 Transmission rate (dataset #1, May 2020)
R_tracking_data_May_2020_4$Binomial.dist=as.numeric(R_tracking_data_May_2020_4$Binomial.dist)
Transmission_GLM_2_May=glm(formula=Binomial.dist~R,family=binomial,data=R_tracking_data_May_2020_4)
summary(Transmission_GLM_2_May)
Anova(Transmission_GLM_2_May)
xR <- seq(0,2.1,0.01)
yR <- predict(Transmission_GLM_2_May, list(R=xR), type="response")
plot(R_tracking_data_May_2020_4$R,R_tracking_data_May_2020_4$Binomial.dist,pch=16, xlab= "Transmission rate", ylab = "Probability to be at the\nhighest recommended distance")
lines(xR, yR)

#15-6 Transmission rate (dataset #1, Aug 2020)
R_tracking_data_Aug_2020_4$Binomial.dist=as.numeric(R_tracking_data_Aug_2020_4$Binomial.dist)
Transmission_GLM_2_Aug=glm(formula=Binomial.dist~R,family=binomial,data=R_tracking_data_Aug_2020_4)
summary(Transmission_GLM_2_Aug)
Anova(Transmission_GLM_2_Aug)
xR <-seq(0,2.1,0.01)
yR <- predict(Transmission_GLM_2_Aug, list(R = xR),type="response")
plot(R_tracking_data_Aug_2020_4$R, R_tracking_data_Aug_2020_4$Binomial.dist, pch = 16, xlab = "Transmission rate", ylab = "Probability to be at the\nhighest recommended distance")
lines(xR, yR)

#15-7 Transmission rate (dataset #2, May 2020)
Transmission_GLM_3_May=glm(formula=Binomial.dist~reproduction_rate,family=binomial,data=Case_data_2020_May_2)
summary(Transmission_GLM_3_May)
Anova(Transmission_GLM_3_May)
xR <- seq(0,2.0,0.01)
yR <- predict(Transmission_GLM_3_May,list(reproduction_rate=xR),type="response")
plot(Case_data_2020_May_2$reproduction_rate,Case_data_2020_May_2$Binomial.dist,pch=16,xlab="Reproduction rate", ylab="Probability to be at the\nhighest recommended distance")
lines(xR,yR)

#15-8 Transmission rate (dataset #2, Aug 2020)
Transmission_GLM_3_Aug=glm(formula=Binomial.dist~reproduction_rate,family=binomial,data=Case_data_2020_Aug_2)
summary(Transmission_GLM_3_Aug)
Anova(Transmission_GLM_3_Aug)
xR <- seq(0,2.0,0.01)
yR <- predict(Transmission_GLM_3_Aug,list(reproduction_rate=xR),type="response")
plot(Case_data_2020_Aug_2$reproduction_rate,Case_data_2020_Aug_2$Binomial.dist,pch=16,xlab="Reproduction rate",ylab="Probability to be at the\nhighest recommended distance")
lines(xR,yR)

#Creating a linear model (for collecting the P value)
#Transmission_LM_2<-lm(formula=Binomial.dist~R,data=R_tracking_data_Aug_2020_4)
#summary(Transmission_LM_2)
#Anova(Transmission_LM_2)

#15-9 Smoothed cases corrected for population (dataset #3, May 2020)
Cases_GLM_May<-glm(formula=Binomial.dist~new_cases_smoothed_per_million,family=binomial,data=Case_data_2020_May_2)
summary(Cases_GLM_May)
Anova(Cases_GLM_May)
xCases_May <-seq(0,300,3)
yCases_May <- predict(Cases_GLM_May, list(new_cases_smoothed_per_million = xCases_May),type="response")
plot(Case_data_2020_May_2$new_cases_smoothed_per_million, Case_data_2020_May_2$Binomial.dist, pch = 16, xlab = "Cases per million(smoothed)", ylab = "Probability to be at the\nhighest recommended distance")
lines(xCases_May, yCases_May)

#15-10 Smoothed cases corrected for population (dataset #3, Aug 2020)
Cases_GLM_Aug<-glm(formula=Binomial.dist~new_cases_smoothed_per_million,family=binomial,data=Case_data_2020_Aug_2)
summary(Cases_GLM_Aug)
Anova(Cases_GLM_Aug)
xCases_Aug <- seq(0,260,3)
yCases_Aug <- predict(Cases_GLM_Aug,list(new_cases_smoothed_per_million=xCases_Aug),type="response")
plot(Case_data_2020_Aug_2$new_cases_smoothed_per_million,Case_data_2020_Aug_2$Binomial.dist,pch=16,xlab="Cases per million(smoothed)", ylab= "Probability to be at the\nhighest recommended distance")
lines(xCases_Aug,yCases_Aug)

Cases_LM_Aug<-lm(formula=new_cases_smoothed_per_million~Binomial.dist,data=Case_data_2020_Aug_2)
summary(Cases_GLM_Aug)

#16 Performing Binomial GLMM 

#16-1 Model 1: GLM with 42 countries, including 3 types of interpersonal distance, language

distanceGLM<-glm(formula=Binomial.dist~Population.density.2020+Intimate.distance.mean+Social.Distance.mean+Personal.Distance.mean+Exposure+legalfactortemp+colonyfactortemp+languagefactortemp+currencyfactortemp,family=binomial,data=distancetable)
summary(distanceGLM)
vif(distanceGLM)
Anova(distanceGLM)

#16-2 Model 2: GLM with 42 countries, including interpersonal distance

distanceGLM2<-glm(formula=Binomial.dist~Population.density.2020+Intimate.distance.mean+Exposure+legalfactortemp+colonyfactortemp+currencyfactortemp,family=binomial,data=distancetable)
summary(distanceGLM2)
vif(distanceGLM2)
Anova(distanceGLM2)
#Intimate distance, Exposure to SARS-CoV-1, and Currency union show significance (P<0.05), with currency showing mild significance (0.053)

#16-3 Model 3: GLM with all(175) countries, not including interpersonal distance

distanceGLM3<-glm(formula=Binomial.dist~Population.density.2020+Exposure+legalfactortemp+colonyfactortemp+currencyfactortemp,family=binomial,data=distancetable2)
summary(distanceGLM3)
vif(distanceGLM3)
Anova(distanceGLM3)

#Legal factor and Currency union shows significance (P<0.05)
#p<-c(0.310,0.052)
#fisher(p,m=2)

#Supplementary-1 correlation between currency and language

cor_lan_temp <- distancetable2[,c('languagefactortemp','currencyfactortemp')]
cor_lan_cur_df<-table(cor_lan_temp$languagefactortemp,cor_lan_temp$currencyfactortemp)
CramerV(cor_lan_cur_df)



