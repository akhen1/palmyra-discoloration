#set working directory
setwd("C:/Users/adikh/Desktop")

#load data
data=read.csv(file="Planar area and discoloration_all corals.csv", header=TRUE, sep=",")

#convert to date format
data$Date[data$Date == 909] <- '0909'
library(zoo)
ym <- as.yearmon(as.character(data$Date), "%y%m")
total_data=cbind(data,ym)
total_data$taxa=sapply(strsplit(as.character(total_data$Colony.ID),'_'),function(x) paste(x[is.na(as.numeric(x))],collapse='_'))

#subset by habitat
total_data[,"Habitat"] <- NA
total_data$Habitat[total_data$Site %in% c("FR3", "FR5", "FR7", "FR9")] <- "FR"
total_data$Habitat[total_data$Site %in% c("RT1", "RT4", "RT10", "RT13")] <- "RT"

#calculate percent cover (within 0.54 m^2 quadrat)
total_data[,"Percent.cover"] <- (total_data$Total.area/5400)*100

library(dplyr)
#calculate summary data: mean percent cover by species for each quadrat over time
corals_all=tapply(total_data$Percent.cover, list(total_data$Habitat, total_data$Site, total_data$Quad, total_data$ym, total_data$taxa), sum)
library(reshape2)
corals_all=melt(corals_all)
colnames(corals_all)= c("Habitat", "Site", "Quad", "Date", "Species", "Percent.cover")
corals_all= na.omit(corals_all)

#subset by time point
corals_all.2009=subset(corals_all, subset= Date %in% "Sep 2009")
corals_all.2019=subset(corals_all, subset= Date %in% "Sep 2019")
corals_combined=rbind(corals_all.2009, corals_all.2019)
corals_combined2=dcast(corals_combined, Date + Habitat + Site + Quad ~ Species, value.var = "Percent.cover")
corals_combined2[is.na(corals_combined2)] <- 0

library(vegan)
NMDS.corals_combined=metaMDS(corals_combined2[,c(5:38)], distance = "bray", k=2)

library(ggord)
cols=c("red","orange")

#can only color-code by one variable at a time

library(ggplot2)
#color-coded by time point
NMDS.corals_combined.plot=ggord(NMDS.corals_combined, corals_combined2$Date, size=3, ellipse=F, txt=3, lab.cex=5, repel=F, vectyp=0) +
  theme_classic(base_size=12) +
  ggtitle("2009 and 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.spacing.y = unit(0, "mm"), legend.title = element_blank(), legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) +
  annotate(geom="label", x=-1.8, y=-1.8, label="Stress=0.15", size=4.5)+
  scale_shape_manual('Date',values = c(16, 17))+
  scale_color_manual('Habitat', values=cols)+
  xlab("MDS1")+
  ylab("MDS2") 

#color-coded by habitat
NMDS.corals_combined.plot2=ggord(NMDS.corals_combined, corals_combined2$Habitat, size=3, ellipse=F, txt=3, lab.cex=5, repel=F, vectyp=0) +
  theme_classic(base_size=12) +
  ggtitle("2009 and 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.spacing.y = unit(0, "mm"), legend.title = element_blank(), legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) +
  annotate(geom="label", x=-1.8, y=-1.8, label="Stress=0.15", size=4.5)+
  scale_shape_manual('Date',values = c(16, 17))+
  scale_color_manual('Habitat', values=cols)+
  xlab("MDS1")+
  ylab("MDS2") 

#merge plots and update legend in Illustrator, also remove text labels for non-focal species

#PERMANOVA over time
corals_all_2=dcast(corals_all, Habitat + Site + Quad + Date ~ Species, value.var = "Percent.cover")
corals_all_2[is.na(corals_all_2)] <- 0

NMDS.dist.all<- vegdist(corals_all_2[,c(5:38)], method = "bray")

perm.all=adonis(NMDS.dist.all ~ Habitat/Site*as.factor(Date), data = corals_all_2, permutations = 9999)
print(perm.all)

#SIMPER analysis to identify species that best describe differences among habitats
coral.spp=corals_all_2[,c(5:38)]
coral.spp.env=corals_all_2[,c(1:4)]

(sim <- with(coral.spp.env, simper(coral.spp, Habitat)))
summary(sim)