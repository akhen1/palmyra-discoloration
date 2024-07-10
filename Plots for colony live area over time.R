#set working directory
setwd("C:/Users/adikh/Desktop")

#load data
data=read.csv(file="Planar area and discoloration by colony_focal species.csv", header=TRUE, sep=",")

#convert to date format
data$Date[data$Date == 909] <- '0909'
library(zoo)
ym <- as.yearmon(as.character(data$Date), "%y%m")
total_data=cbind(data,ym)

#remove rows for dead colonies
total_data2=total_data[!total_data$Total.area == 0, ]

#remove inconsistent time points (Oct 2010, May 2011, July 2016)
total_data2.2=total_data2[!total_data2$Date == "1010", ]
total_data2.3=total_data2.2[!total_data2.2$Date == "1105", ]
total_data2.4=total_data2.3[!total_data2.3$Date == "1607", ]

#subset by taxon
total_data3 <- split(total_data2.4,f = total_data2.4$Genus_species)
names(total_data3)

#function for standard error
std.error <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

#function for sample sizes
give.n <- function(x) {                                                                                                                                                                                                                                                                                                
  return(c(y = mean(x), label = length(x)))                                                                                                                                                                                                                                                                         
}  

library(dplyr)

#calculate summary data, by species
Astrea_curta2 <- total_data3$Astrea_curta %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Pigmented.area),
            SE= std.error(Pigmented.area),
            type="Pigmented.area")
as.data.frame(Astrea_curta2)
Astrea_curta3 <- total_data3$Astrea_curta %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Discolored.area),
            SE= std.error(Discolored.area),
            type="Discolored.area")
as.data.frame(Astrea_curta3)

Astrea_curta4=as.data.frame(rbind(Astrea_curta2, Astrea_curta3))

Astrea_curta4$type <- factor(Astrea_curta4$type, levels = c("Pigmented.area", "Discolored.area")) 
Astrea_curta4$y_pos = NA
Astrea_curta4$y_pos[Astrea_curta4$type == "Discolored.area"] = Astrea_curta4$Mean[Astrea_curta4$type == "Discolored.area"]
Astrea_curta4$y_pos[Astrea_curta4$type == "Pigmented.area"] = Astrea_curta4$Mean[Astrea_curta4$type == "Discolored.area"] + 
  Astrea_curta4$Mean[Astrea_curta4$type == "Pigmented.area"]

library(ggplot2)
Astrea_curta_bar=ggplot(Astrea_curta4, aes(x = as.factor(ym), y = Mean, fill=type)) +
  theme_classic(base_size=11) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position=c(0.8,0.8), legend.text = element_text(size=7), legend.key.size = unit(0.3, 'cm'), legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.x = element_text(angle=90, hjust = 0.6, vjust= 0.5)) +
  geom_bar(stat ="identity", position="stack", color="black")+
  geom_errorbar(aes(ymin=y_pos , ymax=y_pos+SE), position = "identity", width=0)+
  xlab("Date") + 
  ylab("Live planar area") +    
  ggtitle(~italic("Astrea curta"))+
  scale_fill_manual(values = c("burlywood4", "cornsilk"))+
  annotate(geom="text", x="Apr 2010", y=45, label="n=19", size=3.5)

###
Astreopora_myriophthalma=as.data.frame(Astreopora_myriophthalma)
Astreopora_myriophthalma2 <- total_data3$Astreopora_myriophthalma %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Pigmented.area),
            SE= std.error(Pigmented.area),
            type="Pigmented.area")
as.data.frame(Astreopora_myriophthalma2)
Astreopora_myriophthalma3 <- total_data3$Astreopora_myriophthalma %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Discolored.area),
            SE= std.error(Discolored.area),
            type="Discolored.area")
as.data.frame(Astreopora_myriophthalma3)

Astreopora_myriophthalma4=as.data.frame(rbind(Astreopora_myriophthalma2, Astreopora_myriophthalma3))

Astreopora_myriophthalma4$type <- factor(Astreopora_myriophthalma4$type, levels = c("Pigmented.area", "Discolored.area")) 
Astreopora_myriophthalma4$y_pos = NA
Astreopora_myriophthalma4$y_pos[Astreopora_myriophthalma4$type == "Discolored.area"] = Astreopora_myriophthalma4$Mean[Astreopora_myriophthalma4$type == "Discolored.area"]
Astreopora_myriophthalma4$y_pos[Astreopora_myriophthalma4$type == "Pigmented.area"] = Astreopora_myriophthalma4$Mean[Astreopora_myriophthalma4$type == "Discolored.area"] + 
  Astreopora_myriophthalma4$Mean[Astreopora_myriophthalma4$type == "Pigmented.area"]

Astreopora_myriophthalma_bar=ggplot(Astreopora_myriophthalma4, aes(x = as.factor(ym), y = Mean, fill=type)) +
  theme_classic(base_size=11) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none", legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.x = element_text(angle=90, hjust = 0.6, vjust= 0.5)) +
  geom_bar(stat ="identity", position="stack", color="black")+
  geom_errorbar(aes(ymin=y_pos , ymax=y_pos+SE), position = "identity", width=0)+
  xlab("Date") + 
  ylab("Live planar area") +    
  ggtitle(~italic("Astreopora myriophthalma"))+
  scale_fill_manual(values = c("burlywood4", "cornsilk"))+
  annotate(geom="text", x="Apr 2010", y=800, label="n=7", size=3.5)

###
Goniastrea_stelligera=as.data.frame(Goniastrea_stelligera)
Goniastrea_stelligera2 <- total_data3$Goniastrea_stelligera %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Pigmented.area),
            SE= std.error(Pigmented.area),
            type="Pigmented.area")
as.data.frame(Goniastrea_stelligera2)
Goniastrea_stelligera3 <- total_data3$Goniastrea_stelligera %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Discolored.area),
            SE= std.error(Discolored.area),
            type="Discolored.area")
as.data.frame(Goniastrea_stelligera3)

Goniastrea_stelligera4=as.data.frame(rbind(Goniastrea_stelligera2, Goniastrea_stelligera3))

Goniastrea_stelligera4$type <- factor(Goniastrea_stelligera4$type, levels = c("Pigmented.area", "Discolored.area")) 
Goniastrea_stelligera4$y_pos = NA
Goniastrea_stelligera4$y_pos[Goniastrea_stelligera4$type == "Discolored.area"] = Goniastrea_stelligera4$Mean[Goniastrea_stelligera4$type == "Discolored.area"]
Goniastrea_stelligera4$y_pos[Goniastrea_stelligera4$type == "Pigmented.area"] = Goniastrea_stelligera4$Mean[Goniastrea_stelligera4$type == "Discolored.area"] + 
  Goniastrea_stelligera4$Mean[Goniastrea_stelligera4$type == "Pigmented.area"]

Goniastrea_stelligera_bar=ggplot(Goniastrea_stelligera4, aes(x = as.factor(ym), y = Mean, fill=type)) +
  theme_classic(base_size=11) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none", legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.x = element_text(angle=90, hjust = 0.6, vjust= 0.5)) +
  geom_bar(stat ="identity", position="stack", color="black")+
  geom_errorbar(aes(ymin=y_pos , ymax=y_pos+SE), position = "identity", width=0)+
  xlab("Date") + 
  ylab("Live planar area") +    
  ggtitle(~italic("Goniastrea stelligera"))+
  scale_fill_manual(values = c("burlywood4", "cornsilk"))+
  annotate(geom="text", x="Apr 2010", y=275, label="n=49", size=3.5)

###
Hydnophora_microconos=as.data.frame(Hydnophora_microconos)
Hydnophora_microconos2 <- total_data3$Hydnophora_microconos %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Pigmented.area),
            SE= std.error(Pigmented.area),
            type="Pigmented.area")
as.data.frame(Hydnophora_microconos2)
Hydnophora_microconos3 <- total_data3$Hydnophora_microconos %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Discolored.area),
            SE= std.error(Discolored.area),
            type="Discolored.area")
as.data.frame(Hydnophora_microconos3)

Hydnophora_microconos4=as.data.frame(rbind(Hydnophora_microconos2, Hydnophora_microconos3))

Hydnophora_microconos4$type <- factor(Hydnophora_microconos4$type, levels = c("Pigmented.area", "Discolored.area")) 
Hydnophora_microconos4$y_pos = NA
Hydnophora_microconos4$y_pos[Hydnophora_microconos4$type == "Discolored.area"] = Hydnophora_microconos4$Mean[Hydnophora_microconos4$type == "Discolored.area"]
Hydnophora_microconos4$y_pos[Hydnophora_microconos4$type == "Pigmented.area"] = Hydnophora_microconos4$Mean[Hydnophora_microconos4$type == "Discolored.area"] + 
  Hydnophora_microconos4$Mean[Hydnophora_microconos4$type == "Pigmented.area"]

Hydnophora_microconos_bar=ggplot(Hydnophora_microconos4, aes(x = as.factor(ym), y = Mean, fill=type)) +
  theme_classic(base_size=11) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none", legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.x = element_text(angle=90, hjust = 0.6, vjust= 0.5)) +
  geom_bar(stat ="identity", position="stack", color="black")+
  geom_errorbar(aes(ymin=y_pos , ymax=y_pos+SE), position = "identity", width=0)+
  xlab("Date") + 
  ylab("Live planar area") +    
  ggtitle(~italic("Hydnophora microconos"))+
  scale_fill_manual(values = c("burlywood4", "cornsilk"))+
  annotate(geom="text", x="Apr 2010", y=400, label="n=9", size=3.5)

###
Pavona_chiriquiensis2 <- total_data3$Pavona_chiriquiensis %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Pigmented.area),
            SE= std.error(Pigmented.area),
            type="Pigmented.area")
as.data.frame(Pavona_chiriquiensis2)
Pavona_chiriquiensis3 <- total_data3$Pavona_chiriquiensis %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Discolored.area),
            SE= std.error(Discolored.area),
            type="Discolored.area")
as.data.frame(Pavona_chiriquiensis3)

Pavona_chiriquiensis4=as.data.frame(rbind(Pavona_chiriquiensis2, Pavona_chiriquiensis3))

Pavona_chiriquiensis4$type <- factor(Pavona_chiriquiensis4$type, levels = c("Pigmented.area", "Discolored.area")) 
Pavona_chiriquiensis4$y_pos = NA
Pavona_chiriquiensis4$y_pos[Pavona_chiriquiensis4$type == "Discolored.area"] = Pavona_chiriquiensis4$Mean[Pavona_chiriquiensis4$type == "Discolored.area"]
Pavona_chiriquiensis4$y_pos[Pavona_chiriquiensis4$type == "Pigmented.area"] = Pavona_chiriquiensis4$Mean[Pavona_chiriquiensis4$type == "Discolored.area"] + 
  Pavona_chiriquiensis4$Mean[Pavona_chiriquiensis4$type == "Pigmented.area"]

Pavona_chiriquiensis_bar=ggplot(Pavona_chiriquiensis4, aes(x = as.factor(ym), y = Mean, fill=type)) +
  theme_classic(base_size=11) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none", legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.x = element_text(angle=90, hjust = 0.6, vjust= 0.5)) +
  geom_bar(stat ="identity", position="stack", color="black")+
  geom_errorbar(aes(ymin=y_pos , ymax=y_pos+SE), position = "identity", width=0)+
  xlab("Date") + 
  ylab("Live planar area") +    
  ggtitle(~italic("Pavona chiriquiensis"))+
  scale_fill_manual(values = c("burlywood4", "cornsilk"))+
  annotate(geom="text", x="Apr 2010", y=95, label="n=37", size=3.5)

###
Pavona_duerdeni2 <- total_data3$Pavona_duerdeni %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Pigmented.area),
            SE= std.error(Pigmented.area),
            type="Pigmented.area")
as.data.frame(Pavona_duerdeni2)
Pavona_duerdeni3 <- total_data3$Pavona_duerdeni %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Discolored.area),
            SE= std.error(Discolored.area),
            type="Discolored.area")
as.data.frame(Pavona_duerdeni3)

Pavona_duerdeni4=as.data.frame(rbind(Pavona_duerdeni2, Pavona_duerdeni3))

Pavona_duerdeni4$type <- factor(Pavona_duerdeni4$type, levels = c("Pigmented.area", "Discolored.area")) 
Pavona_duerdeni4$y_pos = NA
Pavona_duerdeni4$y_pos[Pavona_duerdeni4$type == "Discolored.area"] = Pavona_duerdeni4$Mean[Pavona_duerdeni4$type == "Discolored.area"]
Pavona_duerdeni4$y_pos[Pavona_duerdeni4$type == "Pigmented.area"] = Pavona_duerdeni4$Mean[Pavona_duerdeni4$type == "Discolored.area"] + 
  Pavona_duerdeni4$Mean[Pavona_duerdeni4$type == "Pigmented.area"]

Pavona_duerdeni_bar=ggplot(Pavona_duerdeni4, aes(x = as.factor(ym), y = Mean, fill=type)) +
  theme_classic(base_size=11) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none", legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.x = element_text(angle=90, hjust = 0.6, vjust= 0.5)) +
  geom_bar(stat ="identity", position="stack", color="black")+
  geom_errorbar(aes(ymin=y_pos , ymax=y_pos+SE), position = "identity", width=0)+
  xlab("Date") + 
  ylab("Live planar area") +    
  ggtitle(~italic("Pavona duerdeni"))+
  scale_fill_manual(values = c("burlywood4", "cornsilk"))+
  annotate(geom="text", x="Apr 2010", y=590, label="n=8", size=3.5)

###
Pocillopora_damicornis2 <- total_data3$Pocillopora_damicornis %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Pigmented.area),
            SE= std.error(Pigmented.area),
            type="Pigmented.area")
as.data.frame(Pocillopora_damicornis2)
Pocillopora_damicornis3 <- total_data3$Pocillopora_damicornis %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Discolored.area),
            SE= std.error(Discolored.area),
            type="Discolored.area")
as.data.frame(Pocillopora_damicornis3)

Pocillopora_damicornis4=as.data.frame(rbind(Pocillopora_damicornis2, Pocillopora_damicornis3))

Pocillopora_damicornis4$type <- factor(Pocillopora_damicornis4$type, levels = c("Pigmented.area", "Discolored.area")) 
Pocillopora_damicornis4$y_pos = NA
Pocillopora_damicornis4$y_pos[Pocillopora_damicornis4$type == "Discolored.area"] = Pocillopora_damicornis4$Mean[Pocillopora_damicornis4$type == "Discolored.area"]
Pocillopora_damicornis4$y_pos[Pocillopora_damicornis4$type == "Pigmented.area"] = Pocillopora_damicornis4$Mean[Pocillopora_damicornis4$type == "Discolored.area"] + 
  Pocillopora_damicornis4$Mean[Pocillopora_damicornis4$type == "Pigmented.area"]

Pocillopora_damicornis_bar=ggplot(Pocillopora_damicornis4, aes(x = as.factor(ym), y = Mean, fill=type)) +
  theme_classic(base_size=11) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none", legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.x = element_text(angle=90, hjust = 0.6, vjust= 0.5)) +
  geom_bar(stat ="identity", position="stack", color="black")+
  geom_errorbar(aes(ymin=y_pos , ymax=y_pos+SE), position = "identity", width=0)+
  xlab("Date") + 
  ylab("Live planar area") +    
  ggtitle(~italic("Pocillopora damicornis"))+
  scale_fill_manual(values = c("burlywood4", "cornsilk"))+
  annotate(geom="text", x="Apr 2010", y=100, label="n=38", size=3.5)

###
Pocillopora_meandrina2 <- total_data3$Pocillopora_meandrina %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Pigmented.area),
            SE= std.error(Pigmented.area),
            type="Pigmented.area")
as.data.frame(Pocillopora_meandrina2)
Pocillopora_meandrina3 <- total_data3$Pocillopora_meandrina %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Discolored.area),
            SE= std.error(Discolored.area),
            type="Discolored.area")
as.data.frame(Pocillopora_meandrina3)

Pocillopora_meandrina4=as.data.frame(rbind(Pocillopora_meandrina2, Pocillopora_meandrina3))

Pocillopora_meandrina4$type <- factor(Pocillopora_meandrina4$type, levels = c("Pigmented.area", "Discolored.area")) 
Pocillopora_meandrina4$y_pos = NA
Pocillopora_meandrina4$y_pos[Pocillopora_meandrina4$type == "Discolored.area"] = Pocillopora_meandrina4$Mean[Pocillopora_meandrina4$type == "Discolored.area"]
Pocillopora_meandrina4$y_pos[Pocillopora_meandrina4$type == "Pigmented.area"] = Pocillopora_meandrina4$Mean[Pocillopora_meandrina4$type == "Discolored.area"] + 
  Pocillopora_meandrina4$Mean[Pocillopora_meandrina4$type == "Pigmented.area"]

Pocillopora_meandrina_bar=ggplot(Pocillopora_meandrina4, aes(x = as.factor(ym), y = Mean, fill=type)) +
  theme_classic(base_size=11) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none", legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.x = element_text(angle=90, hjust = 0.6, vjust= 0.5)) +
  geom_bar(stat ="identity", position="stack", color="black")+
  geom_errorbar(aes(ymin=y_pos , ymax=y_pos+SE), position = "identity", width=0)+
  xlab("Date") + 
  ylab("Live planar area") +    
  ggtitle(~italic("Pocillopora meandrina"))+
  scale_fill_manual(values = c("burlywood4", "cornsilk"))+
  annotate(geom="text", x="Apr 2010", y=225, label="n=140", size=3.5)

###
Stylophora_pistillata2 <- total_data3$Stylophora_pistillata %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Pigmented.area),
            SE= std.error(Pigmented.area),
            type="Pigmented.area")
as.data.frame(Stylophora_pistillata2)
Stylophora_pistillata3 <- total_data3$Stylophora_pistillata %>% 
  group_by(ym) %>% 
  summarise(Mean = mean(Discolored.area),
            SE= std.error(Discolored.area),
            type="Discolored.area")
as.data.frame(Stylophora_pistillata3)

Stylophora_pistillata4=as.data.frame(rbind(Stylophora_pistillata2, Stylophora_pistillata3))

Stylophora_pistillata4$type <- factor(Stylophora_pistillata4$type, levels = c("Pigmented.area", "Discolored.area")) 
Stylophora_pistillata4$y_pos = NA
Stylophora_pistillata4$y_pos[Stylophora_pistillata4$type == "Discolored.area"] = Stylophora_pistillata4$Mean[Stylophora_pistillata4$type == "Discolored.area"]
Stylophora_pistillata4$y_pos[Stylophora_pistillata4$type == "Pigmented.area"] = Stylophora_pistillata4$Mean[Stylophora_pistillata4$type == "Discolored.area"] + 
  Stylophora_pistillata4$Mean[Stylophora_pistillata4$type == "Pigmented.area"]

Stylophora_pistillata_bar=ggplot(Stylophora_pistillata4, aes(x = as.factor(ym), y = Mean, fill=type)) +
  theme_classic(base_size=11) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none", legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.text.x = element_text(angle=90, hjust = 0.6, vjust= 0.5)) +
  geom_bar(stat ="identity", position="stack", color="black")+
  geom_errorbar(aes(ymin=y_pos , ymax=y_pos+SE), position = "identity", width=0)+
  xlab("Date") + 
  ylab("Live planar area") +    
  ggtitle(~italic("Stylophora pistillata"))+
  scale_fill_manual(values = c("burlywood4", "cornsilk"))+
  annotate(geom="text", x="Apr 2010", y=450, label="n=7", size=3.5)

library(gridExtra)
All_spp_plot=grid.arrange(Astrea_curta_bar, Astreopora_myriophthalma_bar, Goniastrea_stelligera_bar,
                           Hydnophora_microconos_bar, Pavona_chiriquiensis_bar, Pavona_duerdeni_bar, 
                           Pocillopora_damicornis_bar, Pocillopora_meandrina_bar, Stylophora_pistillata_bar,
                           ncol=3,nrow=3)
