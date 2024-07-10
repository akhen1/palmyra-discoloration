#set working directory
setwd("C:/Users/adikh/Desktop")

#load data
data=read.csv(file="Planar area and discoloration by colony_focal species.csv", header=TRUE, sep=",")

#convert to date format
data$Date[data$Date == 909] <- '0909'
library(zoo)
ym <- as.yearmon(as.character(data$Date), "%y%m")
total_data=cbind(data,ym)

#assign full IDs
total_data$taxa=sapply(strsplit(as.character(total_data$Colony.ID),'_'),function(x) paste(x[is.na(as.numeric(x))],collapse='_'))
total_data$ID=paste(total_data$Site,total_data$Quad,total_data$Colony.ID, sep="_")

#calculate percent discoloration
total_data$Percent.discoloration=(total_data$Discolored.area/total_data$Total.area)*100
total_data$Percent.discoloration=as.numeric(total_data$Percent.discoloration)

#subset by taxon
total_data <- split(total_data,f = total_data$Genus_species)
names(total_data)

library(lme4)
library(lmerTest)

#linear mixed effects models for live planar area
Astrea_curta_lme = lmer(Total.area ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Astrea_curta)
anova(Astrea_curta_lme, type=1)

Astreopora_myriophthalma_lme = lmer(Total.area ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Astreopora_myriophthalma)
anova(Astreopora_myriophthalma_lme, type=1)

#use type-3 instead for testing interactions
Goniastrea_stelligera_lme = lmer(Total.area ~ as.numeric(DHW) + as.numeric(Months) * as.factor(Habitat) + (1|ID), data=total_data$Goniastrea_stelligera)
anova(Goniastrea_stelligera_lme, type=3)

Hydnophora_microconos_lme = lmer(Total.area ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Hydnophora_microconos)
anova(Hydnophora_microconos_lme, type=1)

Pavona_chiriquiensis_lme = lmer(Total.area ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Pavona_chiriquiensis)
anova(Pavona_chiriquiensis_lme, type=1)

Pavona_duerdeni_lme = lmer(Total.area ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Pavona_duerdeni)
anova(Pavona_duerdeni_lme, type=1)

Pocillopora_damicornis_lme = lmer(Total.area ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Pocillopora_damicornis)
anova(Pocillopora_damicornis_lme, type=1)

Pocillopora_meandrina_lme = lmer(Total.area ~ as.numeric(DHW) + as.numeric(Months) * as.factor(Habitat) + (1|ID), data=total_data$Pocillopora_meandrina)
anova(Pocillopora_meandrina_lme, type=3)

Stylophora_pistillata_lme = lmer(Total.area ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Stylophora_pistillata)
anova(Stylophora_pistillata_lme, type=1)

#linear mixed effects models for percent discoloration
Astrea_curta_lme2 = lmer(Percent.discoloration ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Astrea_curta)
anova(Astrea_curta_lme2, type=1)

Astreopora_myriophthalma_lme2 = lmer(Percent.discoloration ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Astreopora_myriophthalma)
anova(Astreopora_myriophthalma_lme2, type=1)

#use type-3 for testing interactions
Goniastrea_stelligera_lme2 = lmer(Percent.discoloration ~ as.numeric(DHW) + as.numeric(Months) * as.factor(Habitat) + (1|ID), data=total_data$Goniastrea_stelligera)
anova(Goniastrea_stelligera_lme2, type=3)

Hydnophora_microconos_lme2 = lmer(Percent.discoloration ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Hydnophora_microconos)
anova(Hydnophora_microconos_lme2, type=1)

Pavona_chiriquiensis_lme2 = lmer(Percent.discoloration ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Pavona_chiriquiensis)
anova(Pavona_chiriquiensis_lme2, type=1)

Pavona_duerdeni_lme2 = lmer(Percent.discoloration ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Pavona_duerdeni)
anova(Pavona_duerdeni_lme2, type=1)

Pocillopora_damicornis_lme2 = lmer(Percent.discoloration ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Pocillopora_damicornis)
anova(Pocillopora_damicornis_lme2, type=1)

Pocillopora_meandrina_lme2 = lmer(Percent.discoloration ~ as.numeric(DHW) + as.numeric(Months) * as.factor(Habitat) + (1|ID), data=total_data$Pocillopora_meandrina)
anova(Pocillopora_meandrina_lme2, type=3)

Stylophora_pistillata_lme2 = lmer(Percent.discoloration ~ as.numeric(DHW) + as.numeric(Months) + (1|ID), data=total_data$Stylophora_pistillata)
anova(Stylophora_pistillata_lme2, type=1)
