#------------------------------------------------------------------------#
## Compute density curves for species counts along the mixture gradient ##
# author : J-Y Barnagaud (jean-yves.barnagaud@ephe.psl.eu)
# This script computes probability density curves on species' counts per 
# sites along the mixture gradient
#------------------------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(ggridges)
library(viridis)

## birds -----------------------------------------------------------------------

# get data

ois <- read.csv2("data/Rel_Env_Sp_Bird_2026.csv")
ois.traits <- read.csv2("data/Sp_Traits_Bird_2026.csv")

# reframe to long

ois1 <- ois[,-c(2:7,47,48)]
rownames(ois1) <- ois$point
ois1.long=pivot_longer(ois1,cols=2:ncol(ois1),names_to="species",values_to="count")
mixture <- ois[,c("point","MEL_point")]

# subset trait matrix

ois.tr <- ois.traits[,c("Espece","Taxonomy.English.Name..BirdLife...IOC...Clements.AviList.","Primary.Diet")]
colnames(ois.tr) <- c("species","name","diet")

# merge with covariates

ois2 <- merge(ois1.long,mixture,by="point")
ois3 <- uncount(ois2,weights=count)
ois4 <- merge(ois3,ois.tr,by = "species", all.x = T, all.y = F)

# do the ridge plot with color scale ~ diet

ridg1 <- ois4 %>%
  mutate(species = fct_reorder(species, MEL_point)) %>%
  ggplot(aes(x = MEL_point, y = species,fill=diet, colour=diet)) + 
  geom_density_ridges(scale=1, alpha=0.6)+
  scale_colour_manual(values=c(inferno(10)[2],inferno(10)[4],inferno(10)[6],inferno(10)[9]), labels = c("carnivore","Invertebrate","Omnivore","Seed"))+
  scale_fill_manual(values=c(inferno(10)[2],inferno(10)[4],inferno(10)[6],inferno(10)[9]), labels = c("carnivore","Invertebrate","Omnivore","Seed"))+
  theme_classic()+
  xlim(0,100)+
  labs(x="Mixture (% oak vs pine + oak)",y="Species")+
  scale_y_discrete(labels = NULL)

ridg1

# same ridge plot with no color scale (might be more readable)

ridg2 <- ois4 %>%
  mutate(species = fct_reorder(species, MEL_point)) %>%
  ggplot(aes(x = MEL_point, y = species)) + 
  geom_density_ridges(scale=1, alpha=0.6,fill="black", colour="white")+
  theme_classic()+
  xlim(0,100)+
  labs(x="Mixture (% oak vs pine + oak)",y="Species")

ridg2

ggsave("outputs/birds-ridge-plot-all-species.png", width = 7, height = 15)

# same ridge plot, restrict to the 15 most common species

ois.b <- ois[,-c(1:7,47,48)]
freq.tot <- apply(ois.b,2,sum)
freq.tot.s <- rev(sort(freq.tot))
sp.com10 <- names(freq.tot.s[1:15])
ois4.com10 <- subset(ois4,species %in% sp.com10)

ridg3 <- ois4.com10 %>%
  mutate(species = fct_reorder(species, MEL_point)) %>%
  ggplot(aes(x = MEL_point, y = species)) + 
  geom_density_ridges(scale=1, alpha=0.6,fill="black", colour="white")+
  theme_classic()+
  xlim(0,100)+
  labs(x="Mixture (% oak vs pine + oak)",y="Species")

ridg3

ggsave("outputs/birds-ridge-plot-15-most-common-species.png", width = 7, height = 15)

# same ridge plot, restrict to species with >= 10% regional occurrence

freq <- apply(ois.b,2,function(x){sum(x>0)})
freq.rel <- freq/nrow(ois.b)
sp.frq10 <- names(which(freq.rel>=0.1))

ois4.frq10 <- subset(ois4,species %in% sp.frq10)

ridg4 <- ois4.frq10 %>%
  mutate(species = fct_reorder(species, MEL_point)) %>%
  ggplot(aes(x = MEL_point, y = species)) + 
  geom_density_ridges(scale=1, alpha=0.6,fill="black", colour="white")+
  theme_classic()+
  xlim(0,100)+
  labs(x="Mixture (% oak vs pine + oak)",y="Species with frequency > 10% all plots")

ridg4

ggsave("outputs/birds-ridge-plot-common-species.png", width = 7, height = 15)

## carabids---------------------------------------------------------------------

# data

carab <- read.csv2("data/Sp_Carab_2026_selec.csv")
env.carab <- read.csv2("data/Rel_Env_Carab_2026.csv",dec=".")

# clean matrix

colnames(carab) <- tolower(colnames(carab))

# reshape

carab.long <- pivot_longer(carab,cols=2:ncol(carab),names_to="species",values_to="count")

# add mixture level

env.carab1 <- env.carab[,c("plot_Ptrap","MEL_cercle")]
colnames(env.carab1) <- c("piege","MEL_piege")

carab2 <- merge(carab.long,env.carab1,by="piege")
carab3 <- uncount(carab2,weights=count)

# do the ridge plot

ridg.carab <- carab3 %>%
  mutate(species = fct_reorder(species, MEL_piege)) %>%
  ggplot(aes(x = MEL_piege, y = species)) + 
  geom_density_ridges(scale=1, alpha=0.6,fill="black", colour="white")+
  theme_classic()+
  xlim(0,100)+
  labs(x="Mixture (% oak vs pine + oak)",y="Species with frequency > 10% all plots")

ridg.carab

ggsave("outputs/carabid-ridge-plot-common-species.png", width = 7, height = 15)

## saproxylic stuff-------------------------------------------------------------

sapro <- read.csv2("data/tabplotsp_colsxselec.csv")
keep.sp <- read.csv2("data/guildsp_colsxselec.csv")
env.sapro <- read.csv2("data/Rel_Env_Saprox_2026.csv", dec = ".")

# keep only some species defined by C. Bouget

colkeep <- c("plot",keep.sp$species)
sapro1 <- sapro[,colkeep]

# reshape

sapro.long <- pivot_longer(sapro1,cols=2:ncol(sapro1),names_to="species",values_to="count")

# add mixture level

env.sapro1 <- env.sapro[,c("plot","mel1.8ha")]
sapro2 <- merge(sapro.long,env.sapro1,by="plot")
sapro3 <- uncount(sapro2,weights=count)

# add guilds to split the ridge plot

sapro4 <- merge(sapro3,keep.sp,by = "species", all = F)

decid.sapro4 <- subset(sapro4,guild == "decid")
gene.sapro4 <- subset(sapro4,guild == "generalist")
coni.sapro4 <- subset(sapro4,guild == "conif")

# do the ridge plots per guild

# do the ridge plot

ridg.decid.sapro <- decid.sapro4 %>%
  mutate(species = fct_reorder(species, mel1.8ha)) %>%
  ggplot(aes(x = mel1.8ha, y = species)) + 
  geom_density_ridges(scale=1, alpha=0.6,fill="black", colour="white")+
  theme_classic()+
  xlim(0,100)+
  labs(x="Mixture (% oak vs pine + oak)",y="deciduous-related species")

ridg.decid.sapro

ggsave("outputs/saprox-deciduous-species-ridge-plot.png", width = 7, height = 15)

ridg.coni.sapro <- coni.sapro4 %>%
  mutate(species = fct_reorder(species, mel1.8ha)) %>%
  ggplot(aes(x = mel1.8ha, y = species)) + 
  geom_density_ridges(scale=1, alpha=0.6,fill="black", colour="white")+
  theme_classic()+
  xlim(0,100)+
  labs(x="Mixture (% oak vs pine + oak)",y="conifer-related species")

ridg.coni.sapro

ggsave("outputs/saprox-conifer-species-ridge-plot.png", width = 7, height = 15)

ridg.gene.sapro <- gene.sapro4 %>%
  mutate(species = fct_reorder(species, mel1.8ha)) %>%
  ggplot(aes(x = mel1.8ha, y = species)) + 
  geom_density_ridges(scale=1, alpha=0.6,fill="black", colour="white")+
  theme_classic()+
  xlim(0,100)+
  labs(x="Mixture (% oak vs pine + oak)",y="generalist species")

ridg.gene.sapro

ggsave("outputs/saprox-generalist-species-ridge-plot.png", width = 7, height = 15)

