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
  labs(x="Mixture (% oak vs pine + oak)",y="Species")

ridg4
