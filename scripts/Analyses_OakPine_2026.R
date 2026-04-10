library(lme4)
library(nlme)
library(multcomp)
library(gam)
library(glmmTMB)
library(DHARMa)
library(ade4)
library(labdsv)
library(indicspecies)
library(betapart)
library(ggplot2)
library(AICcmodavg)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(vegan)
library(fitdistrplus)

#options(constrasts=c("contr.treatment","contr.poly"))
#setwd("P:/Emmanuelle/MelangeEss_FOrl?ans/Analyses/These_JYB_2011")
#setwd("Z:/projets/MelangeEss_FOrleans/TheseJYB/Analyses\These_JYB_2011")
setwd("C:/Users/farchaux/Documents/OakPine/inrae-tree-mixture-communities/data")

##############################################################################################
##############################################################################################
###########################     CARABIDS    ##################################################
##############################################################################################
##############################################################################################


Carab.Rel.Env<-read.csv("Rel_Env_Carab_2026.csv", sep=";", header=T)
dim(Carab.Rel.Env)

#Reducing the number of levels of ground vegetation type Fougère-Aigle, Molinie, Autres végétations)
Carab.Rel.Env$veg1[Carab.Rel.Env$veg1=="BRYO"]<-"Other"
Carab.Rel.Env$veg1[Carab.Rel.Env$veg1=="CH300"]<-"Other"
Carab.Rel.Env$veg1[Carab.Rel.Env$veg1=="CH600"]<-"Other"
Carab.Rel.Env$veg1[Carab.Rel.Env$veg1=="RON"]<-"Other"

#Reordering tree mixture categories along a gradient of increasing oak (deciduous) basal area 
Carab.Rel.Env$MEL_cat<- factor(Carab.Rel.Env$MEL_cat, levels = c("Pine", "Mixed", "Oak"))
Carab.Rel.Env$MEL_cercle_cat<- factor(Carab.Rel.Env$MEL_cat, levels = c("Pine", "Mixed", "Oak"))

hist(Carab.Rel.Env$RS_all) #family Poisson
hist(Carab.Rel.Env$Abdce_all) #family nbinom1 in glmmTMB


plot(x=Carab.Rel.Env$G_Ch_PS,y=Carab.Rel.Env$taux_veg1)

plot(Carab.Rel.Env$RS_all~Carab.Rel.Env$MEL_parcelle)


# Visual inspection of total abundance and local tree mixture
gam_model <- gam(Abdce_all ~ s(MEL_cercle,k=2), data = Carab.Rel.Env)

# Create prediction grid
MEL_cercle_seq <- seq(min(Carab.Rel.Env$MEL_cercle), max(Carab.Rel.Env$MEL_cercle), length.out = 200)
pred <- predict(
  gam_model,
  newdata = data.frame(MEL_cercle = MEL_cercle_seq),
  se.fit = TRUE
)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  MEL_cercle = MEL_cercle_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit
)

# Plot with ggplot2
ggplot(pred_df, aes(x = MEL_cercle, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Carab.Rel.Env, aes(x = MEL_cercle, y = Abdce_all), color = "black") +
  labs(title = "GAM: Abdce_all ~ s(MEL_cercle) with 95% CI", x = "Deciduous basal area (%)", y = "Number of individuals (all species)") +
  theme_minimal()

boxplot(Carab.Rel.Env$Abdce_all~Carab.Rel.Env$MEL_cercle_cat) #donne l'impression que l'abondance est maximale dans les pins purs

# Visual inspection of taxonomic richness and local tree mixture
gam_model <- gam(RS_all ~ s(MEL_cercle), data = Carab.Rel.Env)

# Create prediction grid
MEL_cercle_seq <- seq(min(Carab.Rel.Env$MEL_cercle), max(Carab.Rel.Env$MEL_cercle), length.out = 200)
pred <- predict(
  gam_model,
  newdata = data.frame(MEL_cercle = MEL_cercle_seq),
  se.fit = TRUE
)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  MEL_cercle = MEL_cercle_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit
)

# Plot with ggplot2
ggplot(pred_df, aes(x = MEL_cercle, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Carab.Rel.Env, aes(x = MEL_cercle, y = RS_all), color = "black") +
  labs(title = "GAM: RS_all ~ s(MEL_cercle) with 95% CI", x = "Deciduous basal area (%)", y = "Species richness (all)") +
  theme_minimal()

boxplot(Carab.Rel.Env$RS_all~Carab.Rel.Env$MEL_cercle_cat)
  
####################################################################################################################################
#Test for linear or quadratic relationship of taxonomic diversity/total abundance and local tree mixture using glmmTMB to account for a random site (parcelle) effect
####################################################################################################################################

descdist(Carab.Rel.Env$RS_all,discrete=TRUE,boot=1001)
plot(fitdist(Carab.Rel.Env$RS_all,"norm"))
fitnb<-fitdist(Carab.Rel.Env$RS_all,"nbinom")
fitp<-fitdist(Carab.Rel.Env$RS_all,"pois")
fitn<-fitdist(Carab.Rel.Env$RS_all,"norm")
fitlnorm<-fitdist(Carab.Rel.Env$RS_all,"lnorm")
fitexp<-fitdist(Carab.Rel.Env$RS_all,"exp")
fitgeom<-fitdist(Carab.Rel.Env$RS_all,"geom")
fitbeta<-fitdist(Carab.Rel.Env$RS_all,"beta")
fitunif<-fitdist(Carab.Rel.Env$RS_all,"unif")
fitg<-fitdist(Carab.Rel.Env$RS_all,"gamma",method="mme")
gofstat(fitnb)$chisqpvalue
gofstat(fitp)$chisqpvalue
gofstat(fitn)$chisqpvalue
gofstat(fitlnorm)$chisqpvalue
gofstat(fitexp)$chisqpvalue
gofstat(fitgeom)$chisqpvalue
gofstat(fitunif)$chisqpvalue
gofstat(fibeta)$chisqpvalue
gofstat(fitg)$chisqpvalue

glmm_RS_all<-glmmTMB(RS_all~G_all_cercle+veg1+I(MEL_cercle/100)+I((MEL_cercle/100)^2)+(1|plot),family=poisson,data=Carab.Rel.Env)
summary(glmm_RS_all) #simple effect, p=0.12, quadratic effect p=0.29, G p=0.31, taux_veg1=0.83, AIC=285.2

glmm_RS_all<-glmmTMB(RS_all~G_all_cercle+veg1+I(MEL_cercle/100)+(1|plot),family=poisson,data=Carab.Rel.Env)
summary(glmm_RS_all) #simple effect, p=0.0377, AIC=284.3

#Test for linear or quadratic relationship of total abundance and local tree mixture using glmmTMB to account for a random site (parcelle) effect

descdist(Carab.Rel.Env$Abdce_all,discrete=TRUE,boot=1001)
plot(fitdist(Carab.Rel.Env$Abdce_all,"norm"))
fitnb<-fitdist(Carab.Rel.Env$Abdce_all,"nbinom")
fitp<-fitdist(Carab.Rel.Env$Abdce_all,"pois")
fitn<-fitdist(Carab.Rel.Env$Abdce_all,"norm")
fitlnorm<-fitdist(Carab.Rel.Env$Abdce_all,"lnorm")
fitexp<-fitdist(Carab.Rel.Env$Abdce_all,"exp")
fitgeom<-fitdist(Carab.Rel.Env$Abdce_all,"geom")
fitbeta<-fitdist(Carab.Rel.Env$Abdce_all,"beta")
fitunif<-fitdist(Carab.Rel.Env$Abdce_all,"unif")
fitg<-fitdist(Carab.Rel.Env$Abdce_all,"gamma",method="mme")
gofstat(fitnb)$chisqpvalue
gofstat(fitp)$chisqpvalue
gofstat(fitn)$chisqpvalue
gofstat(fitlnorm)$chisqpvalue
gofstat(fitexp)$chisqpvalue
gofstat(fitgeom)$chisqpvalue
gofstat(fitunif)$chisqpvalue
gofstat(fibeta)$chisqpvalue
gofstat(fitg)$chisqpvalue

glmm_Abdce_all_nb<-glmmTMB(Abdce_all~G_all+I(MEL_cercle/100)+I((MEL_cercle/100)^2)+(1|plot),family=nbinom1(),data=Carab.Rel.Env)
summary(glmm_Abdce_all_nb)#G coeff=0.06 p=0.03, MEL coeff=1.42 p=0.036, MEL^2 coeff=-0.91 p=0.145 AIC 539.7

sim<-simulateResiduals(glmm_Abdce_all_nb)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value # Dispersion test  # Outliers 
testOutliers(sim)

#Data: Carab.Rel.Env
#AIC       BIC    logLik -2*log(L)  df.resid 
#540.1     553.4    -264.1     528.1        62 
#
#Random effects:
#  
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#plot   (Intercept) 0.2262   0.4756  
#Number of obs: 68, groups:  plot, 22
#
#Dispersion parameter for nbinom1 family (): 2.48 
#
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)            1.73109    0.73971   2.340   0.0193 *
#  G_all                  0.04596    0.02820   1.630   0.1031  
#I(MEL_cercle/100)      1.26356    0.67067   1.884   0.0596 .
#I((MEL_cercle/100)^2) -0.90799    0.63215  -1.436   0.1509  



# #magnitude
#récupérer les valeurs de Estimate et Std. Error pour la variable d’intérêt X
Estimate_mod<-rnorm(10000,mean=37.7373,sd=18.2535)
DX<-exp(Estimate_mod*0.1)-1 #pour un delta de X de 10 (%)
mean(DX)
quantile(DX, c(0.01, 0.99))

#positive effect of basal area and tree mixture (potentially humped-shaped)


#####################################################
##############         ACP      #####################
#####################################################

Carab.Rel.Spe<-read.csv("Rel_Sp_Carab_2026.csv", sep=";", header=T)
# Analyse en composantes principales
Carab_acp <- PCA(Carab.Rel.Spe[,c(2:29)], graph = FALSE)

# Visualisation avec regroupement par modalité
fviz_pca_ind(
  Carab_acp,
  geom.ind = "point",       # Affiche les individus
  col.ind = Carab.Rel.Env$MEL_cercle_cat,   # Couleur selon la modalité
  palette = "jco",          # Palette de couleurs
  addEllipses = TRUE,       # Ellipses de confiance par groupe
  )

#####################################################
##############         RDA      #####################
#####################################################
Carab.rda <- rda(Carab.Rel.Spe[,c(2:29)])
biplot(Carab.rda)
ordiellipse(Carab.rda,group = Carab.Rel.Env$MEL_cercle_cat,col = c(1,2,3),label=TRUE)

##################################################################################################
##############      RLQ analysis   ###############################################################
##################################################################################################

Carab.Rel.Spe<-read.csv("Rel_Sp_Carab_2026.csv", sep=";", header=T)
dim(Carab.Rel.Spe)
Carab.Sp.Trait<-read.csv("Sp_Trait_Carab_2026.csv", sep=";", header=T)
dim(Carab.Sp.Trait)

#Merging dimorphic and polymorphic species (only one species)
Carab.Sp.Trait$Wing_type[Carab.Sp.Trait$Wing_type=="polymorf"]<-"dimorf"

afcL.Carab <- dudi.coa(Carab.Rel.Spe[,c(2:29)], scannf = FALSE)
acpR.Carab <- dudi.hillsmith(Carab.Rel.Env[,c("MEL_cercle","G_all")], row.w = afcL.Carab$lw,
                             scannf = FALSE)
acpQ.Carab <- dudi.pca(Carab.Sp.Trait[,c(6:8,10:12,14:15)], row.w = afcL.Carab$cw,
                       scannf = FALSE)
rlq.Carab <- rlq(acpR.Carab, afcL.Carab, acpQ.Carab,
                 scannf = FALSE)
plot(rlq.Carab)

par(mfrow = c(1, 3))
s.arrow(rlq.Carab$l1)
s.arrow(rlq.Carab$c1)
s.label(rlq.Carab$lQ, label=Carab.Sp.Trait$code_sp,boxes = TRUE)

###############################################################################################
###########################                IndVal          ####################################
###############################################################################################
indval_Carab <- multipatt(Carab.Rel.Spe[,c(2:29)], Carab.Rel.Env$MEL_cercle_cat,control = how(nperm=999)) 
#indval_Carab<-indval(Carab.Rel.Spe[,c(2:29)],clustering=cluster,numitr=1000)
summary(indval_Carab)


################################################################################################
###################      Beta partitioning (Baselga 2017 MEE) ##################################
################################################################################################

aa<-betapart.core.abund(Carab.Rel.Spe[,c(2:29)])
beta.multi.abund(aa, index.family="bray")

bb<-beta.sample.abund(Carab.Rel.Spe[,c(2:29)], index.family="bray", sites = 10, samples = 10000)
plot(bb$sampled.values)

##############################################################################################
##############################################################################################
###########################     BIRDS    #####################################################
##############################################################################################
##############################################################################################

par(mfrow = c(1, 1))

Bird.Rel.Env.Sp<-read.csv("Rel_Env_Sp_Bird_2026.csv", sep=";", header=T)
dim(Bird.Rel.Env.Sp)

rs.guild.saprox<-read.table("tabplotguilds.txt",header=T)


#Reordering tree mixture categories along a gradient of increasing oak (deciduous) basal area 
Bird.Rel.Env.Sp$MEL_cat<- factor(Bird.Rel.Env.Sp$MEL_point_cat, levels = c("Pine", "Mixed", "Oak"))

hist(Bird.Rel.Env.Sp$SR_all) #family Poisson
mean(Bird.Rel.Env.Sp$SR_all) #12.82
var(Bird.Rel.Env.Sp$SR_all) #8.24 

hist(Bird.Rel.Env.Sp$Abdce_all) #family Poisson
mean(Bird.Rel.Env.Sp$Abdce_all) #19.76
var(Bird.Rel.Env.Sp$Abdce_all) #23.37

plot(Bird.Rel.Env.Sp$SR_all~Bird.Rel.Env.Sp$MEL_point)


# Visual inspection of total abundance and local tree mixture
gam_model <- gam(Abdce_all ~ s(MEL_point), data = Bird.Rel.Env.Sp)

# Create prediction grid
MEL_seq <- seq(min(Bird.Rel.Env.Sp$MEL_point), max(Bird.Rel.Env.Sp$MEL_point), length.out = 200)
pred <- predict(
  gam_model,
  newdata = data.frame(MEL_point = MEL_seq),
  se.fit = TRUE
)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  MEL_point = MEL_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit
)

# Plot with ggplot2
ggplot(pred_df, aes(x = MEL_point, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = MEL_point, y = Abdce_all), color = "black") +
  labs(title = "GAM: Abdce_all ~ s(MEL_point) with 95% CI", x = "Deciduous basal area (%)", y = "Number of individuals (all species)") +
  theme_minimal()

#almost linear increase of total abundance with % of oak (although kind of plateau)

boxplot(Bird.Rel.Env.Sp$Abdce_all~Bird.Rel.Env.Sp$MEL_cat) #plateauing increase of total abundance with proportion of oaks

# Visual inspection of taxonomic richness and local tree mixture
gam_model <- gam(SR_all ~ s(MEL,k=3), data = Bird.Rel.Env.Sp)

# Create prediction grid
MEL_seq <- seq(min(Bird.Rel.Env.Sp$MEL), max(Bird.Rel.Env.Sp$MEL), length.out = 200)
pred <- predict(
  gam_model,
  newdata = data.frame(MEL = MEL_seq),
  se.fit = TRUE
)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  MEL = MEL_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit
)

# Plot with ggplot2
ggplot(pred_df, aes(x = MEL, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = MEL, y = SR_all), color = "black") +
  labs(title = "GAM: SR_all ~ s(MEL) with 95% CI", x = "Deciduous basal area (%)", y = "Species richness (all)") +
  theme_minimal()

boxplot(Bird.Rel.Env.Sp$SR_all~Bird.Rel.Env.Sp$MEL_cat) #globally same pattern as total abundance (plateau)

####################################################################################################################################
#Test for linear or quadratic relationship of taxonomic diversity and local tree mixture using glmmTMB to account for a random site (parcelle) effect
####################################################################################################################################

glmm_SR_all<-glmmTMB(SR_all~G_all+I(MEL_point/100)+I((MEL_point/100)^2)+(1|plot),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_all) #simple effect, p=0.12, quadratic effect p=0.29, G p=0.31, taux_veg1=0.83, AIC=285.2
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)           2.363951   0.230875  10.239   <2e-16 ***
#  G_all                -0.002286   0.008852  -0.258   0.7962    
#I(MEL_point/100)      1.173185   0.508405   2.308   0.0210 *  
#  I((MEL_point/100)^2) -1.016384   0.608559  -1.670   0.0949 .  
AICc(glmm_SR_all) #[1] 334.2836


glmm_SR_all<-glmmTMB(SR_all~G_all+I(MEL_point/100)+(1|plot),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_all) #simple effect, p=0.0377, AIC=284.3
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       2.4284617  0.2252669  10.780  < 2e-16 ***
#  G_all            -0.0009063  0.0087510  -0.104  0.91751    
#I(MEL_point/100)  0.3535493  0.1354959   2.609  0.00907 ** 
AICc(glmm_SR_all) #334.7857

#Almost linear increase of species richness with % of oak (no effect of basal area)

##################################################################################################
########################   Test for linear or quadratic relationship of total abundance and local tree mixture using glmmTMB to account for a random site (parcelle) effect
##################################################################################################

glmm_Abdce_all<-glmmTMB(Abdce_all~G_all+I(MEL_point/100)+I((MEL_point/100)^2)+(1|plot),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_all)
#(Intercept)           2.7729872  0.1855616  14.944   <2e-16 ***
#  G_all                -0.0004843  0.0071010  -0.068   0.9456    
#I(MEL_point/100)      0.9198519  0.4061184   2.265   0.0235 *  
#  I((MEL_point/100)^2) -0.6660573  0.4838513  -1.377   0.1686  
AICc(glmm_Abdce_all) #388.9731

glmm_Abdce_all<-glmmTMB(Abdce_all~G_all+I(MEL_point/100)+(1|plot),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_all)#G coeff=0.06 p=0.03, MEL coeff=0.48 p=0.0291, AIC 539.7
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      2.8173088  0.1814950  15.523  < 2e-16 ***
#  G_all            0.0003549  0.0070422   0.050 0.959803    
#I(MEL_point/100) 0.3808931  0.1092179   3.487 0.000488 ***
AICc(glmm_Abdce_all) #388.5486

#positive effect of tree mixture (potentially humped-shaped)

###########################################################
##############        BIRD - PCA      #####################
###########################################################

# Analyse en composantes principales
Bird_acp <- PCA(Bird.Rel.Env.Sp[,c(8:46)], graph = FALSE)

# Visualisation avec regroupement par modalité
fviz_pca_ind(
  Bird_acp,
  geom.ind = "point",       # Affiche les individus
  col.ind = Bird.Rel.Env.Sp$MEL_point_cat,   # Couleur selon la modalité
  palette = "jco",          # Palette de couleurs
  addEllipses = TRUE,       # Ellipses de confiance par groupe
)


###########################################################
##############        BIRD - RDA      #####################
###########################################################

Bird.rda <- rda(Bird.Rel.Env.Sp[,c(8:46)])
biplot(Bird.rda,display = c("sites","species"),type = c("text","points"))
#ordihull(Bird.rda,group = Bird.Rel.Env.Sp$MEL_point_cat,col = c(1,2,3),label=TRUE)
ordiellipse(Bird.rda,group = Bird.Rel.Env.Sp$MEL_point_cat,col = c(1,2,3),label=TRUE)

##############################################################
##############    BIRD - RLQ analysis   ###########################
##############################################################

Bird.Sp.Trait<-read.csv("Sp_Traits_Bird_2026.csv", sep=";", header=T)
dim(Bird.Sp.Trait)

afcL.Bird <- dudi.coa(Bird.Rel.Env.Sp[,c(8:46)], scannf = FALSE)
acpR.Bird <- dudi.hillsmith(Bird.Rel.Env.Sp[,c("MEL_point","G_all")], row.w = afcL.Bird$lw,
                             scannf = FALSE)
acpQ.Bird <- dudi.pca(Bird.Sp.Trait[,c(6:8,10:12,15:17)], row.w = afcL.Bird$cw,
                       scannf = FALSE)
rlq.Bird <- rlq(acpR.Bird, afcL.Bird, acpQ.Bird,
                 scannf = FALSE)
plot(rlq.Bird)

par(mfrow = c(1, 3))
s.arrow(rlq.Bird$l1)
s.arrow(rlq.Bird$c1)
s.label(rlq.Bird$lQ, label=Bird.Sp.Trait$Espece,boxes = TRUE)

################################################################################################
###########################                IndVal          ####################################
###############################################################################################
indval_Bird <- multipatt(Bird.Rel.Env.Sp[,c(8:46)], Bird.Rel.Env.Sp$MEL_point_cat,control = how(nperm=999)) 
summary(indval_Bird)

#Group Oak  #sps.  1 
#       stat    p.value   
#PHOPHO 0.736   0.007 **
  
#Group Mixed+Oak  #sps.  3 
#       stat    p.value    
#CERBRA 0.879   0.001 ***
#PARCAE 0.876   0.001 ***
#SITEUR 0.870   0.001 ***


################################################################################################
###################      Beta partitioning (Baselga 2017 MEE)  #################################
################################################################################################

aa.Bird<-betapart.core.abund(Bird.Rel.Env.Sp[,c(8:46)])
beta.multi.abund(aa.Bird, index.family="bray")
#$beta.BRAY.BAL
#[1] 0.9237699
#
#$beta.BRAY.GRA
#[1] 0.02037705

#$beta.BRAY
#[1] 0.944147


bb.Bird<-beta.sample.abund(Bird.Rel.Env.Sp[,c(8:46)], index.family="bray", sites = 10, samples = 10000)



par(mfrow = c(1, 1))

###############################################################################################
###############################################################################################
###########################     SAPROX BEETLES    ###########################################
##############################################################################################
##############################################################################################

Saprox.Rel.Env<-read.csv("Rel_Env_Saprox_2026.csv", sep=";", header=T)
dim(Saprox.Rel.Env) #48 29

Saprox.Rel.Sp<-read.csv("Rel_Sp_Saprox_2026.csv", sep=";", header=T)
dim(Saprox.Rel.Sp) #48 212

#Reordering tree mixture categories along a gradient of increasing oak (deciduous) basal area 
Saprox.Rel.Env$mel_trap_cat<- factor(Saprox.Rel.Env$mel_trap_cat, levels = c("Pine", "Mixed", "Oak"))


##############################################################################################
###########################     SAPROX Species richness    ##################################
##############################################################################################

#####################################
########RS all
################################

hist(Saprox.Rel.Sp$SR_all) #family Poisson?
mean(Saprox.Rel.Sp$SR_all) #38.29167
var(Saprox.Rel.Sp$SR_all) #59.70035

descdist(Saprox.Rel.Env$rs_all,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env$rs_all,"pois"))
fitnb<-fitdist(Saprox.Rel.Env$rs_all,"nbinom")
fitp<-fitdist(Saprox.Rel.Env$rs_all,"pois")
gofstat(fitnb)$chisqpvalue #0.5436261
gofstat(fitp)$chisqpvalue #0.7831754 --> Poisson


plot(Saprox.Rel.Env$rs_all~Saprox.Rel.Env$mel_trap)

# Visual inspection of taxonomic richness and local tree mixture
gam_model <- gam(rs_all ~ s(mel_trap,k=3), data = Saprox.Rel.Env)

# Create prediction grid
mel_seq <- seq(min(Saprox.Rel.Env$mel_trap), max(Saprox.Rel.Env$mel_trap), length.out = 200)
pred <- predict(
  gam_model,
  newdata = data.frame(mel_trap = mel_seq),
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mel_trap = mel_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mel_trap, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env, aes(x = mel_trap, y = SR_all), color = "black") +
  labs(title = "GAM: SR_all ~ s(mel_trap) with 95% CI", x = "Deciduous basal area (%)", y = "Species number (all species)") +
  theme_minimal()

boxplot(Saprox.Rel.Env$ab_all~Saprox.Rel.Env$mel_trap_cat)

glmm_rs_all<-glmmTMB(rs_all~G_all+I(mel_trap/100)+I((mel_trap/100)^2)+(1|plot),family=poisson,data=Saprox.Rel.Env)
summary(glmm_rs_all) 
#Conditional model:
#  Groups Name        Variance Std.Dev.
#plot   (Intercept) 0.006482 0.08051 
#Number of obs: 48, groups:  plot, 48
#
#Conditional model:
#                     Estimate  Std. Error  z value Pr(>|z|)    
#(Intercept)          3.852813   0.100305   38.41  < 2e-16 ***
#G_all               -0.009823   0.003373   -2.91  0.00358 ** 
#I(mel_trap/100)      0.400420   0.273439    1.46  0.14309    
#I((mel_trap/100)^2) -0.435797   0.264574   -1.65  0.09952 .  

sim<-simulateResiduals(glmm_rs_all)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value # Dispersion test  # Outliers 
testOutliers(sim)

#no effect of tree mixture on trap saproxylic species

#####################################
########RS conif
################################

descdist(Saprox.Rel.Env$rs.conif,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env$rs.conif,"pois"))
fitnb<-fitdist(Saprox.Rel.Env$rs.conif,"nbinom")
fitp<-fitdist(Saprox.Rel.Env$rs.conif,"pois")
gofstat(fitnb)$chisqpvalue #0.00205022 --> Poisson
gofstat(fitp)$chisqpvalue #1.239995e-28 


plot(Saprox.Rel.Env$rs_all~Saprox.Rel.Env$mel_trap)

# Visual inspection of taxonomic richness and local tree mixture
gam_model <- gam(rs.conif ~ s(mel_trap,k=3), data = Saprox.Rel.Env)

# Create prediction grid
mel_seq <- seq(min(Saprox.Rel.Env$mel_trap), max(Saprox.Rel.Env$mel_trap), length.out = 200)
pred <- predict(
  gam_model,
  newdata = data.frame(mel_trap = mel_seq),
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mel_trap = mel_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mel_trap, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env, aes(x = mel_trap, y = rs.conif), color = "black") +
  labs(title = "GAM: rs.conif ~ s(mel_trap) with 95% CI", x = "Deciduous basal area (%)", y = "Species number (conifer species)") +
  theme_minimal()

boxplot(Saprox.Rel.Env$ab_all~Saprox.Rel.Env$mel_trap_cat)

glmm_rs_conif<-glmmTMB(rs.conif~G_all+I(mel_trap/100)+I((mel_trap/100)^2)+(1|plot),family=poisson,data=Saprox.Rel.Env)
summary(glmm_rs_conif) 
# Family: poisson  ( log )
#Formula:          rs.conif ~ G_all + I(mel_trap/100) + I((mel_trap/100)^2) + (1 |      plot)
#Data: Saprox.Rel.Env
#AIC       BIC    logLik -2*log(L)  df.resid 
#232.5     241.8    -111.2     222.5        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#plot   (Intercept) 0.1564   0.3955  
#Number of obs: 48, groups:  plot, 48
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          2.47024    0.35331   6.992 2.71e-12 ***
#  G_all               -0.02078    0.01252  -1.659   0.0971 .  
#I(mel_trap/100)      0.50533    0.94191   0.536   0.5916    
#I((mel_trap/100)^2) -2.31423    1.01602  -2.278   0.0227 *  

sim<-simulateResiduals(glmm_rs_conif)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value # Dispersion test  # Outliers 
testOutliers(sim) # KS test p=0.81, Dispersion test, p=0.88, Outlier test, p=1

#quadratic effect of tree mixture on conifer species richness

#####################################
########RS deciduous
################################

descdist(Saprox.Rel.Env$rs.decid,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env$rs.decid,"pois"))
fitnb<-fitdist(Saprox.Rel.Env$rs.decid,"nbinom")
fitp<-fitdist(Saprox.Rel.Env$rs.decid,"pois")
gofstat(fitnb)$chisqpvalue #0.4257678 --> Negbin
gofstat(fitp)$chisqpvalue #0.1261187 


plot(Saprox.Rel.Env$rs.decid~Saprox.Rel.Env$mel_trap)

# Visual inspection of taxonomic richness and local tree mixture
gam_model <- gam(rs.decid ~ s(mel_trap,k=3), data = Saprox.Rel.Env)

# Create prediction grid
mel_seq <- seq(min(Saprox.Rel.Env$mel_trap), max(Saprox.Rel.Env$mel_trap), length.out = 200)
pred <- predict(
  gam_model,
  newdata = data.frame(mel_trap = mel_seq),
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mel_trap = mel_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mel_trap, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env, aes(x = mel_trap, y = rs.decid), color = "black") +
  labs(title = "GAM: rs.decid ~ s(mel_trap) with 95% CI", x = "Deciduous basal area (%)", y = "Species number (deciduous species)") +
  theme_minimal()

boxplot(Saprox.Rel.Env$ab_all~Saprox.Rel.Env$mel_trap_cat)

glmm_rs_decid<-glmmTMB(rs.decid~G_all+I(mel_trap/100)+I((mel_trap/100)^2)+(1|plot),family=poisson,data=Saprox.Rel.Env)
summary(glmm_rs_decid) 
#  Family: poisson  ( log )
#Formula:          rs.decid ~ G_all + I(mel_trap/100) + I((mel_trap/100)^2) + (1 |      plot)
#Data: Saprox.Rel.Env
#AIC       BIC    logLik -2*log(L)  df.resid 
#308.5     317.8    -149.2     298.5        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev.
#plot   (Intercept) 0.0002539 0.01593 
#Number of obs: 48, groups:  plot, 48
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          3.491904   0.103042   33.89  < 2e-16 ***
#  G_all               -0.009262   0.003401   -2.72  0.00647 ** 
#  I(mel_trap/100)      0.556113   0.281665    1.97  0.04834 *  
#  I((mel_trap/100)^2) -0.365468   0.267762   -1.36  0.17228    

sim<-simulateResiduals(glmm_rs_decid)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value 0.69 # Dispersion test 0.81 # Outliers 1 
testOutliers(sim) # RAS

#linear positive effect of tree mixture on deciduousspecies richness

#####################################
########RS generalist
################################

descdist(Saprox.Rel.Env$rs.gene,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env$rs.gene,"pois"))
fitnb<-fitdist(Saprox.Rel.Env$rs.gene,"nbinom")
fitp<-fitdist(Saprox.Rel.Env$rs.gene,"pois")
gofstat(fitnb)$chisqpvalue #NULL
gofstat(fitp)$chisqpvalue #0.1310683 


plot(Saprox.Rel.Env$rs.gene~Saprox.Rel.Env$mel_trap)

# Visual inspection of taxonomic richness and local tree mixture
gam_model <- gam(rs.gene ~ s(mel_trap,k=3), data = Saprox.Rel.Env)

# Create prediction grid
mel_seq <- seq(min(Saprox.Rel.Env$mel_trap), max(Saprox.Rel.Env$mel_trap), length.out = 200)
pred <- predict(
  gam_model,
  newdata = data.frame(mel_trap = mel_seq),
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mel_trap = mel_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mel_trap, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env, aes(x = mel_trap, y = rs.gene), color = "black") +
  labs(title = "GAM: rs.decid ~ s(mel_trap) with 95% CI", x = "Deciduous basal area (%)", y = "Species number (generalist species)") +
  theme_minimal()

boxplot(Saprox.Rel.Env$rs.gene~Saprox.Rel.Env$mel_trap_cat)

glmm_rs_gene<-glmmTMB(rs.gene~G_all+I(mel_trap/100)+I((mel_trap/100)^2)+(1|plot),family=poisson,data=Saprox.Rel.Env)
summary(glmm_rs_gene) 
# Family: poisson  ( log )
#Formula:          rs.gene ~ G_all + I(mel_trap/100) + I((mel_trap/100)^2) + (1 |      plot)
#Data: Saprox.Rel.Env
#AIC       BIC    logLik -2*log(L)  df.resid 
#142.5     151.8     -66.2     132.5        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#plot   (Intercept) 3.233e-10 1.798e-05
#Number of obs: 48, groups:  plot, 48
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)          0.55986    0.44625   1.255    0.210
#G_all               -0.01437    0.01465  -0.981    0.327
#I(mel_trap/100)      1.90950    1.21758   1.568    0.117
#I((mel_trap/100)^2) -1.82062    1.16456  -1.563    0.118

sim<-simulateResiduals(glmm_rs_gene)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value 0.14 # Dispersion test 0.016 # Outliers 1 
testOutliers(sim) # RAS

#linear positive effect of tree mixture on generalist pecies richness



##############################################################################################
###########################     SAPROX Total abundance    ##################################
##############################################################################################

descdist(Saprox.Rel.Env$ab_all,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env$ab_all,"pois"))
fitnb<-fitdist(Saprox.Rel.Env$ab_all,"nbinom")
fitp<-fitdist(Saprox.Rel.Env$ab_all,"pois")
gofstat(fitnb)$chisqpvalue #0.1367534 --> Negbin
gofstat(fitp)$chisqpvalue #0

# Visual inspection of total abundance and local tree mixture
gam_model <- gam(Abdce_all ~ s(mel_trap,k=3), data = Saprox.Rel.Env)

# Create prediction grid
mel_seq <- seq(min(Saprox.Rel.Env$mel_trap), max(Saprox.Rel.Env$mel_trap), length.out = 200)
pred <- predict(
  gam_model,
  newdata = data.frame(mel_trap = mel_seq),
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mel_trap = mel_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mel_trap, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env, aes(x = mel_trap, y = Abdce_all), color = "black") +
  labs(title = "GAM: Abdce_all ~ s(mel_trap) with 95% CI", x = "Deciduous basal area (%)", y = "Number of individuals (all species)") +
  theme_minimal()

boxplot(Saprox.Rel.Env$ab_all~Saprox.Rel.Env$mel_trap_cat) 

glmm_ab_all<-glmmTMB(ab_all~G_all+I(mel_trap/100)+I((mel_trap/100)^2)+(1|plot),family=nbinom1(),data=Saprox.Rel.Env)
summary(glmm_ab_all)
#Conditional model:
#  Groups Name        Variance Std.Dev.
#plot   (Intercept) 0.1891   0.4349  
#Number of obs: 48, groups:  plot, 48
#
#Dispersion parameter for nbinom1 family (): 2.29e-06 
#
#Conditional model:
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          5.258297   0.245479  21.421   <2e-16 ***
#G_all               -0.012561   0.008165  -1.538    0.124    
#I(mel_trap/100)      0.732365   0.669244   1.094    0.274    
#I((mel_trap/100)^2) -0.708099   0.645788  -1.096    0.273  
AICc(glmm_Abdce_all) #[1] 555.7867

sim<-simulateResiduals(glmm_ab_all)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value # Dispersion test  # Outliers 
testOutliers(sim) # KS test p=0.64, Dispersion test, p=095, Outlier test, p=1

#neither tree mixture, nor G influence total abundance

######################################################
##############        SAPROX - PCA      #####################
#####################################################

# Analyse en composantes principales
Saprox_acp <- PCA(Saprox.Rel.Sp[,c(7:210)], graph = FALSE)

# Visualisation avec regroupement par modalité
fviz_pca_ind(
  Saprox_acp,
  geom.ind = "point",       # Affiche les individus
  col.ind = Saprox.Rel.Env$mel_trap_cat,   # Couleur selon la modalité
  palette = "jco",          # Palette de couleurs
  addEllipses = TRUE,       # Ellipses de confiance par groupe
)

###########################################################
##############        BIRD - RDA      #####################
###########################################################

Saprox.rda <- rda(Saprox.Rel.Sp[,c(7:210)])
biplot(Saprox.rda,display = c("sites","species"),type = c("text","points"))
#ordihull(Saprox.rda,group = Saprox.Rel.Env.Sp$mel_trap_cat,col = c(1,2,3),label=TRUE)
ordiellipse(Saprox.rda,group = Saprox.Rel.Env$mel_trap_cat,col = c(1,2,3),label=TRUE)


##############################################################
##############    SAPROX - RLQ analysis   ###########################
##############################################################

Saprox.Sp.Trait<-read.csv("Sp_Traits_Sa^prox_2026.csv", sep=";", header=T)
dim(Bird.Sp.Trait)

afcL.Saprox <- dudi.coa(Saprox.Rel.Sp[,c(7:210)], scannf = FALSE)
acpR.Saprox <- dudi.hillsmith(Saprox.Rel.Env[,c("mel_trap","G_all")], row.w = afcL.Saprox$lw,
                            scannf = FALSE)
acpQ.Saprox <- dudi.pca(Saprox.Sp.Trait[,c(6:8,10:12,15:17)], row.w = afcL.Saprox$cw,
                      scannf = FALSE)
rlq.Saprox <- rlq(acpR.Saprox, afcL.Saprox, acpQ.Saprox,
                scannf = FALSE)
plot(rlq.Saprox)

par(mfrow = c(1, 3))
s.arrow(rlq.Saprox$l1)
s.arrow(rlq.Saprox$c1)
s.label(rlq.Saprox$lQ, label=Saprox.Sp.Trait$Espece,boxes = TRUE)

################################################################################################
###########################                IndVal          ####################################
###############################################################################################
indval_Saprox <- multipatt(Saprox.Rel.Sp[,c(7:210)], Saprox.Rel.Env$mel_trap_cat,control = how(nperm=999)) 
summary(indval_Saprox)

#  Group Mixed  #sps.  3 
#                         stat    p.value   
#  Silvanus.unidentatus   0.520   0.024 * 
#  Thymalus.limbatus      0.513   0.010 **
#  Octotemnus.glabriculus 0.459   0.034 * 
  
#  Group Oak  #sps.  1 
#                    stat    p.value   
#  Cryptarcha.undata 0.598   0.002 **
  
#  Group Pine  #sps.  4 
#                          stat    p.value    
#  Hylurgops.palliatus     0.856   0.001 ***
#  Tomicus.piniperda       0.839   0.001 ***
#  Pityophthorus.pubescens 0.652   0.005 ** 
#  Rhizophagus.depressus   0.612   0.008 ** 
  
#  Group Mixed+Oak  #sps.  2 
#                       stat    p.value  
#  Ampedus.quercicola   0.792   0.016 *
#  Isoriphis.melasoides 0.744   0.049 *
  
#  Group Mixed+Pine  #sps.  6 
#                                stat    p.value    
#  Hylastes.linearis             0.854   0.001 ***
#  Hylastes.attenuatus           0.806   0.007 ** 
#  Rhagium.mordax                0.664   0.002 ** 
#  Ampedus.sanguinolentus        0.652   0.018 *  
#  Glischrochilus.quadriguttatus 0.628   0.039 *  
#  Hylastes.opacus               0.594   0.037 *  
  
#  Group Oak+Pine  #sps.  1 
#                          stat    p.value  
#  Vincenzellus.ruficollis 0.707   0.027 *


################################################################################################
###################      Beta partitioning (Baselga 2017 MEE)  #################################
################################################################################################

aa.Saprox<-betapart.core.abund(Saprox.Rel.Sp[,c(7:210)])
beta.multi.abund(aa.Saprox, index.family="bray")
#$beta.BRAY.BAL
#[1] 0.9219115
#
#$beta.BRAY.GRA
#[1] 0.02802564#
#
#$beta.BRAY
#[1] 0.94993717


bb.Saprox<-beta.sample.abund(Saprox.Rel.Sp[,c(7:210)], index.family="bray", sites = 10, samples = 10000)


