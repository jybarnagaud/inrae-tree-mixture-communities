library(lme4)
library(nlme)
library(multcomp)
library(gam)
library(glmmTMB)
library(DHARMa)
library(ade4)
library(subniche)
library(knitr)
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
library(adespatial)

#options(constrasts=c("contr.treatment","contr.poly"))
#setwd("P:/Emmanuelle/MelangeEss_FOrl?ans/Analyses/These_JYB_2011")
#setwd("Z:/projets/MelangeEss_FOrleans/TheseJYB/Analyses\These_JYB_2011")
setwd("C:/Users/farchaux/Documents/OakPine/inrae-tree-mixture-communities/data")

##############################################################################################
##############################################################################################
###########################     GROUND BEETLES    ############################################
##############################################################################################
##############################################################################################

Carab.Rel.Env.Sp<-read.csv("Rel_Env_Sp_Carab_2026.csv", sep=";", header=T)

###############################################################################################
########################### GROUND BEETLES - GLMM          ####################################
###############################################################################################

###GROUND BEETLES - Species richness all species

#GROUND BEETLES - Statistical distribution

descdist(Carab.Rel.Env.Sp$SR_all,discrete=TRUE,boot=1001)
plot(fitdist(Carab.Rel.Env.Sp$SR_all,"norm"))
fitnb<-fitdist(Carab.Rel.Env.Sp$SR_all,"nbinom")
fitp<-fitdist(Carab.Rel.Env.Sp$SR_all,"pois")
gofstat(fitnb)$chisqpvalue #0.004523473
gofstat(fitp)$chisqpvalue #0.004523473 --> Poisson

glmm_SR_all_quad<-glmmTMB(SR_all~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Carab.Rel.Env.Sp)
glmm_SR_all_simple<-glmmTMB(SR_all~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Carab.Rel.Env.Sp)
AICc(glmm_SR_all_quad)#286.3324 (delatAICc 1.8895)
AICc(glmm_SR_all_simple)#284.4429

summary(glmm_SR_all_simple)
#Family: poisson  ( log )
#Formula:          SR_all ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Carab.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#283.8     292.7    -137.9     275.8        64 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 2.639e-10 1.624e-05
#Number of obs: 68, groups:  stand, 15
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         1.130849   0.304644   3.712 0.000206 ***
#  G_all_plot          0.018681   0.009855   1.896 0.058023 .  
#I(mixture_plot/100) 0.307193   0.160231   1.917 0.055214 .

# Create prediction grid
mixture_seq <- seq(min(Carab.Rel.Env.Sp$mixture_plot), max(Carab.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Carab.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
stand_seq<-rep("245",200)
pred <- predict(
  glmm_SR_all_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot=mixture_seq,stand=stand_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Carab.Rel.Env.Sp, aes(x = mixture_plot, y = SR_all), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of carabid species (all)") +
  theme_minimal()

#GROUND BEETLES - Abundance all species

descdist(Carab.Rel.Env.Sp$Abdce_all,discrete=TRUE,boot=1001)
fitnb<-fitdist(Carab.Rel.Env.Sp$Abdce_all,"nbinom")
fitp<-fitdist(Carab.Rel.Env.Sp$Abdce_all,"pois")
gofstat(fitnb)$chisqpvalue #0.4665424 --> NegBin
gofstat(fitp)$chisqpvalue #0

glmm_Abdce_all_nb_quad<-glmmTMB(Abdce_all~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=nbinom1(),data=Carab.Rel.Env.Sp)
glmm_Abdce_all_nb_simple<-glmmTMB(Abdce_all~G_all_plot+I(mixture_plot/100)+(1|stand),family=nbinom1(),data=Carab.Rel.Env.Sp)
AICc(glmm_Abdce_all_nb_quad) #531.3036 
AICc(glmm_Abdce_all_nb_simple) #528.9225

sim<-simulateResiduals(glmm_Abdce_all_nb_simple)
testUniformity(sim)#D = 0.06594, p-value = 0.9099 
#KS Test p-value 0.96# Dispersion test  # Outliers 
testOutliers(sim) #p-value = 1

summary(glmm_Abdce_all_nb_simple) 
#Family: nbinom1  ( log )
#Formula:          Abdce_all ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Carab.Rel.Env
#AIC       BIC    logLik -2*log(L)  df.resid 
#528.0     539.1    -259.0     518.0        63 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.1966   0.4433  
#Number of obs: 68, groups:  stand, 15
#Dispersion parameter for nbinom1 family ():  2.5 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      2.29012    0.43665   5.245 1.57e-07 ***
#  G_all_plot       0.03121    0.01440   2.167   0.0303 *  
#  I(ixture_plot/100)  0.23098    0.32166   0.718   0.4727

# Create prediction grid
mixture_seq <- seq(min(Carab.Rel.Env.Sp$mixture_plot), max(Carab.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Carab.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
stand_seq<-rep("245",200)
pred <- predict(
  glmm_Abdce_all_nb_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot=mixture_seq,stand=stand_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Carab.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_all), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of carabid indivduals (all)") +
  theme_minimal()

# #magnitude
#récupérer les valeurs de Estimate et Std. Error pour la variable d’intérêt X
Estimate_mod<-rnorm(10000,mean=0.23098,0.32166)
DX<-exp(Estimate_mod*0.1)-1 #pour un delta de X de 10 (%)
mean(DX)
quantile(DX, c(0.01, 0.99))


###############################################################################################
########################### GROUND BEETLES - PCA          ####################################
###############################################################################################

Carab.Rel.Spe<-read.csv("Rel_Sp_Carab_2026.csv", sep=";", header=T)
# Analyse en composantes principales
Carab_acp <- PCA(Carab.Rel.Spe[,c(2:29)], graph = FALSE)
Carab.Rel.Env$cat_mel_plot<- factor(Carab.Rel.Env$cat_mel_plot, levels = c("pine", "mixed", "oak"))


# Visualisation avec regroupement par modalité
fviz_pca_ind(
  Carab_acp,
  geom.ind = "point",       # Affiche les individus
  col.ind = Carab.Rel.Env$MEL_cercle_cat,   # Couleur selon la modalité
  palette = "jco",          # Palette de couleurs
  addEllipses = TRUE,       # Ellipses de confiance par groupe
  )

###############################################################################################
########################### GROUND BEETLES - RDA          ####################################
###############################################################################################
Carab.rda <- rda(Carab.Rel.Spe[,c(2:29)])
biplot(Carab.rda)
ordiellipse(Carab.rda,group = Carab.Rel.Env$cat_mel_plot,col = c(1,2,3),label=TRUE)

###############################################################################################
########################### GROUND BEETLES - OMI          ####################################
###############################################################################################

dudi1 <- dudi.pca(Carab.Rel.Env.Sp[,c("mixture_plot","G_all_plot")], scale = TRUE, scan = FALSE, nf = 2)
scatter(dudi1)

nic1 <- niche(dudi1,Y=Carab.Rel.Env.Sp[,c(10:37)], scann = FALSE)
nic1
summary(nic1)
plot(nic1)

kable(niche.param(nic1))
rtest(nic1,100)
#class: krandtest lightkrandtest 
#Monte-Carlo tests
#Call: as.krandtest(sim = t(sim), obs = obs)
#Number of tests:   29 
#Adjustment method for multiple comparisons:   none 
#Permutation number:   100 
#Test        Obs    Std.Obs   Alter     Pvalue
#1      abat 0.11725202  2.9088719 greater 0.02970297
#2      abov 1.86614671  1.4739514 greater 0.09900990
#3      abpa 0.12470043  2.6896586 greater 0.02970297
#4      amlu 0.03894376 -0.7884762 greater 0.86138614
#5      amsi 0.40696997 -0.5907334 greater 0.61386139
#6      amti 0.79438902 -0.6961014 greater 0.76237624
#7      babu 1.03076530  0.4028889 greater 0.25742574
#8      caau 0.07343171 -0.7117005 greater 0.73267327
#9      cacr 0.59539415  3.0931025 greater 0.02970297
#10     cacv 0.39509424  0.2833866 greater 0.29702970
#11     cait 1.35213982  2.6184326 greater 0.03960396
#12     cane 0.33096495  2.3946216 greater 0.04950495
#13     capr 0.16746122  5.6359113 greater 0.00990099
#14     cavi 0.04766464  0.7192999 greater 0.14851485
#15     dygl 0.70239283 -0.4508069 greater 0.57425743
#16     lema 4.05097078  1.5922170 greater 0.14851485
#17     lias 0.31646146 -1.1820993 greater 0.94059406
#18     mela 0.31538080 -1.0952518 greater 0.84158416
#19     nebr 0.20205207 -0.8704986 greater 0.81188119
#20     nobi 0.09685728 -0.7654889 greater 0.78217822
#21     noru 0.14104717 -0.8186205 greater 0.88118812
#22     plli 3.70984783  1.3376932 greater 0.16831683
#23     pocu 1.03636359 -0.8102399 greater 0.79207921
#24     ptma 0.55965745  1.2856607 greater 0.12871287
#25     ptme 0.37079384 -0.4437152 greater 0.55445545
#26     ptni 0.35102073 -0.3434065 greater 0.47524752
#27     ptob 0.07683557 -0.8168526 greater 0.81188119
#28     ptst 0.31646146 -1.0506779 greater 0.92079208
#29 OMI.mean 0.69955217 -0.4881198 greater 0.70297030

# Creation of the factor
fact <- as.factor(Carab.Rel.Env.Sp$cat_mixture_plot)
# plotting the two subsets
s.class(nic1$ls, fact, col=c("red", "blue","black"),cellipse=0, cpoint=2, pch=3)
s.chull(nic1$ls, fact, col=c("red", "blue","black"),optchull = 1, add.plot = T)

###############################################################################################
########################### GROUND BEETLES - RLQ          ####################################
###############################################################################################

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
########################### GROUND BEETLES - IndVal        ####################################
###############################################################################################
indval_Carab <- multipatt(Carab.Rel.Spe[,c(2:29)], Carab.Rel.Env$cat_mel_plot,control = how(nperm=999)) 
#indval_Carab<-indval(Carab.Rel.Spe[,c(2:29)],clustering=cluster,numitr=1000)
summary(indval_Carab)


################################################################################################
################### GROUND BEETLES - Beta partitioning (Baselga 2017 MEE) ######################
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

Bird.Rel.Env.Sp<-read.csv("Rel_Env_Sp_Bird_2026.csv", sep=";", header=T)

#Reordering tree mixture categories along a gradient of increasing oak (deciduous) basal area 
Bird.Rel.Env.Sp$cat_mixture_plot<- factor(Bird.Rel.Env.Sp$cat_mixture_plot, levels = c("pine", "mixed", "oak"))

#############################################################################################
#####################      BIRDS - GLMM        ##############################################
#############################################################################################

#####################      BIRDS - Species richness        ##################################

#### BIRDS - Species richness all species

glmm_SR_all_quad<-glmmTMB(SR_all~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
glmm_SR_all_simple<-glmmTMB(SR_all~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
AICc(glmm_SR_all_quad) #334.2836
AICc(glmm_SR_all_simple) #334.7857

summary(glmm_SR_all_quad)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)           2.363951   0.230875  10.239   <2e-16 ***
#  G_all_plot                -0.002286   0.008852  -0.258   0.7962    
#I(mixture_plot/100)      1.173185   0.508405   2.308   0.0210 *  
#  I((mixture_plot/100)^2) -1.016384   0.608559  -1.670   0.0949 .  

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_all_quad,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = SR_all), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of bird species (all)") +
  theme_minimal()

#### BIRDS - Species richness of Generalist species

glmm_SR_Generalist_quad<-glmmTMB(SR_Generalist1~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
glmm_SR_Generalist_simple<-glmmTMB(SR_Generalist1~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
AICc(glmm_SR_Generalist_quad) #313.1851
AICc(glmm_SR_Generalist_simple) #312.0974

summary(glmm_SR_Generalist_simple)
# Family: poisson  ( log )
#Formula:          SR_Generalist1 ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#311.4     320.2    -151.7     303.4        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 8.384e-11 9.156e-06
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         2.024550   0.257871   7.851 4.13e-15 ***
#  G_all_plot          0.005075   0.009990   0.508   0.6114    
#I(mixture_plot/100) 0.308946   0.155143   1.991   0.0464 *  

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Generalist_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Generalist1), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of bird species (generalist)") +
  theme_minimal()

#### BIRDS - Species richness of Oak specialist species

glmm_SR_Oak_quad<-glmmTMB(SR_Oak~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
glmm_SR_Oak_simple<-glmmTMB(SR_Oak~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
AICc(glmm_SR_Oak_quad) #201.2625
AICc(glmm_SR_Oak_simple) #203.9422

summary(glmm_SR_Oak_quad)
# Family: poisson  ( log )
#Formula:          SR_Oak ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#200.3     211.2     -95.1     190.3        61 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 1.934e-09 4.398e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)             -1.000582   0.700055  -1.429  0.15292   
#G_all_plot               0.007723   0.023639   0.327  0.74388   
#I(mixture_plot/100)      5.217113   1.596568   3.268  0.00108 **
#I((mixture_plot/100)^2) -3.830064   1.712861  -2.236  0.02535 *  

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Oak_quad,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Oak), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of bird species (oak specialist)") +
  theme_minimal()

#### BIRDS - Spieces richness of Pine species

glmm_SR_Pine_quad<-glmmTMB(SR_Pine~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
glmm_SR_Pine_simple<-glmmTMB(SR_Pine~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
AICc(glmm_SR_Pine_quad) #178.7159
AICc(glmm_SR_Pine_simple) #178.5433

summary(glmm_SR_Pine_simple)
#Family: poisson  ( log )
#Formula:          SR_Pine ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#177.9     186.6     -84.9     169.9        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 6.589e-10 2.567e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)          2.09344    0.70995   2.949  0.00319 **
#  G_all_plot          -0.05775    0.02926  -1.974  0.04843 * 
#  I(mixture_plot/100) -1.18908    0.43988  -2.703  0.00687 **

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Pine_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Pine), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of bird species (pine specialist)") +
  theme_minimal()

#### BIRDS - SR Generalist 2 (excluding species tolerating mixed stands but avoiding pure stands of one species) 

glmm_SR_Generalist2_quad<-glmmTMB(SR_Generalist2~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
glmm_SR_Generalist2_simple<-glmmTMB(SR_Generalist2~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
AICc(glmm_SR_Generalist2_quad) #287.2593
AICc(glmm_SR_Generalist2_simple) #285.9623

summary(glmm_SR_Generalist2_simple)
#Family: poisson  ( log )
#Formula:          SR_Generalist2 ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#285.3     294.1    -138.7     277.3        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 1.283e-10 1.133e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          1.953495   0.303934   6.427  1.3e-10 ***
#  G_all_plot          -0.007641   0.011831  -0.646   0.5184    
#I(mixture_plot/100)  0.466551   0.182894   2.551   0.0107 *

#### BIRDS - Species richness of oak preferring species tolerating mixed deciduous-conifers 

glmm_SR_Oak_Mixed_quad<-glmmTMB(SR_Oak_Mixed~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
glmm_SR_Oak_Mixed_simple<-glmmTMB(SR_Oak_Mixed~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
AICc(glmm_SR_Oak_Mixed_quad) #218.682
AICc(glmm_SR_Oak_Mixed_simple) #217.642

summary(glmm_SR_Oak_Mixed_simple)
#Family: poisson  ( log )
#Formula:          SR_Oak_Mixed ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#217.0     225.7    -104.5     209.0        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 2.081e-10 1.443e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)          0.02775    0.50489   0.055   0.9562  
#G_all_plot           0.03819    0.01933   1.976   0.0482 *
#  I(mixture_plot/100) -0.16715    0.30371  -0.550   0.5821
AICc(glmm_SR_Oak_Mixed) #217.642

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Oak_Mixed_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Oak_Mixed), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of bird species (oak-mixed oak-pine)") +
  theme_minimal()

########################   BIRD Abundance ########################################################

#### BIRDS - Abundance all species

glmm_Abdce_all_quad<-glmmTMB(Abdce_all~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
glmm_Abdce_all_simple<-glmmTMB(Abdce_all~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
AICc(glmm_Abdce_all_quad) #388.9731
AICc(glmm_Abdce_all_simple) #388.5486

summary(glmm_Abdce_all_simple)
#Family: poisson  ( log )
#Formula:          Abdce_all ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#387.9     396.7    -189.9     379.9        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 6.601e-09 8.125e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         2.8173088  0.1814950  15.523  < 2e-16 ***
#  G_all_plot          0.0003549  0.0070422   0.050 0.959803    
#I(mixture_plot/100) 0.3808931  0.1092179   3.487 0.000488 ***

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_all_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_all), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Abundance of bird species (all)") +
  theme_minimal()

#### BIRDS - Abundance generalist species

glmm_Abdce_Generalist1_quad<-glmmTMB(Abdce_Generalist1~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
glmm_Abdce_Generalist1_simple<-glmmTMB(Abdce_Generalist1~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
AICc(glmm_Abdce_Generalist1_quad) #368.6477
AICc(glmm_Abdce_Generalist1_simple) #367.3275

summary(glmm_Abdce_Generalist1_simple)
#Family: poisson  ( log )
#Formula:          Abdce_Generalist1 ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#366.7     375.4    -179.3     358.7        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 1.837e-10 1.356e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         2.406845   0.203328  11.837   <2e-16 ***
#  G_all_plot          0.008677   0.007860   1.104   0.2696    
#I(mixture_plot/100) 0.312445   0.122370   2.553   0.0107 * 

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Generalist1_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_Generalist1), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Abundance of bird species (generalist)") +
  theme_minimal()

#### BIRDS - Abundance Generalist 2 (excluding species tolerating mixed stands but avoiding pure stands of one species)

glmm_Abdce_Generalist2_quad<-glmmTMB(Abdce_Generalist2~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
glmm_Abdce_Generalist2_simple<-glmmTMB(Abdce_Generalist2~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
AICc(glmm_Abdce_Generalist2_quad) #346.2066
AICc(glmm_Abdce_Generalist2_simple) #344.522

summary(glmm_Abdce_Generalist2_simple)
#Family: poisson  ( log )
#Formula:          Abdce_Generalist2 ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#343.9     352.6    -167.9     335.9        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 9.243e-11 9.614e-06
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         2.297652   0.229803   9.998  < 2e-16 ***
#  G_all_plot          0.001463   0.008904   0.164  0.86947    
#I(mixture_plot/100) 0.427939   0.138390   3.092  0.00199 ** 

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Generalist2_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_Generalist2), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of bird species (generalist)") +
  theme_minimal()

#### BIRDS - Abundance Oak_Mixed (preferring oak but tolerating mixed deciduous-conifer stands) with quadratic effect of tree mixture

glmm_Abdce_Oak_Mixed_quad<-glmmTMB(Abdce_Oak_Mixed~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
glmm_Abdce_Oak_Mixed_simple<-glmmTMB(Abdce_Oak_Mixed~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
AICc(glmm_Abdce_Oak_Mixed_quad) #248.8811
AICc(glmm_Abdce_Oak_Mixed_simple) #247.9966

summary(glmm_Abdce_Oak_0_1_Mixed_simple)
# Family: poisson  ( log )
#Formula:          Abdce_Oak_Mixed ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#247.3     256.1    -119.7     239.3        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 6.326e-10 2.515e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)          0.40148    0.44726   0.898   0.3694  
#G_all_plot           0.03232    0.01717   1.883   0.0598 .
#I(mixture_plot/100) -0.12027    0.26909  -0.447   0.6549   

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Oak_0_1_Mixed_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_Oak_Mixed), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Abundance of bird species (oak-mixed stands)") +
  theme_minimal()

#### BIRDS - Abundance of Oak specialist species

glmm_Abdce_Oak_quad<-glmmTMB(Abdce_Oak~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
glmm_Abdce_Oak_simple<-glmmTMB(Abdce_Oak~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
AICc(glmm_Abdce_Oak_quad) #236.4866
AICc(glmm_Abdce_Oak_simple) #238.2129

summary(glmm_Abdce_Oak_0_1_quad)
# Family: poisson  ( log )
#Formula:          Abdce_Oak ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#235.5     246.4    -112.7     225.5        61 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.06097  0.2469  
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)             -0.551236   0.711940  -0.774  0.43877   
#G_all_plot              -0.001504   0.025077  -0.060  0.95217   
#I(mixture_plot/100)      4.948971   1.509098   3.279  0.00104 **
#I((mixture_plot/100)^2) -3.249982   1.600920  -2.030  0.04235 *  


# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Oak_quad,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Oak), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of bird individuals (oak specialist)") +
  theme_minimal()

#### BIRDS - Abundance Pine specialist species

glmm_Abdce_Pine_quad<-glmmTMB(Abdce_Pine~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
glmm_Abdce_Pine_simple<-glmmTMB(Abdce_Pine~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
AICc(glmm_Abdce_Pine_quad) #202.5964
AICc(glmm_Abdce_Pine_simple) #202.2014

summary(glmm_Abdce_Pine_simple)
#Family: poisson  ( log )
#Formula:          Abdce_Pine ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#201.5     210.3     -96.8     193.5        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 4.234e-10 2.058e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          2.81399    0.63292   4.446 8.75e-06 ***
#  G_all_plot          -0.07651    0.02642  -2.895  0.00379 ** 
#  I(mixture_plot/100) -1.28705    0.39223  -3.281  0.00103 **

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Pine_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_Pine), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of bird individuals (pine specialist)") +
  theme_minimal()


#####################################################
##############  BIRDS -  OMI      ###################
#####################################################

dudi1 <- dudi.pca(Bird.Rel.Env.Sp[,c("mixture_plot","G_all_plot")], scale = TRUE, scan = FALSE, nf = 2)
scatter(dudi1)

nic1 <- niche(dudi1,Y=Bird.Rel.Env.Sp[,c(7:45)], scann = FALSE)
nic1
summary(nic1)
plot(nic1)

kable(niche.param(nic1))
rtest(nic1,100)
#Adjustment method for multiple comparisons:   none 
#Permutation number:   100 
#Test         Obs       Std.Obs   Alter     Pvalue
#1    AEGCAU 0.637696153 -0.2739817987 greater 0.56435644
#2    ANTTRI 0.008040181 -0.8272649199 greater 0.91089109
#3    CARCAR 2.552771210  2.9792525330 greater 0.01980198*
#4    CERBRA 0.264912868 11.2971145511 greater 0.00990099**
#5    COCCOC 0.055180165 -0.8069229562 greater 0.74257426
#6    COLOEN 0.631846751 -0.8631989250 greater 0.80198020
#7    COLPAL 0.141693209  1.7195420743 greater 0.03960396*
#8    CORCOR 1.722790665  3.4252934607 greater 0.01980198*
#9    CUCCAN 0.196515187 -0.7655221388 greater 0.82178218
#10   DENMAJ 0.090844679  2.1011544107 greater 0.05940594
#11   DENMED 0.178736777  0.2504010550 greater 0.32673267
#12   DENMIN 5.144949187  1.7750400171 greater 0.06930693
#13   ERIRUB 0.006038086 -0.7766926903 greater 0.78217822
#14   FICHYP 2.714431124  0.5131125358 greater 0.26732673
#15   FRICOE 0.015248938  5.3601589306 greater 0.00990099**
#16   GARGLA 0.350837214  0.0136017756 greater 0.34653465
#17   ORIORI 0.235617346 -0.4091593667 greater 0.56435644
#18   PARATE 0.957949909  2.8644783190 greater 0.04950495*
#19   PARCAE 0.519575426 12.0556050229 greater 0.00990099**
#20   PARCRI 0.243219392  5.6585674882 greater 0.00990099**
#21   PARMAJ 0.016626519 -0.0002363138 greater 0.39603960
#22   PARPAL 0.009498599 -0.8776923005 greater 0.82178218
#23   PHOPHO 0.544285570  6.2296951966 greater 0.00990099**
#24   PHYBON 0.019518635 -0.1748246687 greater 0.46534653
#25   PHYCOL 0.033135130  1.6958629456 greater 0.06930693
#26   PHYSIB 0.041748500  0.8147609452 greater 0.19801980
#27   PHYTRO 0.098690346 -0.4441502094 greater 0.61386139
#28   PICVIR 0.233063904 -0.8076129258 greater 0.88118812
#29   PRUMOD 0.569421391  1.3948255704 greater 0.11881188
#30   REGIGN 0.126042886 -0.2675153489 greater 0.43564356
#31   REGREG 0.111673925  0.9379357566 greater 0.14851485
#32   SITEUR 0.307963762  7.7604837282 greater 0.00990099**
#33   STRTUR 0.001179621 -0.9765908530 greater 1.00000000
#34   STUVUL 0.157632265 -0.2554258801 greater 0.45544554
#35   SYLATR 0.056172873  1.8552808394 greater 0.05940594
#36   TROTRO 0.082230938  3.3647503773 greater 0.01980198*
#37   TURMER 0.325751790  0.2853560909 greater 0.27722772
#38   TURPHI 0.553990098  0.5211143849 greater 0.21782178
#39   TURVIS 0.091732867 -0.4175554582 greater 0.58415842
#40 OMI.mean 0.514083438  2.0381996910 greater 0.02970297*


# Creation of the factor
fact <- Bird.Rel.Env.Sp$cat_mixture_plot
# plotting the two subsets
s.class(nic1$ls, fact, col=c("red", "blue","black"),cellipse=0, cpoint=2, pch=3)
s.chull(nic1$ls, fact, col=c("red", "blue","black"),optchull = 1, add.plot = T)



###########################################################
##############      BIRDS - PCA      #####################
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
##############      BIRDS - RDA      ######################
###########################################################

Bird.rda <- rda(Bird.Rel.Env.Sp[,c(8:46)])
biplot(Bird.rda,display = c("sites","species"),type = c("text","points"))
#ordihull(Bird.rda,group = Bird.Rel.Env.Sp$cat_mel_plot,col = c(1,2,3),label=TRUE)
ordiellipse(Bird.rda,group = Bird.Rel.Env.Sp$cat_mel_plot,col = c(1,2,3),label=TRUE)

##############################################################
##############  BIRDS - RLQ analysis   #######################
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

################################################################
####################    BIRDS -  IndVal          ###############
################################################################

beta_bird<-beta.div(Bird.Rel.Env.Sp[,c(7:45)])
plot(Bird.Rel.Env.Sp$MEL_point,beta_bird$LCBD)
cor.test(Bird.Rel.Env.Sp$MEL_point,beta_bird$LCBD)

indval_Bird <- multipatt(Bird.Rel.Env.Sp[,c(7:45)], Bird.Rel.Env.Sp$cat_mixture_plot,control = how(nperm=999)) 
summary(indval_Bird)
# Multilevel pattern analysis
#---------------------------
#
#  Association function: IndVal.g
#Significance level (alpha): 0.05
#Total number of species: 39
#Selected number of species: 6 
#Number of species associated to 1 group: 2 
#Number of species associated to 2 groups: 4 
#List of species associated to each combination: 
#  Group oak  #sps.  2 
#stat p.value    
#PHOPHO 0.890   0.001 ***
#CARCAR 0.558   0.024 *  
#  Group pine+mixed  #sps.  1 
#stat p.value  
#TROTRO 0.873   0.018 *
#  Group mixed+oak  #sps.  3 
#stat p.value    
#CERBRA 0.865   0.001 ***
#SITEUR 0.855   0.001 ***
#PARCAE 0.847   0.001 ***
 


#############################################################
#####   BIRDS - Beta partitioning (Baselga 2017 MEE)  #######
#############################################################

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

###############################################################################################
###############################################################################################
###########################     SAPROX BEETLES    ###########################################
##############################################################################################
##############################################################################################

Saprox.Rel.Env.Sp<-read.csv("Rel_Env_Sp_Saprox_2026.csv", sep=";", header=T)

#Reordering tree mixture categories along a gradient of increasing oak (deciduous) basal area 
Saprox.Rel.Env.Sp$cat_mixture_plot<- factor(Saprox.Rel.Env.Sp$cat_mixture_plot, levels = c("pine", "mixed", "oak"))

############################################################################################
###########################     SAPROX BEETLES - GLMMM    ##################################
############################################################################################

######## SAPROX BEETLES - Species richness all species

descdist(Saprox.Rel.Env$SR_all,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env$SR_all,"pois"))
fitnb<-fitdist(Saprox.Rel.Env$SR_all,"nbinom")
fitp<-fitdist(Saprox.Rel.Env$SR_all,"pois")
gofstat(fitnb)$chisqpvalue #0.5436261
gofstat(fitp)$chisqpvalue #0.7831754 --> Poisson

glmm_SR_all_quad<-glmmTMB(SR_all~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
glmm_SR_all_simple<-glmmTMB(SR_all~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
AICc(glmm_SR_all_quad) #331.1574
AICc(glmm_SR_all)#331.0967

summary(glmm_SR_all_simple) 
#Family: poisson  ( log )
#Formula:          SR_all ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Saprox.Rel.Env
#AIC       BIC    logLik -2*log(L)  df.resid 
#330.2     337.7    -161.1     322.2        44 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.00632  0.0795  
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          3.912350   0.097243   40.23  < 2e-16 ***
#  G_all_plot          -0.010025   0.003388   -2.96  0.00309 ** 
#  I(mixture_plot/100) -0.036635   0.080721   -0.45  0.64994    

sim<-simulateResiduals(glmm_SR_all_simple)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value 0.77 # Dispersion test 0.608  # Outliers p=1 
testOutliers(sim) #p=1

# Create prediction grid
mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_all_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = SR_all), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle individuals (all)") +
  theme_minimal()

######## SAPROX BEETLES - Species richness conifer species (categories 0 and 1)

descdist(Saprox.Rel.Env.Sp$SR.pine,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env.Sp$SR.pine,"pois"))
fitnb<-fitdist(Saprox.Rel.Env.Sp$SR.pine,"nbinom")
fitp<-fitdist(Saprox.Rel.Env.Sp$SR.pine,"pois")
gofstat(fitnb)$chisqpvalue #0.00205022 --> Poisson
gofstat(fitp)$chisqpvalue #1.239995e-28 

glmm_SR_Pine_quad<-glmmTMB(SR_Pine~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
glmm_SR_Pine_simple<-glmmTMB(SR_Pine~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
AICc(glmm_SR_Pine_quad) #226.6784
AICc(glmm_SR_Pine) #228.4148

summary(glmm_SR_Pine_quad) 
# Family: poisson  ( log )
#Formula:          SR_Pine ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#225.2     234.6    -107.6     215.2        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.1793   0.4234  
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              2.50153    0.34959   7.156 8.32e-13 ***
#  G_all_plot              -0.02347    0.01200  -1.956   0.0505 .  
#I(mixture_plot/100)      0.39370    0.99196   0.397   0.6914    
#I((mixture_plot/100)^2) -2.14845    1.04337  -2.059   0.0395 *  

sim<-simulateResiduals(glmm_SR_Pine_quad)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value p=.47785 #Dispersion test p=0.872 # Outliers p=0.34
testOutliers(sim) # 1 outlier (lowest residual)

# Create prediction grid
mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Pine_quad,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Pine), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle species (pine)") +
  theme_minimal()

######## SAPROX BEETLES - Species richness deciduous species (categories 0 and 1)

descdist(Saprox.Rel.Env.Sp$SR_Oak,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env.Sp$SR_Oak,"pois"))
fitnb<-fitdist(Saprox.Rel.Env.Sp$SR_Oak,"nbinom")
fitp<-fitdist(Saprox.Rel.Env.Sp$SR_Oak,"pois")
gofstat(fitnb)$chisqpvalue #0.4257678 --> Negbin
gofstat(fitp)$chisqpvalue #0.1261187 

glmm_SR_Oak_quad<-glmmTMB(SR_Oak~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
glmm_SR_Oak_simple<-glmmTMB(SR_Oak~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
AICc(glmm_SR_Oak_quad) #309.8831
AICc(glmm_SR_Oak) #309.2632

summary(glmm_SR_Oak_simple) 
#  Family: poisson  ( log )
#Formula:          SR_Oak ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#308.3     315.8    -150.2     300.3        44 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 2.427e-09 4.926e-05
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          3.536422   0.097006   36.46   <2e-16 ***
#  G_all_plot          -0.009093   0.003389   -2.68   0.0073 ** 
#  I(mixture_plot/100)  0.185845   0.074504    2.49   0.0126 *       

sim<-simulateResiduals(glmm_SR_Oak_simple)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value 0.8388 # Dispersion test 0.704 # Outliers 1 
testOutliers(sim) # RAS

# Create prediction grid
mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Oak,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Oak), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle species (oak)") +
  theme_minimal()

######## SAPROX BEETLES - Species richness generalist species (category 2)

descdist(Saprox.Rel.Env.Sp$SR.Generalist,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env.Sp$SR.Generalist,"pois"))
fitnb<-fitdist(Saprox.Rel.Env.Sp$SR.Generalist,"nbinom")
fitp<-fitdist(Saprox.Rel.Env.Sp$SR.Generalist,"pois")
gofstat(fitnb)$chisqpvalue #NULL
gofstat(fitp)$chisqpvalue #0.1310683 

glmm_SR_Generalist_quad<-glmmTMB(SR_Generalist~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
glmm_SR_Generalist_simple<-glmmTMB(SR_Generalist~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
AICc(glmm_SR_Generalist_quad)#143.8809
AICc(glmm_SR_Generalist_simple)#143.864

summary(glmm_SR_Generalist_simple) 
# Family: poisson  ( log )
#Formula:          SR_Generalist ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#142.9     150.4     -67.5     134.9        44 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 4.948e-10 2.225e-05
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)          0.78285    0.41579   1.883   0.0597 .
#G_all_plot          -0.01343    0.01471  -0.913   0.3613  
#I(mixture_plot/100)  0.08201    0.31960   0.257   0.7975

sim<-simulateResiduals(glmm_SR_Generalist_simple)
testUniformity(sim)
#KS Test p-value 0.16553 # Dispersion test 0.032 # Outliers 1 
testOutliers(sim) # NTD

# Create prediction grid
mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Generalist,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Generalist), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle species (oak)") +
  theme_minimal()

######## SAPROX BEETLES - Species richness oak specialist species (category 0)

glmm_SR_Oak_0_quad<-glmmTMB(SR_Oak_0~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
glmm_SR_Oak_0_simple<-glmmTMB(SR_Oak_0~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
AICc(glmm_SR_Oak_0_quad) #285.2093
AICc(glmm_SR_Oak_0_simple)#285.2304

summary(glmm_SR_Oak_0_quad)
#Family: poisson  ( log )
#Formula:          SR_Oak_0 ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#283.8     293.1    -136.9     273.8        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 1.847e-10 1.359e-05
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              2.955613   0.131001  22.562   <2e-16 ***
#  G_all_plot              -0.010103   0.004274  -2.364   0.0181 *  
#  I(mixture_plot/100)      0.850392   0.359099   2.368   0.0179 *  
#  I((mixture_plot/100)^2) -0.536143   0.338091  -1.586   0.1128 

mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Oak_0_quad,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Oak_0), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle species (oak)") +
  theme_minimal()

######## SAPROX BEETLES - Species richness oak specialist tolerant to mixing (category 1)

glmm_SR_Oak_Mixed_1_quad<-glmmTMB(SR_Oak_Mixed_1~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
glmm_SR_Oak_Mixed_1_simple<-glmmTMB(SR_Oak_Mixed_1~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
AICc(glmm_SR_Oak_Mixed_1_quad) #235.7415
AICc(glmm_SR_Oak_Mixed_1_simple)#233.3106

summary(glmm_SR_Oak_Mixed_1_simple)
#Family: poisson  ( log )
#Formula:          SR_Oak_Mixed_1 ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#232.4     239.9    -112.2     224.4        44 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 6.746e-11 8.214e-06
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          2.621188   0.157883   16.60   <2e-16 ***
#  G_all_plot          -0.007776   0.005556   -1.40    0.162    
#I(mixture_plot/100) -0.007245   0.121539   -0.06    0.952 

mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Oak_Mixed_1_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Oak_Mixed_1), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle species (oak tolerant to pine)") +
  theme_minimal()

######## SAPROX BEETLES - Species richness pine specialist (category 0)

glmm_SR_Pine_0_quad<-glmmTMB(SR_Pine_0~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
glmm_SR_Pine_0_simple<-glmmTMB(SR_Pine_0~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
AICc(glmm_SR_Pine_0_quad) #204.0788
AICc(glmm_SR_Pine_0_simple)#208.7299

summary(glmm_SR_Pine_0_quad)
#Family: poisson  ( log )
#Formula:          SR_Pine_0 ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#202.7     212.0     -96.3     192.7        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.1515   0.3893  
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              2.21026    0.36981   5.977 2.28e-09 ***
#  G_all_plot              -0.02160    0.01293  -1.671   0.0947 .  
#I(mixture_plot/100)      0.99040    1.04051   0.952   0.3412    
#I((mixture_plot/100)^2) -2.98479    1.13075  -2.640   0.0083 ** 

mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Pine_0_quad,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Pine_0), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle species (pine)") +
  theme_minimal()

######## SAPROX BEETLES - Species richness pine specialist tolerant to mixing (category 1)

glmm_SR_Pine_Mixed_1_quad<-glmmTMB(SR_Pine_Mixed_1~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
glmm_SR_Pine_Mixed_1_simple<-glmmTMB(SR_Pine_Mixed_1~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
AICc(glmm_SR_Pine_Mixed_1_quad) #130.5391
AICc(glmm_SR_Pine_Mixed_1_simple)#128.08

summary(glmm_SR_Pine_Mixed_1_simple)
#Family: poisson  ( log )
#Formula:          SR_Pine_Mixed_1 ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#127.1     134.6     -59.6     119.1        44 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.01922  0.1386  
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)          0.97115    0.53333   1.821  0.06862 . 
#G_all_plot          -0.01788    0.02030  -0.881  0.37845   
#I(mixture_plot/100) -1.21856    0.43973  -2.771  0.00559 **

mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Pine_Mixed_1_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Pine_Mixed_1), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle species (pine tolerant to oak)") +
  theme_minimal()

######## SAPROX BEETLES - Species richness generalist species including true generalist and species tolerating mixing (categories 1-2-3)

Saprox.Rel.Env.Sp$SR_Generalist_cat0to3<-Saprox.Rel.Env.Sp$SR_Generalist+Saprox.Rel.Env.Sp$SR_Oak_Mixed_1+Saprox.Rel.Env.Sp$SR_Pine_Mixed_1

descdist(Saprox.Rel.Env.Sp$SR_Generalist_cat0to3,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env.Sp$SR_Generalist_cat0to3,"pois"))
fitnb<-fitdist(Saprox.Rel.Env.Sp$SR_Generalist_cat0to3,"nbinom")
fitp<-fitdist(Saprox.Rel.Env.Sp$SR_Generalist_cat0to3,"pois")
gofstat(fitnb)$chisqpvalue #NULL
gofstat(fitp)$chisqpvalue #0.1310683 

glmm_SR_Generalist_quad<-glmmTMB(SR_Generalist_cat0to3~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
glmm_SR_Generalist_simple<-glmmTMB(SR_Generalist_cat0to3~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
AICc(glmm_SR_Generalist_quad)#253.3386
AICc(glmm_SR_Generalist_simple)#251.2019

summary(glmm_SR_Generalist_simple)
#Family: poisson  ( log )
#Formula:          SR_Generalist_cat0to3 ~ G_all_plot + I(mixture_plot/100) + (1 |      stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#250.3     257.8    -121.1     242.3        44 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev.
#stand  (Intercept) 1.144e-10 1.07e-05
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          2.902242   0.141909  20.451   <2e-16 ***
#  G_all_plot          -0.009099   0.005023  -1.812   0.0701 .  
#I(mixture_plot/100) -0.081993   0.109400  -0.749   0.4536 

mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Generalist_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Generalist_cat0to3), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle species (generalists)") +
  theme_minimal()


##############################################################################################
###########################     SAPROX Total abundance    ##################################
##############################################################################################

######## SAPROX BEETLES - Abundance all species

descdist(Saprox.Rel.Env$Abdce_all,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env$Abdce_all,"pois"))
fitnb<-fitdist(Saprox.Rel.Env$Abdce_all,"nbinom")
fitp<-fitdist(Saprox.Rel.Env$Abdce_all,"pois")
gofstat(fitnb)$chisqpvalue #0.1367534 --> NegBin
gofstat(fitp)$chisqpvalue #0

glmm_Abdce_all_quad<-glmmTMB(Abdce_all~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
glmm_Abdce_all_simple<-glmmTMB(Abdce_all~G_all_plot+I(mixture_plot/100)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
AICc(glmm_Abdce_all_quad)#556.3937
AICc(glmm_Abdce_all_simple)#554.598

summary(glmm_Abdce_all) 
#Family: nbinom1  ( log )
#Formula:          Abdce_all ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#553.2     562.5    -271.6     543.2        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev.
#stand  (Intercept) 0.0001134 0.01065 
#Number of obs: 48, groups:  stand, 21
#Dispersion parameter for nbinom1 family (): 31.3 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          5.398479   0.230187  23.453   <2e-16 ***
#  G_all_plot          -0.011145   0.007865  -1.417    0.156    
#I(mixture_plot/100)  0.038670   0.176996   0.218    0.827    

sim<-simulateResiduals(glmm_Abdce_all_simple)
testUniformity(sim)
#KS Test p-value 0.99838 # Dispersion test 0.504  # Outliers p=1 
testOutliers(sim) #p=1

# Create prediction grid
mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_all_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_all), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle individuals (all)") +
  theme_minimal()

#####################################
########Abundance conifer (pine) specialists
################################

descdist(Saprox.Rel.Env.Sp$Abdce_Pine,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env.Sp$Abdce_Pine,"pois"))
fitnb<-fitdist(Saprox.Rel.Env.Sp$Abdce_Pine,"nbinom")
fitp<-fitdist(Saprox.Rel.Env.Sp$Abdce_Pine,"pois")
gofstat(fitnb)$chisqpvalue #0.007874077
gofstat(fitp)$chisqpvalue #0 

########Abundance pine species with quadratic effect of tree mixture

glmm_Abdce_Pine<-glmmTMB(Abdce_Pine~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
summary(glmm_Abdce_Pine) 
#  Family: nbinom1  ( log )
#Formula:          Abdce_Pine ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#354.2     365.5    -171.1     342.2        42 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.1949   0.4415  
#Number of obs: 48, groups:  stand, 21
#Dispersion parameter for nbinom1 family (): 12.2 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              3.729633   0.469658   7.941    2e-15 ***
#  G_all_plot              -0.004018   0.017019  -0.236    0.813    
#I(mixture_plot/100)     -1.167241   1.331762  -0.876    0.381    
#I((mixture_plot/100)^2) -1.299835   1.428250  -0.910    0.363    
AICc(glmm_Abdce_Pine) #356.2953

sim<-simulateResiduals(glmm_Abdce_Pine)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value p=.70952 #Dispersion test p=0.408 # Outliers p=1
testOutliers(sim) # p=1, RAS

########Abundance pine specialist species with simple effect of tree mixture

glmm_Abdce_Pine<-glmmTMB(Abdce_Pine~G_all_plot+I(mixture_plot/100)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
summary(glmm_Abdce_Pine) 
#  Family: nbinom1  ( log )
#Formula:          Abdce_Pine ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#353.1     362.4    -171.5     343.1        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.1983   0.4453  
#Number of obs: 48, groups:  stand, 21
#Dispersion parameter for nbinom1 family (): 12.4 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          3.806766   0.464054   8.203 2.34e-16 ***
#  G_all_plot          -0.002715   0.016995  -0.160    0.873    
#I(mixture_plot/100) -2.316756   0.442752  -5.233 1.67e-07 ***  
AICc(glmm_Abdce_Pine) #354.5107

# Create prediction grid
mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Pine,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_Pine), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle individuals (pine)") +
  theme_minimal()


#####################################
########Abdce deciduous
################################

descdist(Saprox.Rel.Env.Sp$Abdce_Oak_0_1,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env.Sp$Abdce_Oak_0_1,"pois"))
fitnb<-fitdist(Saprox.Rel.Env.Sp$Abdce_Oak_0_1,"nbinom")
fitp<-fitdist(Saprox.Rel.Env.Sp$Abdce_Oak_0_1,"pois")
gofstat(fitnb)$chisqpvalue #0.4542295 --> Negbin
gofstat(fitp)$chisqpvalue #0 

########Abundance oak specialist species with quadratic effect of tree mixture

glmm_Abdce_Oak_0_1<-glmmTMB(Abdce_Oak_0_1~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
summary(glmm_Abdce_Oak_0_1) 
#  Family: nbinom1  ( log )
#Formula:          Abdce_Oak_0_1 ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#536.7     547.9    -262.3     524.7        42 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 2.765e-09 5.258e-05
#Number of obs: 48, groups:  stand, 21
#Dispersion parameter for nbinom1 family (): 28.6 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              4.909056   0.263317  18.643   <2e-16 ***
#  G_all_plot              -0.013168   0.008325  -1.582   0.1137    
#I(mixture_plot/100)      1.207976   0.706096   1.711   0.0871 .  
#I((mixture_plot/100)^2) -0.828080   0.651307  -1.271   0.2036     
AICc(glmm_Abdce_Oak_0_1) #538.7397

sim<-simulateResiduals(glmm_Abdce_Oak_0_1)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value 0.9683 # Dispersion test 0.352 # Outliers p=0.78 
testOutliers(sim) # p=0.82, one outliying value (highest residual)

########Species richness oak specialist species with simple effect of tree mixture

glmm_Abdce_Oak_0_1<-glmmTMB(Abdce_Oak_0_1~G_all_plot+I(mixture_plot/100)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
summary(glmm_Abdce_Oak_0_1) 
#   Family: nbinom1  ( log )
#Formula:          Abdce_Oak_0_1 ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#536.3     545.6    -263.1     526.3        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 2.267e-08 0.0001506
#Number of obs: 48, groups:  stand, 21
#Dispersion parameter for nbinom1 family (): 29.7 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          5.05865    0.23576  21.457   <2e-16 ***
#G_all_plot          -0.01417    0.00848  -1.671   0.0947 .  
#I(mixture_plot/100)  0.34520    0.18156   1.901   0.0573 .       
AICc(glmm_Abdce_Oak_0_1) #537.7154

sim<-simulateResiduals(glmm_Abdce_Oak_0_1)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value 0.98819 # Dispersion test 0.272 # Outliers p=0.92 
testOutliers(sim) # p=0.82, 1 outlying value (highest residual)

# Create prediction grid
mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Oak_0_1,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_Oak_0_1), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle individuals (oak)") +
  theme_minimal()


#####################################
######## SAPRO - Abundance generalist
################################

descdist(Saprox.Rel.Env.Sp$Abdce_Generalist,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env.Sp$Abdce_Generalist,"pois"))
fitnb<-fitdist(Saprox.Rel.Env.Sp$Abdce_Generalist,"nbinom")
fitp<-fitdist(Saprox.Rel.Env.Sp$Abdce_Generalist,"pois")
gofstat(fitnb)$chisqpvalue #0.4838977
gofstat(fitp)$chisqpvalue #3.05807e-112

########Abundance generalist species with quadratic effect of tree mixture

glmm_Abdce_Generalist<-glmmTMB(Abdce_Generalis~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
summary(glmm_Abdce_Generalist) 

# Family: nbinom1  ( log )
#Formula:          Abdce_Generalis ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#287.7     299.0    -137.9     275.7        42 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.005251 0.07246 
#Number of obs: 48, groups:  stand, 21
#Dispersion parameter for nbinom1 family (): 2.62 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              3.24712    0.34067   9.532   <2e-16 ***
#  G_all_plot              -0.02719    0.01267  -2.147   0.0318 *  
#  I(mixture_plot/100)     -1.26035    0.96588  -1.305   0.1919    
#I((mixture_plot/100)^2)  0.15576    0.97361   0.160   0.8729
AICc(glmm_Abdce_Generalis)#289.7721

sim<-simulateResiduals(glmm_Abdce_Generalist)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value 0.84298 # Dispersion test 0.76 # Outliers 1 
testOutliers(sim) # p=1, RAS

########Abundance generalist species with simple effect of tree mixture

glmm_Abdce_Generalist<-glmmTMB(Abdce_Generalist~G_all_plot+I(mixture_plot/100)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
summary(glmm_Abdce_Generalist) 

# Family: nbinom1  ( log )
#Formula:          Abdce_Generalist ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#285.7     295.1    -137.9     275.7        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.005823 0.07631 
#Number of obs: 48, groups:  stand, 21
#Dispersion parameter for nbinom1 family (): 2.64 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          3.23188    0.32791   9.856  < 2e-16 ***
#  G_all_plot          -0.02721    0.01268  -2.146   0.0319 *  
#  I(mixture_plot/100) -1.11270    0.28465  -3.909 9.26e-05 ***
AICc(glmm_Abdce_Generalis)#287.1774

sim<-simulateResiduals(glmm_Abdce_Generalist)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value 0.58133 # Dispersion test 0.736 # Outliers 1 
testOutliers(sim) # p=1, RAS

# Create prediction grid
mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Generalist,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_Generalist), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle individuals (generalist)") +
  theme_minimal()


######Abundance oak specialist (category 0)

glmm_Abdce_Oak_0_quad<-glmmTMB(Abdce_Oak_0~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
glmm_Abdce_Oak_0_simple<-glmmTMB(Abdce_Oak_0~G_all_plot+I(mixture_plot/100)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
AICc(glmm_Abdce_Oak_0_quad) #488.5852
AICc(glmm_Abdce_Oak_0_simple)#487.699

summary(glmm_Abdce_Oak_0_simple)
#Family: nbinom1  ( log )
#Formula:          Abdce_Oak_0 ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#486.3     495.6    -238.1     476.3        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 7.452e-09 8.633e-05
#Number of obs: 48, groups:  stand, 21
#Dispersion parameter for nbinom1 family (): 18.2 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          4.470539   0.247852  18.037  < 2e-16 ***
#  G_all_plot          -0.016517   0.008621  -1.916  0.05538 .  
#I(mixture_plot/100)  0.536355   0.188268   2.849  0.00439 **  

mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Oak_0_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_Oak_0), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle individuals (oak)") +
  theme_minimal()

######Species richness oak specialist tolerant to mixing (category 1)

glmm_Abdce_Oak_Mixed_1_quad<-glmmTMB(Abdce_Oak_Mixed_1~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
glmm_Abdce_Oak_Mixed_1_simple<-glmmTMB(Abdce_Oak_Mixed_1~G_all_plot+I(mixture_plot/100)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
AICc(glmm_Abdce_Oak_Mixed_1_quad) #483.077
AICc(glmm_Abdce_Oak_Mixed_1_simple)#481.2285

summary(glmm_Abdce_Oak_Mixed_1_simple)
#Family: nbinom1  ( log )
#Formula:          Abdce_Oak_Mixed_1 ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#479.8     489.2    -234.9     469.8        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev. 
#stand  (Intercept) 1.36e-09 3.688e-05
#Number of obs: 48, groups:  stand, 21
#Dispersion parameter for nbinom1 family (): 26.6 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          4.240934   0.327424  12.952   <2e-16 ***
#  G_all_plot          -0.006969   0.011948  -0.583    0.560    
#I(mixture_plot/100) -0.115972   0.250834  -0.462    0.644 

mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Oak_Mixed_1_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_Oak_Mixed_1), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle species (oak tolerant to pine)") +
  theme_minimal()

######Species richness pine specialist (category 0)

glmm_Abdce_Pine_0_quad<-glmmTMB(Abdce_Pine_0~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
glmm_Abdce_Pine_0_simple<-glmmTMB(Abdce_Pine_0~G_all_plot+I(mixture_plot/100)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
AICc(glmm_Abdce_Pine_0_quad) #339.2549
AICc(glmm_Abdce_Pine_0_simple)#338.546

summary(glmm_Abdce_Pine_0_simple)
#Family: nbinom1  ( log )
#Formula:          Abdce_Pine_0 ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#337.1     346.5    -163.6     327.1        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.22     0.4691  
#Number of obs: 48, groups:  stand, 21
#Dispersion parameter for nbinom1 family (): 11.9 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          3.765653   0.489791   7.688 1.49e-14 ***
#  G_all_plot          -0.002605   0.017987  -0.145    0.885    
#I(mixture_plot/100) -2.556390   0.469002  -5.451 5.02e-08 *** 

mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Pine_0_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_Pine_0), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle individuals (pine)") +
  theme_minimal()


######Species richness pine specialist tolerant to mixing (category 1)

glmm_Abdce_Pine_Mixed_1_quad<-glmmTMB(Abdce_Pine_Mixed_1~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
glmm_Abdce_Pine_Mixed_1_simple<-glmmTMB(Abdce_Pine_Mixed_1~G_all_plot+I(mixture_plot/100)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
AICc(glmm_Abdce_Pine_Mixed_1_quad) #180.2738
AICc(glmm_Abdce_Pine_Mixed_1_simple)#177.8549

summary(glmm_Abdce_Pine_Mixed_1_simple)
#Family: nbinom1  ( log )
#Formula:          Abdce_Pine_Mixed_1 ~ G_all_plot + I(mixture_plot/100) + (1 |      stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#176.4     185.8     -83.2     166.4        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.3277   0.5725  
#Number of obs: 48, groups:  stand, 21
#Dispersion parameter for nbinom1 family (): 1.06 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)          1.67272    0.69299   2.414   0.0158 *
#  G_all_plot          -0.03068    0.03281  -0.935   0.3497  
#I(mixture_plot/100) -1.22601    0.57944  -2.116   0.0344 *

mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Pine_Mixed_1_simple,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_Pine_Mixed_1), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle inidviduals (pine tolerant to oak)") +
  theme_minimal()


######## SAPROX BEETLES - Abundance generalist species including true generalist and species tolerating mixing (categories 1-2-3)

Saprox.Rel.Env.Sp$Abdce_Generalist_cat0to3<-Saprox.Rel.Env.Sp$Abdce_Generalist_2+Saprox.Rel.Env.Sp$Abdce_Oak_Mixed_1+Saprox.Rel.Env.Sp$Abdce_Pine_Mixed_1

descdist(Saprox.Rel.Env.Sp$Abdce_Generalist_cat0to3,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env.Sp$Abdce_Generalist_cat0to3,"pois"))
fitnb<-fitdist(Saprox.Rel.Env.Sp$Abdce_Generalist_cat0to3,"nbinom")
fitp<-fitdist(Saprox.Rel.Env.Sp$Abdce_Generalist_cat0to3,"pois")
gofstat(fitnb)$chisqpvalue #0.5074654
gofstat(fitp)$chisqpvalue #0 

glmm_Abdce_Generalist_quad<-glmmTMB(Abdce_Generalist_cat0to3~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
glmm_Abdce_Generalist_simple<-glmmTMB(Abdce_Generalist_cat0to3~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
AICc(glmm_Abdce_Generalist_quad)#1003.085
AICc(glmm_Abdce_Generalist_simple)#1080.442

summary(glmm_Abdce_Generalist_quad)
#Family: poisson  ( log )
#Formula:          Abdce_Generalist_cat0to3 ~ G_all_plot + I(mixture_plot/100) +      I((mixture_plot/100)^2) + (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#1001.7    1011.0    -495.8     991.7        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.8607   0.9278  
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              4.151737   0.240198  17.285  < 2e-16 ***
#  G_all_plot              -0.020341   0.003648  -5.577 2.45e-08 ***
#  I(mixture_plot/100)     -1.751513   0.445986  -3.927 8.59e-05 ***
#  I((mixture_plot/100)^2)  3.514453   0.399140   8.805  < 2e-16 *** 

mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Generalist_quad,
  newdata = data.frame(G_all_plot=G_all_seq,mixture_plot = mixture_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mixture_plot = mixture_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mixture_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Saprox.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_Generalist_cat0to3), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of saproxylic beetle species (generalists)") +
  theme_minimal()























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
##############  SAPROX - OMI    #####################
#####################################################

dudi1 <- dudi.pca(Saprox.Rel.Env.Sp[,c("mixture_plot","G_all_plot")], scale = TRUE, scan = FALSE, nf = 2)
scatter(dudi1)

nic1 <- niche(dudi1,Y=Saprox.Rel.Env.Sp[,c(11:213)], scann = FALSE)
nic1
summary(nic1)
plot(nic1)

kable(niche.param(nic1))
rtest(nic1,100)
#class: krandtest lightkrandtest 
#Monte-Carlo tests
#Call: as.krandtest(sim = t(sim), obs = obs)
#Number of tests:   204 
#Adjustment method for multiple comparisons:   none 
#Permutation number:   100 
#Test         Obs       Std.Obs   Alter     Pvalue
#1                Abdera.bifasciata 1.904494601  2.9139074211 greater 0.02970297*
#2            Abdera.quadrifasciata 3.041828929  0.4496543656 greater 0.25742574
#3             Acanthocinus.aedilis 1.335356685 -0.3298452024 greater 0.55445545
#4                   Agathidium.sp. 1.335356685 -0.4666253786 greater 0.63366337
#5               Agrilus.angustulus 1.179798462  0.2779944197 greater 0.34653465
#6            Alosterna.tabacicolor 4.568265363  1.6388181642 greater 0.04950495*
#7                Ampedus.balteatus 0.209899512  5.4764166170 greater 0.00990099**
#8             Ampedus.cinnaberinus 0.294029020 -0.6450611186 greater 0.67326733
#9                 Ampedus.glycerus 0.306997989  5.4592736345 greater 0.00990099**
#10              Ampedus.nigerrimus 0.023355791 -0.6862392987 greater 0.67326733
#11              Ampedus.quercicola 0.151622217  1.8480739611 greater 0.07920792
#12              Ampedus.sanguineus 0.130322750 -0.7942995617 greater 0.80198020
#13          Ampedus.sanguinolentus 0.295932658  0.6480092946 greater 0.20792079
#14                Anaspis.fasciata 0.390532651  0.0535637516 greater 0.39603960
#15                   Anaspis.flava 1.825222807 -0.1506622058 greater 0.51485149
#16                Anaspis.garneysi 0.135917918 -0.7759581359 greater 0.73267327
#17                Anaspis.melanopa 0.020545977 -0.9889910504 greater 0.84158416
#18               Anaspis.thoracica 0.196758753 -0.8889454552 greater 0.82178218
#19               Anidorus.nigrinus 1.250585512 -0.5538021464 greater 0.66336634
#20             Anisotoma.humeralis 0.264936815 -0.5845718783 greater 0.63366337
#21             Anthribus.nebulosus 0.477894624 -0.0154317518 greater 0.41584158
#22            Aplocnemus.impressus 0.144422889  0.3314674252 greater 0.23762376
#23              Arhopalus.rusticus 1.908010190  0.8923355943 greater 0.16831683
#24        Aspidiphorus.orbiculatus 0.239842069 -0.1991851991 greater 0.50495050
#25      Aulonothroscus.brevicollis 1.736882016 -0.0901318534 greater 0.47524752
#26                  Bitoma.crenata 0.038780662 -1.0166231124 greater 0.96039604
#27           Calambus.bipustulatus 0.583048993  0.6939112334 greater 0.17821782
#28                 Carpophilus.sp. 0.644892024 -0.3484359258 greater 0.50495050
#29               Cartodere.nodifer 0.006827727 -1.2676122480 greater 0.99009901
#30               Cerambyx.scopolii 0.110136009  0.4356458945 greater 0.24752475
#31              Cerylon.deplanatum 2.551924996  0.5531742432 greater 0.31683168
#32             Cerylon.ferrugineum 0.125934459 -0.0538242127 greater 0.44554455
#33             Cerylon.histeroides 0.206356457 -0.7556740182 greater 0.74257426
#34                  Cetonia.aurata 0.449494277  6.3694374844 greater 0.00990099**
#35          Chrysanthia.geniculata 0.178126841 -0.8082339101 greater 0.79207921
#36                   Cis.castaneus 1.825222807 -0.0013565463 greater 0.52475248
#37                      Cis.micans 0.405032386  2.7588180662 greater 0.03960396*
#38                   Cis.rugulosus 1.577694176  0.8178637515 greater 0.21782178
#39              Clerus.mutillarius 0.645869075  2.1021746729 greater 0.05940594
#40                  Clytus.arietis 0.020287289 -0.8745378281 greater 0.97029703
#41              Colydium.elongatum 0.561633783 -0.2096020327 greater 0.50495050
#42           Cordylepherus.viridis 2.551924996  0.3354943822 greater 0.38613861
#43                  Corticaria.sp. 0.105040816  3.1801830403 greater 0.02970297*
#44              Corticeus.unicolor 1.647396343  0.8171313713 greater 0.16831683
#45             Cortinicara.gibbosa 1.119118561  3.8015160430 greater 0.00990099**
#46             Cortodera.humeralis 0.391274352  3.1216398990 greater 0.02970297*
#47             Cryphalus.asperatus 0.448041272 -0.6204705571 greater 0.63366337
#48             Cryptarcha.strigata 0.249824443  2.2046440617 greater 0.06930693
#49               Cryptarcha.undata 1.867484082  4.1996890324 greater 0.00990099**
#50         Cryptolestes.duplicatus 0.613286384  0.5958534929 greater 0.22772277
#51             Crypturgus.cinereus 0.862452502 -0.2211641469 greater 0.43564356
#52             Crypturgus.pusillus 0.964711313 -0.8059941105 greater 0.77227723
#53         Cyclorhipidion.bodoanus 0.747050945  1.5512020411 greater 0.09900990
#54               Dacne.bipustulata 0.336867924  3.4929238223 greater 0.00990099**
#55               Dasytes.caeruleus 0.155646932 -0.5340924539 greater 0.63366337
#56             Dendroctonus.micans 0.656698219 -0.9272046842 greater 0.76237624
#57            Denticollis.linearis 1.736882016 -0.2312777501 greater 0.56435644
#58             Dermestes.lardarius 0.132072223 -0.8738210898 greater 0.83168317
#59                 Diaperis.boleti 0.612685498 -0.4892264148 greater 0.59405941
#60        Dissoleucas.niveirostris 2.266209191  0.0945889220 greater 0.42574257
#61                   Dorcatoma.sp. 0.351056248 -0.6064722989 greater 0.70297030
#62            Dromaeolus.barnabita 3.526772486  1.1555578571 greater 0.13861386
#63             Enedreytes.sepicola 0.013671839 -1.2415642045 greater 0.97029703
#64             Enicmus.brevicornis 0.440356241 -0.3388057777 greater 0.51485149
#65                 Enicmus.histrio 1.244314569 -0.2759139020 greater 0.57425743
#66                 Enicmus.rugosus 0.090811157  0.4051234468 greater 0.30693069
#67               Enicmus.testaceus 0.009148960 -1.1968086539 greater 0.96039604
#68             Enicmus.transversus 2.154043919  0.2662374417 greater 0.39603960
#69                     Epuraea.sp. 0.097260417 -1.0014705004 greater 0.85148515
#70                    Ernobius.sp. 0.063416790 -0.9979652004 greater 0.97029703
#71               Euglenes.oculatus 1.250585512 -0.4377056749 greater 0.58415842
#72             Eulagius.filicornis 0.254507236 -0.3274883229 greater 0.54455446
#73         Gastrallus.immarginatus 0.488828009  0.8057957817 greater 0.22772277
#74        Glischrochilus.hortensis 0.286770233  0.0001921276 greater 0.40594059
#75   Glischrochilus.quadriguttatus 0.758438472  3.6915056297 greater 0.02970297*
#76  Glischrochilus.quadripunctatus 1.891081884  0.9972882647 greater 0.15841584
#77            Globicornis.nigripes 0.446731727 -0.3652706844 greater 0.53465347
#78       Gnathotrichus.materiarius 0.860462697  2.6888168228 greater 0.02970297*
#79                Gonodera.luperus 0.566826808 -0.8738463058 greater 0.78217822
#80         Grammoptera.abdominalis 0.185573009 -0.7712874246 greater 0.74257426
#81          Grammoptera.ruficornis 0.975099384  0.4603111737 greater 0.24752475
#82            Grammoptera.ustulata 0.020837332 -0.9866224753 greater 0.97029703
#83          Hemicoelus.fulvicornis 0.191049043  0.3509393489 greater 0.26732673
#84              Hemicoelus.nitidus 4.345943339  1.7268605002 greater 0.05940594
#85                   Hylastes.ater 0.068438599 -1.1068717224 greater 0.90099010
#86             Hylastes.attenuatus 0.187898908  0.1069743511 greater 0.36633663
#87           Hylastes.cunicularius 1.000283063  0.0492277570 greater 0.35643564
#88               Hylastes.linearis 0.223913989  1.0097631750 greater 0.15841584
#89                 Hylastes.opacus 0.523241978  0.2090805966 greater 0.28712871
#90             Hylastinus.obscurus 0.011317596 -1.1786501793 greater 1.00000000
#91               Hylesinus.fraxini 0.964711313 -0.6364789932 greater 0.70297030
#92                Hylis.cariniceps 0.197317340 -0.8695271939 greater 0.81188119
#93                    Hylis.olexai 0.662541793  0.8695732500 greater 0.19801980
#94                   Hylis.simonae 0.579701807 -0.8365472568 greater 0.72277228
#95                Hylobius.abietis 0.964711313 -0.7208701432 greater 0.72277228
#96             Hylurgops.palliatus 0.806273801  8.1592990098 greater 0.00990099**
#97             Hylurgus.ligniperda 1.506092590  1.2175510813 greater 0.09900990
#98        Ischnodes.sanguinicollis 3.526772486  1.2089805541 greater 0.14851485
#99               Ischnomera.cyanea 0.958072983  2.6443009787 greater 0.02970297*
#100           Isoriphis.melasoides 0.530174390  1.2940197802 greater 0.13861386
#101              Leiopus.femoratus 0.579701807 -0.9487525502 greater 0.84158416
#102              Leiopus.nebulosus 1.315424542  2.3865316757 greater 0.04950495*
#103              Leptura.aurulenta 4.568265363  1.6768365249 greater 0.05940594
#104           Lissodema.denticolle 0.019339906 -0.8819756434 greater 0.89108911
#105              Litargus.connexus 0.142002841  0.8757712191 greater 0.19801980
#106                 Lucanus.cervus 0.579701807 -0.8188475201 greater 0.72277228
#107                  Malthinus.sp. 0.416364279  0.2160785856 greater 0.37623762
#108                Megatoma.undata 0.096746994 -0.5168256402 greater 0.55445545
#109              Melandrya.barbata 0.532567445  1.4359605734 greater 0.12871287
#110           Melandrya.caraboides 1.100014260  0.2991411446 greater 0.34653465
#111             Melanotus.villosus 0.098431820  1.9425880616 greater 0.05940594
#112           Melasis.buprestoides 0.014299441 -0.5322189737 greater 0.62376238
#113          Mesosa.curculionoides 0.154234157 -0.8291724878 greater 0.78217822
#114                Mesosa.nebulosa 0.115850013 -0.6092105499 greater 0.65346535
#115           Microrhagus.pygmaeus 0.029140294 -0.9418377661 greater 0.85148515
#116              Mycetochara.maura 0.765315794  2.4335214475 greater 0.02970297
#117         Mycetophagus.atomarius 0.579701807 -0.8690346211 greater 0.71287129
#118            Mycetophagus.piceus 0.559155316 -0.5400742724 greater 0.55445545
#119    Mycetophagus.quadriguttatus 4.350906460  1.8622598911 greater 0.06930693
#120  Mycetophagus.quadripustulatus 0.797025140 -0.3633444077 greater 0.56435644
#121     Nalassus.laevioctostriatus 0.137274164 -0.3026716336 greater 0.52475248
#122             Nemozoma.elongatum 0.071668444 -1.1383791719 greater 0.93069307
#123         Octotemnus.glabriculus 0.099216752 -0.9074147769 greater 0.82178218
#124            Oligomerus.brunneus 2.211197603  1.4414421599 greater 0.10891089
#125              Orchesia.undulata 4.315988127  1.2265289446 greater 0.12871287
#126                  Orthocis.alni 0.096873574 -0.9775777965 greater 0.90099010
#127               Orthocis.coluber 2.780550722  0.4464223174 greater 0.31683168
#128              Orthocis.vestitus 0.446421702  1.6504288220 greater 0.04950495*
#129            Orthotomicus.erosus 0.275680009 -0.6897594585 greater 0.73267327
#130          Oxylaemus.cylindricus 0.416019950 -0.7747648701 greater 0.81188119
#131     Pachytodes.cerambyciformis 0.489254992 -0.5071585905 greater 0.57425743
#132              Palorus.depressus 2.871553376  0.6749049790 greater 0.24752475
#133     Paromalus.parallelepipedus 0.200030087 -1.1173408752 greater 0.92079208
#134     Phloeotribus.rhododactylus 0.711085071 -0.2996558165 greater 0.51485149
#135           Phymatodes.testaceus 0.286698005  0.1945479563 greater 0.35643564
#136          Pityogenes.bidentatus 1.113317788  0.7348588464 greater 0.19801980
#137        Pityophagus.ferrugineus 3.041828929  0.7265561334 greater 0.21782178
#138         Pityophthorus.buyssoni 1.462865485  0.6294152185 greater 0.27722772
#139     Pityophthorus.pityographus 1.736882016 -0.0492244356 greater 0.50495050
#140        Pityophthorus.pubescens 0.818174480  1.8824541207 greater 0.06930693
#141          Platycerus.caraboides 0.146144360  0.0698919138 greater 0.33663366
#142          Platyrhinus.resinosus 0.119491095 -1.2156333252 greater 0.97029703
#143            Platysoma.elongatus 4.345943339  1.3323804596 greater 0.17821782
#144            Platystomos.albinus 0.165919028 -0.5500210337 greater 0.63366337
#145                 Poecilium.alni 1.210444843 -0.5478777131 greater 0.73267327
#146            Pogonocherus.ovatus 1.076760882  0.0660945821 greater 0.34653465
#147          Prionychus.melanarius 0.186025164 -1.2525807177 greater 0.95049505
#148              Protaetia.fieberi 0.405783943 -0.1024093787 greater 0.41584158
#149             Protaetia.lugubris 3.259127180  2.5756961423 greater 0.02970297*
#150      Pseudocistela.ceramboides 0.776361015 -0.8530949351 greater 0.80198020
#151        Ptinomorphus.imperialis 0.363043476 -0.5900311244 greater 0.65346535
#152             Pyrochroa.coccinea 0.566826808 -0.8328156912 greater 0.77227723
#153          Pyrrhidium.sanguineum 0.141472570  0.1311024018 greater 0.32673267
#154            Rhagium.bifasciatum 0.316525098 -1.2695966200 greater 0.88118812
#155                 Rhagium.mordax 0.410474472  1.5005727339 greater 0.09900990
#156             Rhagium.sycophanta 0.211940753  1.5968011670 greater 0.11881188
#157       Rhizophagus.bipustulatus 0.004179539 -0.8244294573 greater 0.84158416
#158          Rhizophagus.depressus 0.918483208  2.4375479160 greater 0.03960396*
#159        Rhizophagus.ferrugineus 0.265431122 -0.7230501191 greater 0.74257426
#160    Rhizophagus.parallelocollis 5.151804344  1.5880915069 greater 0.08910891
#161            Rhizophagus.picipes 2.048791879 -0.1957348930 greater 0.58415842
#162            Ropalopus.femoratus 1.569160775  1.4507365026 greater 0.12871287
#163               Rutpela.maculata 0.171192401  0.0582780202 greater 0.38613861
#164         Salpingus.planirostris 0.109979405 -0.1306978925 greater 0.42574257
#165           Salpingus.ruficollis 0.139873463  0.2457848979 greater 0.28712871
#166          Scaphidema.metallicum 0.964711313 -0.6069752256 greater 0.65346535
#167            Scolytus.intricatus 0.166187312  0.0764780241 greater 0.42574257
#168                 Sericoderus.sp 0.730546079  0.3442325724 greater 0.27722772
#169           Silvanus.unidentatus 0.072197804 -0.8976925046 greater 0.87128713
#170                 Soronia.grisea 0.257424431  5.9467041869 greater 0.00990099**
#171          Soronia.punctatissima 0.234387287 -0.2326579491 greater 0.49504950
#172         Sphaeriestes.castaneus 0.698924863 -0.3191877409 greater 0.53465347
#173         Spondylis.buprestoides 0.100999673 -0.9668775004 greater 0.91089109
#174           Stenagostus.rhombeus 0.089971617 -0.5808054195 greater 0.66336634
#175            Stenurella.melanura 0.130609630 -0.3683064475 greater 0.57425743
#176    Stephostethus.angusticollis 0.377411506 -0.4218921347 greater 0.56435644
#177              Stephostethus.sp. 0.992889955  1.0269311585 greater 0.17821782
#178            Stictoleptura.rubra 0.740862972  0.1599343304 greater 0.30693069
#179       Stictoleptura.scutellata 2.266209191  0.2455393378 greater 0.37623762
#180             Synchita.humeralis 2.266209191  0.1084653121 greater 0.37623762
#181        Taphrorychus.villifrons 0.185110744 -0.5687816132 greater 0.65346535
#182         Thanasimus.formicarius 0.166505571  0.2323363028 greater 0.30693069
#183              Thymalus.limbatus 0.027803524 -1.0231886446 greater 0.87128713
#184              Tomicus.piniperda 0.716728003  9.4900725276 greater 0.00990099**
#185              Tomoxia.bucephala 0.179018169 -0.9132961672 greater 0.85148515
#186             Trachodes.hispidus 0.283500173 -1.0885947259 greater 0.93069307
#187           Trichoceble.floralis 2.480915986  1.3639112893 greater 0.15841584
#188                 Triplax.lepida 0.165309416 -0.4103354735 greater 0.61386139
#189                Triplax.rufipes 0.818073709 -0.3305262079 greater 0.54455446
#190                Triplax.russica 0.406059916  0.3501854130 greater 0.31683168
#191            Tritoma.bipustulata 0.446622519  0.4278384363 greater 0.24752475
#192         Tropideres.albirostris 0.106649590  0.1078296779 greater 0.39603960
#193        Trypodendron.domesticum 0.052657941 -0.8573886135 greater 0.82178218
#194          Trypodendron.signatum 0.265180819  3.1179947021 greater 0.02970297*
#195               Uleiota.planatus 0.316525098 -1.0074293576 greater 0.82178218
#196              Valgus.hemipterus 0.050977142 -1.0349908181 greater 0.96039604
#197        Vincenzellus.ruficollis 0.201636383  1.7032412255 greater 0.06930693
#198         Xestobium.rufovillosum 0.475685369 -0.5424277601 greater 0.61386139
#199          Xyleborinus.saxesenii 0.029936723 -0.4046254262 greater 0.52475248
#200               Xyleborus.dispar 0.735003365  1.3980567279 greater 0.10891089


# Creation of the factor
fact <- Saprox.Rel.Env.Sp$cat_mixture_plot
# plotting the two subsets
s.class(nic1$ls, fact, col=c("red", "blue","black"),cellipse=0, cpoint=2, pch=3)
s.chull(nic1$ls, fact, col=c("red", "blue","black"),optchull = 1, add.plot = T)



######################################################
##############        SAPROX - PCA      #####################
#####################################################

# Analyse en composantes principales
Saprox_acp <- PCA(Saprox.Rel.Env.Sp[,c(11:213)], graph = FALSE)

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

Saprox.rda <- rda(Saprox.Rel.Env.Sp[,c(11:213)])
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
indval_Saprox <- multipatt(Saprox.Rel.Env.Sp[,c(11:213)], Saprox.Rel.Env.Sp$cat_mixture_plot,control = how(nperm=999)) 
summary(indval_Saprox)

#   Multilevel pattern analysis
#---------------------------
#  Association function: IndVal.g
#Significance level (alpha): 0.05
#Total number of species: 203
#Selected number of species: 16 
#Number of species associated to 1 group: 8 
#Number of species associated to 2 groups: 8 
#List of species associated to each combination: 
#  Group pine  #sps.  4 
#stat p.value    
#Hylurgops.palliatus     0.856   0.001 ***
#  Tomicus.piniperda       0.839   0.002 ** 
#  Pityophthorus.pubescens 0.652   0.009 ** 
#  Rhizophagus.depressus   0.612   0.010 ** 
#  Group mixed  #sps.  3 
#stat p.value   
#Silvanus.unidentatus   0.520   0.040 * 
#  Thymalus.limbatus      0.513   0.007 **
#  Octotemnus.glabriculus 0.459   0.032 * 
#  Group oak  #sps.  1 
#stat p.value    
#Cryptarcha.undata 0.598   0.001 ***
#  Group pine+mixed  #sps.  5 
#stat p.value   
#Hylastes.linearis             0.854   0.003 **
#  Hylastes.attenuatus           0.806   0.012 * 
#  Rhagium.mordax                0.664   0.005 **
#  Ampedus.sanguinolentus        0.652   0.028 * 
#  Glischrochilus.quadriguttatus 0.628   0.032 * 
#  Group pine+oak  #sps.  1 
#stat p.value  
#Vincenzellus.ruficollis 0.707   0.025 *
  
  Group mixed+oak  #sps.  2 
stat p.value  
Ampedus.quercicola   0.792   0.018 *
  Isoriphis.melasoides 0.744   0.047 *


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


