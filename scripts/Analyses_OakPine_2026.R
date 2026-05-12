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
library(adespatial)

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

####################################################################################################################################
#### CARABIDS - GLMMM
####################################################################################################################################

###CARABIDS - Species richness

#CARABIDS - Statistical distribution

descdist(Carab.Rel.Env$RS_all,discrete=TRUE,boot=1001)
plot(fitdist(Carab.Rel.Env$SS_all,"norm"))
fitnb<-fitdist(Carab.Rel.Env$SR_all,"nbinom")
fitp<-fitdist(Carab.Rel.Env$SR_all,"pois")
fitn<-fitdist(Carab.Rel.Env$SR_all,"norm")
fitlnorm<-fitdist(Carab.Rel.Env$SR_all,"lnorm")
fitexp<-fitdist(Carab.Rel.Env$SR_all,"exp")
fitgeom<-fitdist(Carab.Rel.Env$SR_all,"geom")
fitbeta<-fitdist(Carab.Rel.Env$SR_all,"beta")
fitunif<-fitdist(Carab.Rel.Env$SR_all,"unif")
fitg<-fitdist(Carab.Rel.Env$SR_all,"gamma",method="mme")
gofstat(fitnb)$chisqpvalue
gofstat(fitp)$chisqpvalue
gofstat(fitn)$chisqpvalue
gofstat(fitlnorm)$chisqpvalue
gofstat(fitexp)$chisqpvalue
gofstat(fitgeom)$chisqpvalue
gofstat(fitunif)$chisqpvalue
gofstat(fibeta)$chisqpvalue
gofstat(fitg)$chisqpvalue

#CARABIDS - SR all species with quadratic effect of tree mixture

glmm_RS_all<-glmmTMB(SR_all~G_all_plot+I(mel_plot/100)+I((mel_plot/100)^2)+(1|stand),family=poisson,data=Carab.Rel.Env)
summary(glmm_RS_all)
# Family: poisson  ( log )
#Formula:          SR_all ~ G_all_plot + I(mel_plot/100) + I((mel_plot/100)^2) +      (1 | stand)
#Data: Carab.Rel.Env
#AIC       BIC    logLik -2*log(L)  df.resid 
#285.4     296.5    -137.7     275.4        63 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 4.677e-10 2.163e-05
#Number of obs: 68, groups:  stand, 15
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          1.13172    0.30618   3.696 0.000219 ***
#  G_all_plot           0.01704    0.01017   1.675 0.093928 .  
#I(mel_plot/100)      0.68892    0.59584   1.156 0.247594    
#I((mel_plot/100)^2) -0.40080    0.60261  -0.665 0.505983 
AICc(glmm_RS_all) #286.3324

#CARABIDS - SR all species with simple effect of tree mixture

glmm_RS_all<-glmmTMB(SR_all~G_all_plot+I(mel_plot/100)+(1|stand),family=poisson,data=Carab.Rel.Env)
summary(glmm_RS_all)
# Family: poisson  ( log )
#Formula:          SR_all ~ G_all_plot + I(mel_plot/100) + (1 | stand)
#Data: Carab.Rel.Env
#AIC       BIC    logLik -2*log(L)  df.resid 
#283.8     292.7    -137.9     275.8        64 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 2.639e-10 1.624e-05
#Number of obs: 68, groups:  stand, 15
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     1.130849   0.304644   3.712 0.000206 ***
#  G_all_plot      0.018681   0.009855   1.896 0.058023 .  
#I(mel_plot/100) 0.307193   0.160231   1.917 0.055214 . 
AICc(glmm_RS_all)#284.4429

# Create prediction grid
mel_seq <- seq(min(Carab.Rel.Env$mel_plot), max(Carab.Rel.Env$mel_plot), length.out = 200)
G_all_moy<-mean(Carab.Rel.Env$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
stand_seq<-rep("245",200)
pred <- predict(
  glmm_RS_all,
  newdata = data.frame(G_all_plot=G_all_seq,mel_plot=mel_seq,stand=stand_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  mel_plot = mel_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = mel_plot, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Carab.Rel.Env, aes(x = mel_plot, y = SR_all), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of carabid species (all)") +
  theme_minimal()

#CARABIDS - Abundance all species with quadratic effect of tree mixture

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

glmm_Abdce_all_nb<-glmmTMB(Abdce_all~G_all_plot+I(mel_plot/100)+I((mel_plot/100)^2)+(1|stand),family=nbinom1(),data=Carab.Rel.Env)
summary(glmm_Abdce_all_nb) 
# Family: nbinom1  ( log )
#Formula:          Abdce_all ~ G_all_plot + I(mel_plot/100) + I((mel_plot/100)^2) +      (1 | stand)
#Data: Carab.Rel.Env
#AIC       BIC    logLik -2*log(L)  df.resid 
#529.9     543.2    -259.0     517.9        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.1954   0.4421  
#Number of obs: 68, groups:  stand, 15
#Dispersion parameter for nbinom1 family ():  2.5 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          2.29510    0.43777   5.243 1.58e-07 ***
#  G_all_plot           0.03039    0.01521   1.998   0.0457 *  
#  I(mel_plot/100)      0.38686    0.98283   0.394   0.6939    
#I((mel_plot/100)^2) -0.16514    0.98353  -0.168   0.8667 

sim<-simulateResiduals(glmm_Abdce_all_nb)
testUniformity(sim)#D = 0.06594, p-value = 0.9099 
#KS Test p-value # Dispersion test  # Outliers 
testOutliers(sim) #p-value = 1

#CARABIDS - Abundance all species with simple effect of tree mixture

glmm_Abdce_all_nb<-glmmTMB(Abdce_all~G_all_plot+I(mel_plot/100)+(1|stand),family=nbinom1(),data=Carab.Rel.Env)
summary(glmm_Abdce_all_nb) 
#Family: nbinom1  ( log )
#Formula:          Abdce_all ~ G_all_plot + I(mel_plot/100) + (1 | stand)
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
#  I(mel_plot/100)  0.23098    0.32166   0.718   0.4727

ggplot(Carab.Rel.Env, aes(x = mel_plot, y = Abdce_all))+  
         geom_point()+
  labs(x = "Mixture (% oak vs pine+oak)", y = "Abundance of ground beetles (all species)") +
  theme_minimal()

# #magnitude
#récupérer les valeurs de Estimate et Std. Error pour la variable d’intérêt X
Estimate_mod<-rnorm(10000,mean=0.23098,0.32166)
DX<-exp(Estimate_mod*0.1)-1 #pour un delta de X de 10 (%)
mean(DX)
quantile(DX, c(0.01, 0.99))


#####################################################
##############         ACP      #####################
#####################################################

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

#####################################################
##############         RDA      #####################
#####################################################
Carab.rda <- rda(Carab.Rel.Spe[,c(2:29)])
biplot(Carab.rda)
ordiellipse(Carab.rda,group = Carab.Rel.Env$cat_mel_plot,col = c(1,2,3),label=TRUE)

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
indval_Carab <- multipatt(Carab.Rel.Spe[,c(2:29)], Carab.Rel.Env$cat_mel_plot,control = how(nperm=999)) 
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

#Reordering tree mixture categories along a gradient of increasing oak (deciduous) basal area 
Bird.Rel.Env.Sp$cat_mixture_plot<- factor(Bird.Rel.Env.Sp$cat_mixture_plot, levels = c("Pine", "Mixed", "Oak"))

####################################################################################################################################
#######BIRDS - GLMM
####################################################################################################################################

#### BIRDS - SR all species with quadratic effect of tree mixture

glmm_SR_all<-glmmTMB(SR_all~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_all) #simple effect, p=0.12, quadratic effect p=0.29, G p=0.31, taux_veg1=0.83, AIC=285.2
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)           2.363951   0.230875  10.239   <2e-16 ***
#  G_all_plot                -0.002286   0.008852  -0.258   0.7962    
#I(mixture_plot/100)      1.173185   0.508405   2.308   0.0210 *  
#  I((mixture_plot/100)^2) -1.016384   0.608559  -1.670   0.0949 .  
AICc(glmm_SR_all) #[1] 334.2836

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_all,
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

#### BIRDS - SR all species with simple effect of tree mixture

glmm_SR_all<-glmmTMB(SR_all~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_all) #simple effect, p=0.0377, AIC=284.3
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       2.4284617  0.2252669  10.780  < 2e-16 ***
#  G_all_plot            -0.0009063  0.0087510  -0.104  0.91751    
#I(mel_plot/100)  0.3535493  0.1354959   2.609  0.00907 ** 
AICc(glmm_SR_all) #334.7857


#### BIRDS - SR Generalist species with quadratic effect of tree mixture

glmm_SR_Generalist<-glmmTMB(SR_Generalist1~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Generalist)
# Family: poisson  ( log )
#Formula:          SR_Generalist1 ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#312.2     323.1    -151.1     302.2        61
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 5.717e-11 7.561e-06
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              1.974598   0.263747   7.487 7.06e-14 ***
#  G_all_plot               0.004086   0.010082   0.405    0.685    
#I(mixture_plot/100)      0.928436   0.577441   1.608    0.108    
#I((mixture_plot/100)^2) -0.767637   0.690037  -1.112    0.266  
AICc(glmm_SR_Generalist) #313.1851

# Create prediction grid
mel_seq <- seq(min(Bird.Rel.Env.Sp$MEL_point), max(Bird.Rel.Env.Sp$MEL_point), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Generalist,
  newdata = data.frame(G_all=G_all_seq,MEL_point = mel_seq,plot=plot_seq),type="response",re.form=NA,
  se.fit = TRUE)

# Compute 95% CI
crit <- qnorm(0.975)  # 1.96 for 95%
pred_df <- data.frame(
  MEL_point = mel_seq,
  fit = pred$fit,
  lower = pred$fit - crit * pred$se.fit,
  upper = pred$fit + crit * pred$se.fit)

# Plot with ggplot2
ggplot(pred_df, aes(x = MEL_point, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = Bird.Rel.Env.Sp, aes(x = MEL_point, y = SR_Generalist), color = "black") +
  labs(title = "GLMM: SR_Generalist ~ G + MEL +MEL2 with 95% CI", x = "Deciduous basal area (%)", y = "Generalist Species number") +
  theme_minimal()

#### BIRDS - SR Generalist species with simple effect of tree mixture

glmm_SR_Generalist<-glmmTMB(SR_Generalist1~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Generalist)
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
AICc(glmm_SR_Generalist) #312.0974

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
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
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Generalist1), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of bird species (generalist)") +
  theme_minimal()

#### BIRDS - SR Oak species with quadratic effect of tree mixture

glmm_SR_Oak<-glmmTMB(SR_Oak~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Oak)
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
AICc(glmm_SR_Oak) #201.2625

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
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
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Oak), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of bird species (oak specialist)") +
  theme_minimal()

#### BIRDS - SR Oak species with simple effect of tree mixture

glmm_SR_Oak<-glmmTMB(SR_Oak~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Oak)
# Family: poisson  ( log )
#Formula:          SR_Oak ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#203.3     212.0     -97.6     195.3        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.04468  0.2114  
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         -0.49425    0.70902  -0.697    0.486    
#G_all_plot           0.00817    0.02658   0.307    0.759    
#I(mixture_plot/100)  1.81626    0.42750   4.249 2.15e-05 ***
AICc(glmm_SR_Oak) #203.9422

#### BIRDS - SR Pine species with quadratic effect of tree mixture

glmm_SR_Pine<-glmmTMB(SR_Pine~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Pine)
#Family: poisson  ( log )
#Formula:          SR_Pine ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#177.7     188.7     -83.9     167.7        61 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 4.646e-10 2.156e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)              2.14287    0.72075   2.973  0.00295 **
#  G_all_plot              -0.06943    0.03081  -2.253  0.02426 * 
#  I(mixture_plot/100)      1.37804    1.83630   0.750  0.45299   
#I((mixture_plot/100)^2) -3.57465    2.51430  -1.422  0.15511   
AICc(glmm_SR_Pine) #178.7159

#### BIRDS - SR Pine species with simple effect of tree mixture

glmm_SR_Pine<-glmmTMB(SR_Pine~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Pine)
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
AICc(glmm_SR_Pine) #178.5433

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Pine,
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


#### BIRDS - SR Generalist 2 (excluding species tolerating mixed stands but avoiding pure stands of one species) with quadratic effect of tree mixture

glmm_SR_Generalist2<-glmmTMB(SR_Generalist2~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Generalist2)
#Family: poisson  ( log )
#Formula:          SR_Generalist2 ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#286.3     297.2    -138.1     276.3        61 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 7.145e-11 8.453e-06
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              1.898709   0.311285   6.100 1.06e-09 ***
#  G_all_plot              -0.008717   0.011947  -0.730   0.4656    
#I(mixture_plot/100)      1.136805   0.684469   1.661   0.0967 .  
#I((mixture_plot/100)^2) -0.827277   0.814953  -1.015   0.3100 
AICc(glmm_SR_Generalist2) #287.2593

#### BIRDS - SR Generalist 2 (excluding species tolerating mixed stands but avoiding pure stands of one species) with simple effect of tree mixture

glmm_SR_Generalist2<-glmmTMB(SR_Generalist2~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Generalist2)
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
AICc(glmm_SR_Generalist2) #285.9623

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Generalist2,
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
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = SR_Generalist2), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of bird species (generalist)") +
  theme_minimal()

#### BIRDS - SR Oak_Mixed (preferring oak but tolerating mixed deciduous-conifer stands) with quadratic effect of tree mixture

glmm_SR_Oak_Mixed<-glmmTMB(SR_Oak_Mixed~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Oak_Mixed)
#Family: poisson  ( log )
#Formula:          SR_Oak_Mixed ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#217.7     228.6    -103.8     207.7        61 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 2.327e-10 1.526e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)             -0.05999    0.51825  -0.116   0.9079  
#G_all_plot               0.03591    0.01963   1.830   0.0673 .
#I(mixture_plot/100)      1.07021    1.13902   0.940   0.3474  
#I((mixture_plot/100)^2) -1.56072    1.38992  -1.123   0.2615
AICc(glmm_SR_Oak_Mixed) #218.682

#### BIRDS - SR Oak_Mixed (preferring oak but tolerating mixed deciduous-conifer stands) with simple effect of tree mixture

glmm_SR_Oak_Mixed<-glmmTMB(SR_Oak_Mixed~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Oak_Mixed)
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
  glmm_SR_Oak_Mixed,
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

#### BIRDS - Abundance Generalist 2 (excluding species tolerating mixed stands but avoiding pure stands of one species) with quadratic effect of tree mixture

glmm_Abdce_Generalist2<-glmmTMB(Abdce_Generalist2~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_Generalist2)
#Family: poisson  ( log )
#Formula:          Abdce_Generalist2 ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#345.2     356.2    -167.6     335.2        61 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 1.043e-10 1.021e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              2.2632535  0.2348762   9.636   <2e-16 ***
#  G_all_plot               0.0008921  0.0089622   0.100    0.921    
#I(mixture_plot/100)      0.8266490  0.5119933   1.615    0.106    
#I((mixture_plot/100)^2) -0.4907771  0.6071389  -0.808    0.419
AICc(glmm_Abdce_Generalist2) #346.2066

#### BIRDS - Abundance Generalist 2 (excluding species tolerating mixed stands but avoiding pure stands of one species) with simple effect of tree mixture

glmm_Abdce_Generalist2<-glmmTMB(Abdce_Generalist2~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_Generalist2)
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
AICc(glmm_Abdce_Generalist2) #344.522

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Generalist2,
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


##################################################################################################
########################   BIRD Abundance
##################################################################################################

#### BIRDS - Abundance all species with quadratic effect of tree mixture

glmm_Abdce_all<-glmmTMB(Abdce_all~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_all)
#Family: poisson  ( log )
#Formula:          Abdce_all ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#388.0     398.9    -189.0     378.0        61 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev.
#stand  (Intercept) 1.743e-10 1.32e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              2.7729872  0.1855616  14.944   <2e-16 ***
#  G_all_plot              -0.0004843  0.0071010  -0.068   0.9456    
#I(mixture_plot/100)      0.9198519  0.4061184   2.265   0.0235 *  
#I((mixture_plot/100)^2) -0.6660573  0.4838513  -1.377   0.1686  
AICc(glmm_Abdce_all) #388.9731

#### BIRDS - Abundance all species with simple effect of tree mixture

glmm_Abdce_all<-glmmTMB(Abdce_all~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_all)
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
AICc(glmm_Abdce_all) #388.5486

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_all,
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

#### BIRDS - Abundance generalist species with quadratic effect of tree mixture

glmm_Abdce_Generalist1<-glmmTMB(Abdce_Generalist1~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_Generalist1)
#Family: poisson  ( log )
#Formula:          Abdce_Generalist1 ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#367.6     378.6    -178.8     357.6        61 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 9.942e-11 9.971e-06
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              2.370069   0.207701  11.411   <2e-16 ***
#  G_all_plot               0.008026   0.007916   1.014   0.3106    
#I(mixture_plot/100)      0.751127   0.452337   1.661   0.0968 .  
#I((mixture_plot/100)^2) -0.542175   0.538641  -1.007   0.3141
AICc(glmm_Abdce_Generalist1) #368.6477

#### BIRDS - Abundance generalist species with simple effect of tree mixture

glmm_Abdce_Generalist1<-glmmTMB(Abdce_Generalist1~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_Generalist1)
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
AICc(glmm_Abdce_Generalist1) #367.3275

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Generalist1,
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

#### BIRDS - Abundance Oak_Mixed (preferring oak but tolerating mixed deciduous-conifer stands) with quadratic effect of tree mixture

glmm_Abdce_Oak_Mixed<-glmmTMB(Abdce_Oak_Mixed~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_Oak_Mixed)
#Family: poisson  ( log )
#Formula:          Abdce_Oak_Mixed ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#247.9     258.8    -118.9     237.9        61 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 3.186e-10 1.785e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)              0.31987    0.45861   0.698   0.4855  
#G_all_plot               0.03016    0.01743   1.731   0.0835 .
#I(mixture_plot/100)      1.04049    1.00913   1.031   0.3025  
#I((mixture_plot/100)^2) -1.46316    1.23020  -1.189   0.2343  
AICc(glmm_Abdce_Oak_Mixed) #248.8811

#### BIRDS - Abundance Oak_Mixed (preferring oak but tolerating mixed deciduous-conifer stands) with simple effect of tree mixture

glmm_Abdce_Oak_Mixed<-glmmTMB(Abdce_Oak_Mixed~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_Oak_0_1_Mixed)
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
AICc(glmm_Abdce_Oak_Mixed) #247.9966

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Oak_0_1_Mixed,
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

#### BIRDS - Abdce Oak species with quadratic effect of tree mixture

glmm_Abdce_Oak<-glmmTMB(Abdce_Oak~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_Oak_0_1)
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
AICc(glmm_Abdce_Oak) #236.4866

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_Oak,
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

#### BIRDS - Abundance Oak species with simple effect of tree mixture

glmm_Abdce_Oak<-glmmTMB(Abdce_Oak~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_Oak)
# Family: poisson  ( log )
#Formula:          Abdce_Oak ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#237.6     246.3    -114.8     229.6        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.1115   0.3339  
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         -0.1119748  0.7366182  -0.152    0.879    
#G_all_plot          -0.0009224  0.0280544  -0.033    0.974    
#I(mixture_plot/100)  2.0132773  0.4291619   4.691 2.72e-06 ***
AICc(glmm_Abdce_Oak) #238.2129

#### BIRDS - Abundance Pine species with quadratic effect of tree mixture

glmm_Abdce_Pine<-glmmTMB(Abdce_Pine~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_Pine)
#Family: poisson  ( log )
#Formula:          Abdce_Pine ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#201.6     212.5     -95.8     191.6        61 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 6.364e-10 2.523e-05
#Number of obs: 66, groups:  stand, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              2.86755    0.64067   4.476 7.61e-06 ***
#  G_all_plot              -0.08674    0.02778  -3.122   0.0018 ** 
#  I(mixture_plot/100)      0.89550    1.64031   0.546   0.5851    
#I((mixture_plot/100)^2) -3.05596    2.25445  -1.356   0.1753   
AICc(glmm_Abdce_Pine) #202.5964

#### BIRDS - Abundance Oak species with simple effect of tree mixture

glmm_Abdce_Pine<-glmmTMB(Abdce_Pine~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_Pine)
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
AICc(glmm_Abdce_Pine) #202.2014

# Create prediction grid
mixture_seq <- seq(min(Bird.Rel.Env.Sp$mixture_plot), max(Bird.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
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
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mixture_plot, y = Abdce_Pine), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of bird individuals (pine specialist)") +
  theme_minimal()





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
#ordihull(Bird.rda,group = Bird.Rel.Env.Sp$cat_mel_plot,col = c(1,2,3),label=TRUE)
ordiellipse(Bird.rda,group = Bird.Rel.Env.Sp$cat_mel_plot,col = c(1,2,3),label=TRUE)

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

beta_bird<-beta.div(Bird.Rel.Env.Sp[,c(7:45)])
plot(Bird.Rel.Env.Sp$MEL_point,beta_bird$LCBD)
cor.test(Bird.Rel.Env.Sp$MEL_point,beta_bird$LCBD)

indval_Bird <- multipatt(Bird.Rel.Env.Sp[,c(7:45)], Bird.Rel.Env.Sp$cat_mel_plot,control = how(nperm=999)) 
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

Saprox.Rel.Env.Sp<-read.csv("Rel_Env_Sp_Saprox_2026.csv", sep=";", header=T)
dim(Saprox.Rel.Env.Sp) #48 222

#Reordering tree mixture categories along a gradient of increasing oak (deciduous) basal area 
Saprox.Rel.Env.Sp$cat_mixture_plot<- factor(Saprox.Rel.Env.Sp$cat_mixture_plot, levels = c("pine", "mixed", "oak"))


##############################################################################################
###########################     SAPROX Species richness    ##################################
##############################################################################################

#####################################
########Species richness all species
################################

hist(Saprox.Rel.Sp$SR_all) #family Poisson?
mean(Saprox.Rel.Sp$SR_all) #38.29167
var(Saprox.Rel.Sp$SR_all) #59.70035

descdist(Saprox.Rel.Env$SR_all,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env$SR_all,"pois"))
fitnb<-fitdist(Saprox.Rel.Env$SR_all,"nbinom")
fitp<-fitdist(Saprox.Rel.Env$SR_all,"pois")
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

########Species richness all species with quadratic effect of tree mixture

glmm_SR_all<-glmmTMB(SR_all~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
summary(glmm_SR_all) 
#Family: poisson  ( log )
#Formula:          SR_all ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Saprox.Rel.Env
#AIC       BIC    logLik -2*log(L)  df.resid 
#329.7     339.1    -159.9     319.7        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.004935 0.07025 
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              3.863717   0.099859   38.69  < 2e-16 ***
#  G_all_plot              -0.010174   0.003329   -3.06  0.00224 ** 
#  I(mixture_plot/100)      0.386640   0.277986    1.39  0.16427    
#I((mixture_plot/100)^2) -0.424306   0.267920   -1.58  0.11326   
AICc(glmm_SR_all)#331.1574

########Species richness all species with simple effect of tree mixture

glmm_SR_all<-glmmTMB(SR_all~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
summary(glmm_SR_all) 
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
AICc(glmm_SR_all)#331.0967

sim<-simulateResiduals(glmm_SR_all)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value 0.77 # Dispersion test 0.608  # Outliers p=1 
testOutliers(sim) #p=1

# Create prediction grid
mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_all,
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

#####################################
########RS conif
################################

descdist(Saprox.Rel.Env$rs.conif,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env$rs.conif,"pois"))
fitnb<-fitdist(Saprox.Rel.Env$rs.conif,"nbinom")
fitp<-fitdist(Saprox.Rel.Env$rs.conif,"pois")
gofstat(fitnb)$chisqpvalue #0.00205022 --> Poisson
gofstat(fitp)$chisqpvalue #1.239995e-28 

########Species richness conifer specialist species with quadratic effect of tree mixture

glmm_SR_Pine<-glmmTMB(SR_Pine~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
summary(glmm_SR_Pine) 
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
AICc(glmm_SR_Pine) #226.6784

sim<-simulateResiduals(glmm_SR_Pine)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value p=.47785 #Dispersion test p=0.872 # Outliers p=0.34
testOutliers(sim) # 1 outlier (lowest residual)

# Create prediction grid
mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Pine,
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

########Species richness conifer specialist species with simple effect of tree mixture

glmm_SR_Pine<-glmmTMB(SR_Pine~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
summary(glmm_SR_Pine) 
# Family: poisson  ( log )
#Formula:          SR_Pine ~ G_all_plot + I(mixture_plot/100) + (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#227.5     235.0    -109.7     219.5        44 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 0.208    0.4561  
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          2.66470    0.34901   7.635 2.26e-14 ***
#  G_all_plot          -0.02220    0.01224  -1.814   0.0697 .  
#I(mixture_plot/100) -1.55537    0.33774  -4.605 4.12e-06 ***  
AICc(glmm_SR_Pine) #228.4148


#####################################
########RS deciduous
################################

descdist(Saprox.Rel.Env.Sp$SR_Oak,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env.Sp$SR_Oak,"pois"))
fitnb<-fitdist(Saprox.Rel.Env.Sp$SR_Oak,"nbinom")
fitp<-fitdist(Saprox.Rel.Env.Sp$SR_Oak,"pois")
gofstat(fitnb)$chisqpvalue #0.4257678 --> Negbin
gofstat(fitp)$chisqpvalue #0.1261187 

########Species richness oak specialist species with quadratic effect of tree mixture

glmm_SR_Oak<-glmmTMB(SR_Oak~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
summary(glmm_SR_Oak) 
#  Family: poisson  ( log )
#Formula:          SR_Oak ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#308.5     317.8    -149.2     298.5        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 1.967e-10 1.402e-05
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              3.492061   0.102577   34.04  < 2e-16 ***
#  G_all_plot              -0.009262   0.003389   -2.73  0.00627 ** 
#  I(mixture_plot/100)      0.556035   0.280619    1.98  0.04754 *  
#  I((mixture_plot/100)^2) -0.365381   0.266724   -1.37  0.17072      
AICc(glmm_SR_Oak) #309.8831

sim<-simulateResiduals(glmm_SR_Oak)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value 0.56741 # Dispersion test 0.872 # Outliers 1 
testOutliers(sim) # RAS

########Species richness oak specialist species with simple effect of tree mixture

glmm_SR_Oak<-glmmTMB(SR_Oak~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
summary(glmm_SR_Oak) 
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
AICc(glmm_SR_Oak) #309.2632

sim<-simulateResiduals(glmm_SR_Oak)
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


#####################################
########RS generalist
################################

descdist(Saprox.Rel.Env$rs.gene,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env$rs.gene,"pois"))
fitnb<-fitdist(Saprox.Rel.Env$rs.gene,"nbinom")
fitp<-fitdist(Saprox.Rel.Env$rs.gene,"pois")
gofstat(fitnb)$chisqpvalue #NULL
gofstat(fitp)$chisqpvalue #0.1310683 

########Species richness oak specialist species with quadratic effect of tree mixture

glmm_SR_Generalist<-glmmTMB(SR_Generalist~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
summary(glmm_SR_Generalist) 

# Family: poisson  ( log )
#Formula:          SR_Generalist ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#142.5     151.8     -66.2     132.5        43 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#stand  (Intercept) 5.71e-10 2.39e-05
#Number of obs: 48, groups:  stand, 21
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)              0.55986    0.44625   1.255    0.210
#G_all_plot              -0.01437    0.01465  -0.981    0.327
#I(mixture_plot/100)      1.90950    1.21758   1.568    0.117
#I((mixture_plot/100)^2) -1.82062    1.16456  -1.563    0.118
AICc(glmm_SR_Generalist)#143.8809

sim<-simulateResiduals(glmm_SR_Generalist)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value 0.09 # Dispersion test 0.024 # Outliers 1 
testOutliers(sim) # RAS

########Species richness oak specialist species with simple effect of tree mixture

glmm_SR_Generalist<-glmmTMB(SR_Generalist~G_all_plot+I(mixture_plot/100)+(1|stand),family=poisson,data=Saprox.Rel.Env.Sp)
summary(glmm_SR_Generalist) 

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
AICc(glmm_SR_Generalist)#143.864

sim<-simulateResiduals(glmm_SR_Generalist)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value 0.16553 # Dispersion test 0.032 # Outliers 1 
testOutliers(sim) # RAS

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


######Species richness oak specialist (category 0)

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

######Species richness oak specialist tolerant to mixing (category 1)

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

######Species richness pine specialist (category 0)

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


######Species richness pine specialist tolerant to mixing (category 1)

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


##############################################################################################
###########################     SAPROX Total abundance    ##################################
##############################################################################################

hist(Saprox.Rel.Sp$Abdce_all) #family NegBin?
mean(Saprox.Rel.Sp$Abdce_all) #170.0833
var(Saprox.Rel.Sp$Abdce_all) #6335.142

descdist(Saprox.Rel.Env$Abdce_all,discrete=TRUE,boot=1001)
plot(fitdist(Saprox.Rel.Env$Abdce_all,"pois"))
fitnb<-fitdist(Saprox.Rel.Env$Abdce_all,"nbinom")
fitp<-fitdist(Saprox.Rel.Env$Abdce_all,"pois")
gofstat(fitnb)$chisqpvalue #0.1367534 --> NegBin
gofstat(fitp)$chisqpvalue #0


########Abundance all species with quadratic effect of tree mixture

glmm_Abdce_all<-glmmTMB(Abdce_all~G_all_plot+I(mixture_plot/100)+I((mixture_plot/100)^2)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
summary(glmm_Abdce_all) 
#Family: nbinom1  ( log )
#Formula:          Abdce_all ~ G_all_plot + I(mixture_plot/100) + I((mixture_plot/100)^2) +      (1 | stand)
#Data: Saprox.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#554.3     565.6    -271.2     542.3        42 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#stand  (Intercept) 3.377e-09 5.811e-05
#Number of obs: 48, groups:  stand, 21
#Dispersion parameter for nbinom1 family (): 30.8 
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              5.311625   0.233254  22.772   <2e-16 ***
#  G_all_plot              -0.010679   0.007649  -1.396    0.163    
#I(mixture_plot/100)      0.593990   0.634091   0.937    0.349    
#I((mixture_plot/100)^2) -0.542841   0.595839  -0.911    0.362    
AICc(glmm_Abdce_all)#556.3937

########Abundance all species with simple effect of tree mixture

glmm_Abdce_all<-glmmTMB(Abdce_all~G_all_plot+I(mixture_plot/100)+(1|stand),family=nbinom1(),data=Saprox.Rel.Env.Sp)
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
AICc(glmm_Abdce_all)#554.598

sim<-simulateResiduals(glmm_Abdce_all)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value 0.99838 # Dispersion test 0.504  # Outliers p=1 
testOutliers(sim) #p=1

# Create prediction grid
mixture_seq <- seq(min(Saprox.Rel.Env.Sp$mixture_plot), max(Saprox.Rel.Env.Sp$mixture_plot), length.out = 200)
G_all_moy<-mean(Saprox.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_all,
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


