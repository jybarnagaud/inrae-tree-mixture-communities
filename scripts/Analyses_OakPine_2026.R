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
Bird.Rel.Env.Sp$cat_mel_plot<- factor(Bird.Rel.Env.Sp$cat_mel_plot, levels = c("Pine", "Mixed", "Oak"))

####################################################################################################################################
#######BIRDS - GLMM
####################################################################################################################################

#### BIRDS - SR all species with quadratic effect of tree mixture

glmm_SR_all<-glmmTMB(SR_all~G_all_plot+I(mel_plot/100)+I((mel_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_all) #simple effect, p=0.12, quadratic effect p=0.29, G p=0.31, taux_veg1=0.83, AIC=285.2
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)           2.363951   0.230875  10.239   <2e-16 ***
#  G_all_plot                -0.002286   0.008852  -0.258   0.7962    
#I(mel_plot/100)      1.173185   0.508405   2.308   0.0210 *  
#  I((mel_plot/100)^2) -1.016384   0.608559  -1.670   0.0949 .  
AICc(glmm_SR_all) #[1] 334.2836

# Create prediction grid
mel_seq <- seq(min(Bird.Rel.Env.Sp$mel_plot), max(Bird.Rel.Env.Sp$mel_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_all,
  newdata = data.frame(G_all_plot=G_all_seq,mel_plot = mel_seq,plot=plot_seq),type="response",re.form=NA,
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
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mel_plot, y = SR_all), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Number of bird species (all)") +
  theme_minimal()

#### BIRDS - SR all species with simple effect of tree mixture

glmm_SR_all<-glmmTMB(SR_all~G_all+I(MEL_point/100)+(1|plot),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_all) #simple effect, p=0.0377, AIC=284.3
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       2.4284617  0.2252669  10.780  < 2e-16 ***
#  G_all            -0.0009063  0.0087510  -0.104  0.91751    
#I(MEL_point/100)  0.3535493  0.1354959   2.609  0.00907 ** 
AICc(glmm_SR_all) #334.7857


#### BIRDS - SR Generalist species with quadratic effect of tree mixture

glmm_SR_Generalist<-glmmTMB(SR_Generalist~G_all+I(MEL_point/100)+I((MEL_point/100)^2)+(1|plot),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Generalist)
#Family: poisson  ( log )
#Formula:          SR_Generalist ~ G_all + I(MEL_point/100) + I((MEL_point/100)^2) +      (1 | plot)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#312.1     323.0    -151.0     302.1        61 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#plot   (Intercept) 7.993e-11 8.941e-06
#Number of obs: 66, groups:  plot, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)           1.968231   0.265269   7.420 1.17e-13 ***
#  G_all                 0.003174   0.010141   0.313   0.7543    
#I(MEL_point/100)      1.095891   0.582534   1.881   0.0599 .  
#I((MEL_point/100)^2) -0.969737   0.697061  -1.391   0.1642    
AICc(glmm_SR_Generalist) #313.0802

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

glmm_SR_Generalist<-glmmTMB(SR_Generalist~G_all+I(MEL_point/100)+(1|plot),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Generalist)
#Family: poisson  ( log )
#Formula:          SR_Generalist ~ G_all + I(MEL_point/100) + (1 | plot)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#312.1     320.8    -152.0     304.1        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#plot   (Intercept) 1.21e-10 1.1e-05 
#Number of obs: 66, groups:  plot, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      2.030701   0.258865   7.845 4.34e-15 ***
#  G_all            0.004452   0.010031   0.444   0.6572    
#I(MEL_point/100) 0.313817   0.155739   2.015   0.0439 *  
AICc(glmm_SR_Generalist) #312.7092

#### BIRDS - SR Oak species with quadratic effect of tree mixture

glmm_SR_Oak<-glmmTMB(SR_Oak~G_all+I(MEL_point/100)+I((MEL_point/100)^2)+(1|plot),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Oak)
#Family: poisson  ( log )
#Formula:          SR_Oak ~ G_all + I(MEL_point/100) + I((MEL_point/100)^2) + (1 |      plot)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#195.4     206.4     -92.7     185.4        61 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#plot   (Intercept) 7.949e-10 2.819e-05
#Number of obs: 66, groups:  plot, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          -1.122967   0.722631  -1.554  0.12018    
#G_all                 0.008989   0.024311   0.370  0.71158    
#I(MEL_point/100)      5.464396   1.654838   3.302  0.00096 ***
#I((MEL_point/100)^2) -4.121930   1.775338  -2.322  0.02025 *  
AICc(glmm_SR_Oak) #196.4205

# Create prediction grid
mel_seq <- seq(min(Bird.Rel.Env.Sp$MEL_point), max(Bird.Rel.Env.Sp$MEL_point), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Oak,
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
  geom_point(data = Bird.Rel.Env.Sp, aes(x = MEL_point, y = SR_Oak), color = "black") +
  labs(title = "GLMM: SR_Oak ~ G + MEL +MEL2 with 95% CI", x = "Deciduous basal area (%)", y = "Number of oak-preferring bird species") +
  theme_minimal()

#### BIRDS - SR Oak species with simple effect of tree mixture

glmm_SR_Oak<-glmmTMB(SR_Oak~G_all+I(MEL_point/100)+(1|plot),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Oak)
#Family: poisson  ( log )
#Formula:          SR_Oak ~ G_all + I(MEL_point/100) + (1 | plot)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#199.2     207.9     -95.6     191.2        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance Std.Dev.
#plot   (Intercept) 0.02988  0.1729  
#Number of obs: 66, groups:  plot, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -0.538602   0.694440  -0.776    0.438    
#G_all             0.008475   0.025970   0.326    0.744    
#I(MEL_point/100)  1.800483   0.427141   4.215  2.5e-05 ***
AICc(glmm_SR_Oak) #199.8354

#### BIRDS - SR Pine species with quadratic effect of tree mixture

glmm_SR_Pine<-glmmTMB(SR_Pine~G_all+I(MEL_point/100)+I((MEL_point/100)^2)+(1|plot),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Pine)
#Family: poisson  ( log )
#Formula:          SR_Pine ~ G_all + I(MEL_point/100) + I((MEL_point/100)^2) + (1 |      plot)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#193.0     204.0     -91.5     183.0        61 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#plot   (Intercept) 7.476e-10 2.734e-05
#Number of obs: 66, groups:  plot, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)           1.88874    0.66900   2.823  0.00475 **
#  G_all                -0.04864    0.02794  -1.741  0.08171 . 
#I(MEL_point/100)     -0.65622    1.58682  -0.414  0.67921   
#I((MEL_point/100)^2) -0.38772    2.07503  -0.187  0.85178   
AICc(glmm_SR_Pine) #194.0265

#### BIRDS - SR Oak species with simple effect of tree mixture

glmm_SR_Pine<-glmmTMB(SR_Pine~G_all+I(MEL_point/100)+(1|plot),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_SR_Pine)
#Family: poisson  ( log )
#Formula:          SR_Pine ~ G_all + I(MEL_point/100) + (1 | plot)
#Data: Bird.Rel.Env.Sp
#AIC       BIC    logLik -2*log(L)  df.resid 
#191.1     199.8     -91.5     183.1        62 
#Random effects:
#  Conditional model:
#  Groups Name        Variance  Std.Dev. 
#plot   (Intercept) 4.212e-10 2.052e-05
#Number of obs: 66, groups:  plot, 22
#Conditional model:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)       1.88850    0.66771   2.828  0.00468 **
#  G_all            -0.04753    0.02725  -1.744  0.08112 . 
#I(MEL_point/100) -0.94310    0.40902  -2.306  0.02112 * 
AICc(glmm_SR_Pine) #191.7175

#Create prediction grid
mel_seq <- seq(min(Bird.Rel.Env.Sp$MEL_point), max(Bird.Rel.Env.Sp$MEL_point), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_SR_Pine,
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
  geom_point(data = Bird.Rel.Env.Sp, aes(x = MEL_point, y = SR_Pine), color = "black") +
  labs(title = "GLMM: SR_Pine ~ G + MEL with 95% CI", x = "Deciduous basal area (%)", y = "Number of pine-preferring bird species") +
  theme_minimal()


##################################################################################################
########################   BIRD Abundance
##################################################################################################

glmm_Abdce_all<-glmmTMB(Abdce_all~G_all_plot+I(mel_plot/100)+I((mel_plot/100)^2)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_all)
#(Intercept)           2.7729872  0.1855616  14.944   <2e-16 ***
#  G_all_plot                -0.0004843  0.0071010  -0.068   0.9456    
#I(mel_plot/100)      0.9198519  0.4061184   2.265   0.0235 *  
#  I((mel_plot/100)^2) -0.6660573  0.4838513  -1.377   0.1686  
AICc(glmm_Abdce_all) #388.9731

glmm_Abdce_all<-glmmTMB(Abdce_all~G_all_plot+I(mel_plot/100)+(1|stand),family=poisson,data=Bird.Rel.Env.Sp)
summary(glmm_Abdce_all)#G coeff=0.06 p=0.03, MEL coeff=0.48 p=0.0291, AIC 539.7
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      2.8173088  0.1814950  15.523  < 2e-16 ***
#  G_all_plot            0.0003549  0.0070422   0.050 0.959803    
#I(mel_point/100) 0.3808931  0.1092179   3.487 0.000488 ***
AICc(glmm_Abdce_all) #388.5486

# Create prediction grid
mel_seq <- seq(min(Bird.Rel.Env.Sp$mel_plot), max(Bird.Rel.Env.Sp$mel_plot), length.out = 200)
G_all_moy<-mean(Bird.Rel.Env.Sp$G_all_plot)
G_all_seq<-rep(G_all_moy,200)
plot_seq<-rep(245,200)
pred <- predict(
  glmm_Abdce_all,
  newdata = data.frame(G_all_plot=G_all_seq,mel_plot = mel_seq,plot=plot_seq),type="response",re.form=NA,
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
  geom_point(data = Bird.Rel.Env.Sp, aes(x = mel_plot, y = Abdce_all), color = "black") +
  labs(x = "Mixture (% oak vs pine+oak)", y = "Abundance of bird species (all)") +
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

#linear positive effect of tree mixture on generalist species richness



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


