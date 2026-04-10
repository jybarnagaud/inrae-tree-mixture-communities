# GLMM
#selection de la loi de proba la mieux ajustée à la distribution des données
library(fitdistrplus)
# sélection de la loi d'ajustement : poisson, gaussien, negative binomiale ou lognormal
descdist(Rscolytexoticfemer$rs.scol.non.ambrosia,discrete=TRUE,boot=1001)
plot(fitdist(Rscolytexoticfemer$rs.scol.non.ambrosia,"norm"))
fitnb<-fitdist(Rscolytexoticfemer$rs.scol.non.ambrosia,"nbinom")
fitp<-fitdist(Rscolytexoticfemer$rs.scol.non.ambrosia,"pois")
fitn<-fitdist(Rscolytexoticflemer$rs.scol.non.ambrosia,"norm")
fitlnorm<-fitdist(Rscolytexoticfemer$rs.scol.non.ambrosia,"lnorm")
fitexp<-fitdist(Rscolytexoticfemer$rs.scol.non.ambrosia,"exp")
fitgeom<-fitdist(Rscolytexoticfemer$rs.scol.non.ambrosia,"geom")
fitbeta<-fitdist(Rscolytexoticfemer$rs.scol.non.ambrosia,"beta")
fitunif<-fitdist(Rscolytexoticfemer$rs.scol.non.ambrosia,"unif")
fitg<-fitdist(Rscolytexoticfemer$rs.scol.non.ambrosia,"gamma",method="mme")
gofstat(fitnb)$chisqpvalue
gofstat(fitp)$chisqpvalue
gofstat(fitn)$chisqpvalue
gofstat(fitlnorm)$chisqpvalue
gofstat(fitexp)$chisqpvalue
gofstat(fitgeom)$chisqpvalue
gofstat(fitunif)$chisqpvalue
gofstat(fibeta)$chisqpvalue
gofstat(fitg)$chisqpvalue
#après log+1 transformation de la variable
descdist(log1p(Rscolytexoticfemer$rs.scol.non.ambrosia),discrete=TRUE,boot=1001)
plot(fitdist(log1p(Rscolytexoticfemer$rs.scol.non.ambrosia),"norm"))
fitn<-fitdist(log1p(Rscolytexoticfemer$rs.scol.non.ambrosia),"norm")
gofstat(fitn)$chisqpvalue
# la loi la mieux ajustée correspond à la p-value la plus élevée

#addition de l'autocorrélation spatiale
#Avec ACS
pos<-numFactor((as.numeric(Rscolytexoticfcircul$longitude)),(as.numeric(Rscolytexoticfcircul$latitude)))#va faire des longitudes /latitudes un couple
ID <- factor(rep(1, 3887))#une placette = un identifiant
t3<-Sys.time()
model3<-glmmTMB(rs.scol.non.ambrosia~occur.germanus+(1|cluster)+(1|idtrapyear)+exp(pos+0|ID),family="nbinom1",data=Rscolytexoticfcircul,na.action=na.fail)

# #magnitude
#récupérer les valeurs de Estimate et Std. Error pour la variable d’intérêt X
Estimate_mod<-rnorm(10000,mean=0.3200,sd=0.1480)
DX<-exp(Estimate_mod*10)-1 #pour un delta de X de 10 (%)
mean(DX)
quantile(DX, c(0.01, 0.99))

#diagnostic glmm : ajustement, surdispersion, outliers
library(DHARMa)
sim<-simulateResiduals(model3)
testUniformity(sim)#le qqplot est issu de cette commande 
#KS Test p-value # Dispersion test  # Outliers 
testOutliers(sim)

