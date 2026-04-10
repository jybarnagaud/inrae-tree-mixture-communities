    library(lme4)
    library(nlme)
    library(multcomp)
    options(constrasts=c("contr.treatment","contr.poly"))
    #setwd("P:/Emmanuelle/MelangeEss_FOrl?ans/Analyses/These_JYB_2011")
    #setwd("Z:/projets/MelangeEss_FOrleans/TheseJYB/Analyses\These_JYB_2011")
    setwd("C:/Users/farchaux/Documents/OakPine")
    
    #attention au sens des barres dans les chemins fichiers : / et non \ 
    #attach("V:/ANIMATION_SCIENTIFIQUE/stats/R/repertoire_commun/.RData", pos = 2, name = FG)
    
    
    Carab.Ptrap.per.list<-read.csv("Carab_MelChPin_FOrleans_2009_v04.csv")
    attach(Carab.Ptrap.per.list)
    fix(Carab.Ptrap.per.list)
    str(Carab.Ptrap.per.list)
    dim(Carab.Ptrap.per.list)
    dimnames(Carab.Ptrap.per.list)
    
        
    ListeSp<-read.csv("ListeSp_carab_2011_OPTMix_v00.csv", sep=";", header=T)
    fix( ListeSp)
    str( ListeSp)
    levels(ListeSp$code_sp)
    ListeSp$code_sp
    dim(ListeSp)
    #ListeSp <- ListeSp[1:150,] 
    #dim(ListeSp)
    dimnames(ListeSp)[[1]] <- ListeSp$code_sp 
    fix( ListeSp)
    dimnames(ListeSp)
  
    Ptrap.per<-read.csv("Pieges_MelEssChPin_OPTMix_v01.csv", sep=";", header=T)
    fix(Ptrap.per)
    str(Ptrap.per)
    dim(Ptrap.per)
    dimnames(Ptrap.per)[[1]] <- Ptrap.per$plot_Ptrap_per 
    fix(Ptrap.per)
    dimnames(Ptrap.per)

    Envt.Ptrap<-read.csv("PlanEchantillonnage_carab_OPTMix_MelChPin_2009.csv", sep=";", header=T)
    attach(Envt.Ptrap)
    fix(Envt.Ptrap)
    str(Envt.Ptrap)
    dim(Envt.Ptrap)
    dimnames(Envt.Ptrap)[[1]] <- Envt.Ptrap$plot_Ptrap
    fix(Envt.Ptrap)
    dimnames(Ptrap.per)


#----------------------------------------------------------------------------------------------------------------
    
 # Creation bilans et tableaux sp/releves 
 
   # Elimination des pi?ges perturb?s 
# ?l?ments de de construction mais qui ne permettent pas d'extraire plus de lignes que de pi?ges
#Ptrap.per[Ptrap.per$selection==1, "plot_Ptrap_per"]
#as.character(Ptrap.per[Ptrap.per$selection==1,"plot_Ptrap_per"])
length(as.character(Ptrap.per[Ptrap.per$selection==1,"plot_Ptrap_per"]))
# commande qui ne marche pas car pas bon nombre de lignes, donc pas de correspondance ad?quate entre le tableau pi?ges et le tableau carab : "Carab.Ptrap.per.list["plot_Ptrap_per"== as.character(Ptrap.per[Ptrap.per$selection==1,"plot_Ptrap_per"]),]"
# la commande pour multiplier les lignes du tableau pi?ges selon le tableau carab est la suivante :
as.character(Ptrap.per[as.character(Carab.Ptrap.per.list$plot_Ptrap_per),"selection"])

Carab.Ptrap.per.list[as.character(Ptrap.per[as.character(Carab.Ptrap.per.list$plot_Ptrap_per),"selection"])==1, "plot_Ptrap_per"]

Carab.Ptrap.per.list2 <- Carab.Ptrap.per.list[as.character(Ptrap.per[as.character(Carab.Ptrap.per.list$plot_Ptrap_per),"selection"])==1, ]
fix(Carab.Ptrap.per.list2)
str(Carab.Ptrap.per.list2)
dim(Carab.Ptrap.per.list2)
dimnames(Carab.Ptrap.per.list2)
Carab.Ptrap.per.list <- Carab.Ptrap.per.list2

    
   # Tableau crois? niv ?chantillon : abondance totale par esp?ce et par Plot_Ptrap_Period
Nt.Ptrap.per.sp<-tapply(Carab.Ptrap.per.list$Nombre, list(as.character(Carab.Ptrap.per.list$plot_Ptrap_per), as.character(Carab.Ptrap.per.list$code_sp)), sum)
# NB variante : tapply avec la fonction identit? (I)
Nt.Ptrap.per.sp
#remplacement des NA par des zeros
Nt.Ptrap.per.sp[is.na(Nt.Ptrap.per.sp)]<-0
Nt.Ptrap.per.sp
Nt.Ptrap.per.sp<-data.frame(Nt.Ptrap.per.sp)
fix(Nt.Ptrap.per.sp) 
str(Nt.Ptrap.per.sp)
# supprimmer la colonne avec le code_sp "0" , correspondant aux pi?ges vides
Nt.Ptrap.per.sp<-data.frame(Nt.Ptrap.per.sp[,-1])
str(Nt.Ptrap.per.sp)
dimnames(Nt.Ptrap.per.sp)

write.csv(Nt.Ptrap.per.sp, file = "Nt.Ptrap.per.sp.csv")

# NB : Penser ? d?selectionner les ?chantillons avec des codes "?" (transform? en X.) ou "indet"
    
# Cr?ation intitul?s ?chantillonnage dans Nt.Ptrap.per.sp
  # d?concat?nation des champs plot-Ptrap-periode ds tableau Nt.Ptrap.per.sp 
toto<-strsplit(rownames(Nt.Ptrap.per.sp), split="_")
toto
# toto est une liste => transformation de toto liste en toto tableau (cf Crawley, 2007, p 43 et 46)
      #variante un peu moins ?l?gante (correspond aux ?tapes d?taill?es du sapply)
      #toto<-unlist(toto)
      #toto
      ##toto est devenu un vecteur, avec toutes les valeurs ? la suite. utiliser dim(toto) pour lui redonner sa structure en tableau (cf Crawley, 2007, p41)
      #dim(toto)<-c(3,486)
      ##NB : le vecteur toto remplit d'abord les colonnes de toto : dim(toto)<-c(486,3) ne s?pare pas l'info comme il faut.
      #toto
      #toto<-t(toto)
# M?thode avec sapply : ATTENTION, adapter la commande au nombre de lignes de Nt.Ptrap.per.sp (= nb d'?chantillons) : ici =486 (cf Crawley, 2007, p 43 et 46) 
dim(Nt.Ptrap.per.sp)
sapply(1:486,function(i) toto[[i]])
toto<-t(sapply(1:486,function(i) toto[[i]]))
toto
dim(toto)
toto<-as.data.frame(toto)
dimnames(toto)
dimnames(toto)[[1]]<-dimnames(Nt.Ptrap.per.sp)[[1]]
dimnames(toto)[[1]]
dimnames(toto)[[2]]<-c("plot","Ptrap","period")
dimnames(toto)[[2]]
fix(toto)
str(toto)
Nt.Ptrap.per.sp$index<-dimnames(Nt.Ptrap.per.sp)[[1]]
fix(Nt.Ptrap.per.sp)
str(Nt.Ptrap.per.sp)
cbind(Nt.Ptrap.per.sp, toto[dimnames(Nt.Ptrap.per.sp)[[1]],])
Nt.Ptrap.per.sp <- cbind(Nt.Ptrap.per.sp, toto[dimnames(Nt.Ptrap.per.sp)[[1]],])
fix(Nt.Ptrap.per.sp)
str(Nt.Ptrap.per.sp)
write.csv(Nt.Ptrap.per.sp, file = "Nt.Ptrap.per.sp.csv")

  # cr?ation des colonnes "plot_Ptrap"  et "Ptrap_period"
Nt.Ptrap.per.sp$plot_Ptrap <- paste(Nt.Ptrap.per.sp$plot,Nt.Ptrap.per.sp$Ptrap, sep="_")
Nt.Ptrap.per.sp$Ptrap_per <- paste(Nt.Ptrap.per.sp$Ptrap, Nt.Ptrap.per.sp$period,sep="_")   
Nt.Ptrap.per.sp$plot_per <- paste(Nt.Ptrap.per.sp$plot,Nt.Ptrap.per.sp$period, sep="_")
str(Nt.Ptrap.per.sp)

# calcul du nombre de p?riodes d'?chantillonnage par pi?ge (plot_Ptrap), p?riode (plot_per) et placette (plot) ds tableau Nt.Ptrap.per.sp 
  # Nombre de p?riodes par pi?ge (dans placette)
Nper_plot.Ptrap <- as.data.frame.table(tapply(1:486,as.factor(Nt.Ptrap.per.sp$plot_Ptrap),function(x) NROW(x)))
Nper_plot.Ptrap
dim(Nper_plot.Ptrap)
dimnames(Nper_plot.Ptrap)
dimnames(Nper_plot.Ptrap)[[2]] <- c("plot_Ptrap","Nper_plot.Ptrap")
dimnames(Nper_plot.Ptrap)[[1]] <- Nper_plot.Ptrap$plot_Ptrap
str(Nper_plot.Ptrap)
Nper_plot.Ptrap

  # Nombre de pi?ges par p?riode (dans placette)
NPtrap_plot.per <- as.data.frame.table(tapply(1:486,as.factor(Nt.Ptrap.per.sp$plot_per),function(x) NROW(x)))
NPtrap_plot.per
dim(NPtrap_plot.per)
dimnames(NPtrap_plot.per)
dimnames(NPtrap_plot.per)[[2]] <- c("plot_per", "NPtrap_plot.per")
dimnames(NPtrap_plot.per)[[1]]  <-  NPtrap_plot.per$plot_per
str(NPtrap_plot.per)
NPtrap_plot.per

  # Nombre d'?chantillons (pi?ge x p?riode) par placette
NPtrap.per_plot <- as.data.frame.table(tapply(1:486,as.factor(Nt.Ptrap.per.sp$plot),function(x) NROW(x)))
NPtrap.per_plot 
dim(NPtrap.per_plot)
dimnames(NPtrap.per_plot)
dimnames(NPtrap.per_plot)[[2]] <- c("plot","NPtrap.per_plot")
dimnames(NPtrap.per_plot)[[1]] <- NPtrap.per_plot$plot
str(NPtrap.per_plot)
NPtrap.per_plot

# Tableau crois? niv pi?ge dans placette (cumul p?riodes/pi?ge): abondance totale par esp?ce et par Plot_Ptrap
        # m?thode originale avec tapply et fonction sur vecteur index
        #tapply(1:486, as.factor(Nt.Ptrap.per.sp$plot_Ptrap), function(x,y){if (length(x)>1){colSums(y[x,])} else {I(y[x,])}},as.matrix(Nt.Ptrap.per.sp[,1:70]))
        #Nt.Ptrap.sp <- t(sapply(tapply(1:471, as.factor(Nt.Ptrap.per.sp$plot_Ptrap), function(x,y){if (length(x)>1){colSums(y[x,])} else {I(y[x,])}},as.matrix(Nt.Ptrap.per.sp[,1:70])), I))
# variante simple synth?tique sous R : 
dimnames(Nt.Ptrap.per.sp)[[2]] 
# ATTENTION : pour savoir quelles colonnes sp s?lectionner, ici ne s?lectionner que les 36 premi?res col correspondant aux sp
rowsum(as.matrix(Nt.Ptrap.per.sp[,1:36]), as.factor(Nt.Ptrap.per.sp$plot_Ptrap))
dimnames(rowsum(as.matrix(Nt.Ptrap.per.sp[,1:36]), as.factor(Nt.Ptrap.per.sp$plot_Ptrap)))
dim(rowsum(as.matrix(Nt.Ptrap.per.sp[,1:36]), as.factor(Nt.Ptrap.per.sp$plot_Ptrap)))
Nt.Ptrap.sp <-   as.data.frame(rowsum(as.matrix(Nt.Ptrap.per.sp[,1:36]), as.factor(Nt.Ptrap.per.sp$plot_Ptrap)))
dim(Nt.Ptrap.sp)
dimnames(Nt.Ptrap.sp)
fix(Nt.Ptrap.sp)
str(Nt.Ptrap.sp)
write.csv(Nt.Ptrap.sp, file = "Nt.Ptrap.sp.csv")


# Cr?ation intitul?s ?chantillonnage dans Nt.Ptrap.sp
    # d?concat?nation des champs plot_Ptrap ds tableau Nt.Ptrap.sp et incorporation directe dans Nt.Ptrap.sp

Nt.Ptrap.sp$index <- dimnames(Nt.Ptrap.sp)[[1]]
toto <- strsplit(rownames(Nt.Ptrap.sp), split="_" )
toto
# attention adapter la commande au nombre de lignes de Nt.Ptrap.sp (= nb de pi?ges * nb placettes) : ici = 100 
Nt.Ptrap.sp[, c("plot", "Ptrap")] <- t(sapply(1:100,function(i) toto[[i]]))
Nt.Ptrap.sp$Ptrap
Nt.Ptrap.sp$plot
Nt.Ptrap.sp$Nper_plot.Ptrap <- as.numeric(Nper_plot.Ptrap[dimnames(Nt.Ptrap.sp)[[1]],"Nper_plot.Ptrap"])
str(Nt.Ptrap.sp)
fix(Nt.Ptrap.sp)
write.csv(Nt.Ptrap.sp, file = "Nt.Ptrap.sp.csv")
 

#------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
# ATTENTION, ? partir de l?, le script n'est plus mis ? jour


# identit? des relev?s pris en compte ??
    
# Tableau crois? niv placette (cumul p?riodes et pi?ge): abondance totale par esp?ce et par Plot    
    
 
#-----------------------------------------------------------------------------------------------------

# CALCUL RS par PIEGE (dans PLACETTE)

# D?pliage du tableau crois? sp/plot_Ptrap (Nt.Ptrap.sp) en tableau liste r?sum? (somme des p?riodes) au niveau plot_Ptrap, sur les colonnes sp (ici = 31 sp) 

Nt.Ptrap.sp.list <- unlist.lme(x=Nt.Ptrap.sp[,1:36])
str(Nt.Ptrap.sp.list)
fix(Nt.Ptrap.sp.list)   
dimnames(Nt.Ptrap.sp.list)
dimnames(Nt.Ptrap.sp.list) [[2]] <- c("Nt", "sp", "plot_Ptrap") 
dimnames(Nt.Ptrap.sp.list) [[2]]
dim(Nt.Ptrap.sp.list)
str(Nt.Ptrap.sp.list)
fix(Nt.Ptrap.sp.list)   



# Descripteurs bdv au niveau pi?ge (plot_Ptrap, bas? sur somme des p?riodes par pi?ge) : 

 # Abondance tot
Ntot.Ptrap<-as.data.frame.table(tapply(Nt.Ptrap.sp.list$Nt, as.character(Nt.Ptrap.sp.list$plot_Ptrap), sum))
Ntot.Ptrap
dimnames(Ntot.Ptrap)[[2]]<-c("plot_Ptrap", "Ntot")
dimnames(Ntot.Ptrap)[[1]] <- Ntot.Ptrap$plot_Ptrap
Ntot.Ptrap
dim(Ntot.Ptrap)
dimnames(Ntot.Ptrap)
fix(Ntot.Ptrap) 
 
 # RS tot
Nt.Ptrap.sp.list$Nt>0
sum(Nt.Ptrap.sp.list$Nt>0)
Stot.Ptrap<-as.data.frame.table(tapply(Nt.Ptrap.sp.list$Nt>0, as.character(Nt.Ptrap.sp.list$plot_Ptrap), sum))
Stot.Ptrap
dimnames(Stot.Ptrap)[[2]]<-c("plot_Ptrap", "Stot")
dimnames(Stot.Ptrap)[[1]] <- Stot.Ptrap$plot_Ptrap
dimnames(Stot.Ptrap)
fix(Stot.Ptrap)

            # vecteur groupe ?cologique associ? au tableau faunistique liste

as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),c("code_sp", "closeness.1"])
            # hab.pref: closeness.1
                  # abondance totale par groupe  closeness.1
Ntot.Ptrap.closeness1<-as.data.frame(tapply(Nt.Ptrap.sp.list$Nt, list(as.character(Nt.Ptrap.sp.list$plot_Ptrap), as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),"closeness.1"])), sum) )
dim(Ntot.Ptrap.closeness1)
dimnames(Ntot.Ptrap.closeness1)
dimnames(Ntot.Ptrap.closeness1)[[2]]<-c("Nt.unknown.closeness", "Nt.generalist", "Nt.openland", "Nt.woodland" )
dimnames(Ntot.Ptrap.closeness1)[[2]]
Ntot.Ptrap.closeness1 
fix(Ntot.Ptrap.closeness1)              
                  
                  # richesse totale par groupe  closeness.1
Nt.Ptrap.sp.list$Nt>0
sum(Nt.Ptrap.sp.list$Nt>0)
as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),"closeness.1"])
tapply(Nt.Ptrap.sp.list$Nt>0, list(as.character(Nt.Ptrap.sp.list$plot_Ptrap), as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),"closeness.1"])), sum)
Stot.Ptrap.closeness1<-as.data.frame(tapply(Nt.Ptrap.sp.list$Nt>0, list(as.character(Nt.Ptrap.sp.list$plot_Ptrap), as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),"closeness.1"])), sum))
dim(Stot.Ptrap.closeness1)
dimnames(Stot.Ptrap.closeness1)
dimnames(Stot.Ptrap.closeness1)[[2]]<-c("St.unknown.closeness", "St.generalist", "St.openland", "St.woodland" )
dimnames(Stot.Ptrap.closeness1)[[2]]
Stot.Ptrap.closeness1
fix(Stot.Ptrap.closeness1)

 # hab.pref: closeness.2
                  # abondance totale par groupe  closeness.2
Ntot.Ptrap.closeness2<-as.data.frame(tapply(Nt.Ptrap.sp.list$Nt, list(as.character(Nt.Ptrap.sp.list$plot_Ptrap), as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),"closeness.2"])), sum))
dim(Ntot.Ptrap.closeness2)
dimnames(Ntot.Ptrap.closeness2)
dimnames(Ntot.Ptrap.closeness2)[[2]]<-c("Nt.unknown.closeness", "Nt.generalist", "Nt.openland", "Nt.woodland.eurytopic", "Nt.woodland.stenotopic" )
dimnames(Ntot.Ptrap.closeness2)[[2]]
Ntot.Ptrap.closeness2
fix(Ntot.Ptrap.closeness2)
                   
                  
                  # richesse totale par groupe  closeness.2
Stot.Ptrap.closeness2<-as.data.frame(tapply(Nt.Ptrap.sp.list$Nt>0, list(as.character(Nt.Ptrap.sp.list$plot_Ptrap), as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),"closeness.2"])), sum))
dim(Stot.Ptrap.closeness2)
dimnames(Stot.Ptrap.closeness2)
dimnames(Stot.Ptrap.closeness2)[[2]]<-c("St.unknown.closeness", "St.generalist", "St.openland", "St.woodland.eurytopic", "St.woodland.stenotopic" )
dimnames(Stot.Ptrap.closeness2)[[2]]
Stot.Ptrap.closeness2
fix(Stot.Ptrap.closeness2)


# wing-developmental-type-desender-etal-2008
                  # abondance totale par groupe  wing-developmental-type-desender-etal-2008
Ntot.Ptrap.wing<-as.data.frame(tapply(Nt.Ptrap.sp.list$Nt, list(as.character(Nt.Ptrap.sp.list$plot_Ptrap), as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),"wing.type"])), sum))
dim(Ntot.Ptrap.wing)
dimnames(Ntot.Ptrap.wing)
dimnames(Ntot.Ptrap.wing)[[2]]<-c("Nt.unknown.wing", "Nt.brachypterous", "Nt.dimorphic", "Nt.macropterous", "Nt.polymorphic" )
dimnames(Ntot.Ptrap.wing)[[2]]
Ntot.Ptrap.wing 
fix(Ntot.Ptrap.wing)              
                  
                  # richesse totale par groupe  wing-developmental-type-desender-etal-2008
Stot.Ptrap.wing<-as.data.frame(tapply(Nt.Ptrap.sp.list$Nt>0, list(as.character(Nt.Ptrap.sp.list$plot_Ptrap), as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),"wing.type"])), sum))
dim(Stot.Ptrap.wing)
dimnames(Stot.Ptrap.wing)
dimnames(Stot.Ptrap.wing)[[2]]<-c("St.unknown.wing",  "St.brachypterous", "St.dimorphic", "St.macropterous", "St.polymorphic" )
dimnames(Stot.Ptrap.wing)[[2]]
Stot.Ptrap.wing
fix(Stot.Ptrap.wing)

# tableau des abd par sp, des abd et richesses par groupe pour chaque plot_Ptrap
Carab.Ptrap<-as.data.frame(cbind(Nt.Ptrap.sp[as.character(row.names(Nt.Ptrap.sp)),], Nper_plot.Ptrap[as.character(row.names(Nt.Ptrap.sp)),], Ntot.Ptrap[as.character(row.names(Nt.Ptrap.sp)),], Stot.Ptrap[as.character(row.names(Nt.Ptrap.sp)),], Ntot.Ptrap.closeness1[as.character(row.names(Nt.Ptrap.sp)),], Stot.Ptrap.closeness1[as.character(row.names(Nt.Ptrap.sp)),], Ntot.Ptrap.closeness2[as.character(row.names(Nt.Ptrap.sp)),], Stot.Ptrap.closeness2[as.character(row.names(Nt.Ptrap.sp)),], Ntot.Ptrap.wing[as.character(row.names(Nt.Ptrap.sp)),], Stot.Ptrap.wing[as.character(row.names(Nt.Ptrap.sp)),]))
str(Carab.Ptrap)
fix(Carab.Ptrap) 
dimnames(Carab.Ptrap)
dim(Carab.Ptrap)

write.csv(Carab.Ptrap, file = "Carab.Ptrap.csv")





#-----------------------------------------------------------------------------------------------------

# Graphiques



#xyplot(Stot ~ MEL_cercle, data=Carab.ChPin.Ptrap)

#sunflowerplot(Carab.ChPin.Ptrap$Stot ~ Carab.ChPin.Ptrap$MEL_cercle, pch = 21,cex=2,cex.lab=1.2, lwd=2, seg.lwd = 2,cex.fact = 2,bty="l",family= "serif", xaxt="n")
#mtext("young poplar", adj= 0.335,side=1,family= "serif",cex=1.2)
#mtext("adult poplar", adj= 0.675,side=1,family= "serif",cex=1.2)
#mtext("adult forest", adj= 1,side=1,family= "serif",cex=1.2)
sunflowerplot(Carab.ChPin.Ptrap$Stot ~ Carab.ChPin.Ptrap$MEL_cercle, xlab = "proportion Ch?ne dans 0.07 ha", ylab= "Nombre d'esp?ces total")
  savePlot(filename = "Stot_MELcercle_sunflower",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)
sunflowerplot(Carab.ChPin.Ptrap$Stot ~ Carab.ChPin.Ptrap$MEL, xlab = "proportion Ch?ne dans 0.85 ha", ylab= "Nombre d'esp?ces total")
  savePlot(filename = "Stot_MEL_sunflower",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)

sunflowerplot(Carab.ChPin.Ptrap$St.woodland ~ Carab.ChPin.Ptrap$MEL_cercle, xlab = "proportion Ch?ne dans 0.07 ha", ylab= "Nombre d'esp?ces foresti?res")
  savePlot(filename = "St.woodland_MELcercle_sunflower",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)
sunflowerplot(Carab.ChPin.Ptrap$St.woodland ~ Carab.ChPin.Ptrap$MEL, xlab = "proportion Ch?ne dans 0.85 ha", ylab= "Nombre d'esp?ces foresti?res")
  savePlot(filename = "St.woodland_MEL_sunflower",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)

sunflowerplot(Carab.ChPin.Ptrap$St.woodland.stenotopic ~ Carab.ChPin.Ptrap$MEL_cercle, xlab = "proportion Ch?ne dans 0.07 ha", ylab= "Nombre d'esp?ces foresti?res sp?cialistes")
  savePlot(filename = "St.woodland.stenotopic_MELcercle_sunflower",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)
sunflowerplot(Carab.ChPin.Ptrap$St.woodland.stenotopic ~ Carab.ChPin.Ptrap$MEL, xlab = "proportion Ch?ne dans 0.85 ha", ylab= "Nombre d'esp?ces foresti?res sp?cialistes")
  savePlot(filename = "St.woodland.stenotopic_MEL_sunflower",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)

sunflowerplot(Carab.ChPin.Ptrap$St.woodland.eurytopic ~ Carab.ChPin.Ptrap$MEL_cercle, xlab = "proportion Ch?ne dans 0.07 ha", ylab= "Nombre d'esp?ces foresti?res g?n?ralistes")
  savePlot(filename = "St.woodland.eurytopic_MELcercle_sunflower",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)
sunflowerplot(Carab.ChPin.Ptrap$St.woodland.eurytopic ~ Carab.ChPin.Ptrap$MEL, xlab = "proportion Ch?ne dans 0.85 ha", ylab= "Nombre d'esp?ces foresti?res g?n?ralistes")
  savePlot(filename = "St.woodland.eurytopic_MEL_sunflower",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)

sunflowerplot(Carab.ChPin.Ptrap$St.generalist ~ Carab.ChPin.Ptrap$MEL_cercle, xlab = "proportion Ch?ne dans 0.07 ha", ylab= "Nombre d'esp?ces g?n?ralistes")
  savePlot(filename = "St.generalist_MELcercle_sunflower",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)
sunflowerplot(Carab.ChPin.Ptrap$St.generalist ~ Carab.ChPin.Ptrap$MEL, xlab = "proportion Ch?ne dans 0.85 ha", ylab= "Nombre d'esp?ces g?n?ralistes")
  savePlot(filename = "St.generalist_MEL_sunflower",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)


sunflowerplot(Carab.ChPin.Ptrap$St.openland ~ Carab.ChPin.Ptrap$MEL_cercle, xlab = "proportion Ch?ne dans 0.07 ha", ylab= "Nombre d'esp?ces de milieux ouverts")
  savePlot(filename = "St.openland_MELcercle_sunflower",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)
sunflowerplot(Carab.ChPin.Ptrap$St.openland ~ Carab.ChPin.Ptrap$MEL, xlab = "proportion Ch?ne dans 0.85 ha", ylab= "Nombre d'esp?ces de milieux ouverts")
  savePlot(filename = "St.openland_MEL_sunflower",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)



#--------------------

sunflowerplot(Carab.ChPin.Ptrap.fullper$Stot ~ Carab.ChPin.Ptrap.fullper$MEL_cercle, xlab = "proportion Ch?ne dans 0.07 ha", ylab= "Nombre d'esp?ces total")
  savePlot(filename = "Stot_MELcercle_sunflower_fullper",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)
sunflowerplot(Carab.ChPin.Ptrap.fullper$Stot ~ Carab.ChPin.Ptrap.fullper$MEL, xlab = "proportion Ch?ne dans 0.85 ha", ylab= "Nombre d'esp?ces total")
  savePlot(filename = "Stot_MEL_sunflower_fullper",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)

sunflowerplot(Carab.ChPin.Ptrap.fullper$St.woodland ~ Carab.ChPin.Ptrap.fullper$MEL_cercle, xlab = "proportion Ch?ne dans 0.07 ha", ylab= "Nombre d'esp?ces foresti?res")
  savePlot(filename = "St.woodland_MELcercle_sunflower_fullper",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)
sunflowerplot(Carab.ChPin.Ptrap.fullper$St.woodland ~ Carab.ChPin.Ptrap.fullper$MEL, xlab = "proportion Ch?ne dans 0.85 ha", ylab= "Nombre d'esp?ces foresti?res")
  savePlot(filename = "St.woodland_MEL_sunflower_fullper",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)

sunflowerplot(Carab.ChPin.Ptrap.fullper$St.woodland.stenotopic ~ Carab.ChPin.Ptrap.fullper$MEL_cercle, xlab = "proportion Ch?ne dans 0.07 ha", ylab= "Nombre d'esp?ces foresti?res sp?cialistes")
  savePlot(filename = "St.woodland.stenotopic_MELcercle_sunflower_fullper",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)
sunflowerplot(Carab.ChPin.Ptrap.fullper$St.woodland.stenotopic ~ Carab.ChPin.Ptrap.fullper$MEL, xlab = "proportion Ch?ne dans 0.85 ha", ylab= "Nombre d'esp?ces foresti?res sp?cialistes")
  savePlot(filename = "St.woodland.stenotopic_MEL_sunflower_fullper",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)

sunflowerplot(Carab.ChPin.Ptrap.fullper$St.woodland.eurytopic ~ Carab.ChPin.Ptrap.fullper$MEL_cercle, xlab = "proportion Ch?ne dans 0.07 ha", ylab= "Nombre d'esp?ces foresti?res g?n?ralistes")
  savePlot(filename = "St.woodland.eurytopic_MELcercle_sunflower_fullper",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)
sunflowerplot(Carab.ChPin.Ptrap.fullper$St.woodland.eurytopic ~ Carab.ChPin.Ptrap.fullper$MEL, xlab = "proportion Ch?ne dans 0.85 ha", ylab= "Nombre d'esp?ces foresti?res g?n?ralistes")
  savePlot(filename = "St.woodland.eurytopic_MEL_sunflower_fullper",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)

sunflowerplot(Carab.ChPin.Ptrap.fullper$St.generalist ~ Carab.ChPin.Ptrap.fullper$MEL_cercle, xlab = "proportion Ch?ne dans 0.07 ha", ylab= "Nombre d'esp?ces g?n?ralistes")
  savePlot(filename = "St.generalist_MELcercle_sunflower_fullper",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)
sunflowerplot(Carab.ChPin.Ptrap.fullper$St.generalist ~ Carab.ChPin.Ptrap.fullper$MEL, xlab = "proportion Ch?ne dans 0.85 ha", ylab= "Nombre d'esp?ces g?n?ralistes")
  savePlot(filename = "St.generalist_MEL_sunflower_fullper",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)


sunflowerplot(Carab.ChPin.Ptrap.fullper$St.openland ~ Carab.ChPin.Ptrap.fullper$MEL_cercle, xlab = "proportion Ch?ne dans 0.07 ha", ylab= "Nombre d'esp?ces de milieux ouverts")
  savePlot(filename = "St.openland_MELcercle_sunflower_fullper",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)
sunflowerplot(Carab.ChPin.Ptrap.fullper$St.openland ~ Carab.ChPin.Ptrap.fullper$MEL, xlab = "proportion Ch?ne dans 0.85 ha", ylab= "Nombre d'esp?ces de milieux ouverts")
  savePlot(filename = "St.openland_MEL_sunflower_fullper",type = c("emf"), device = dev.cur(), restoreConsole = TRUE)


#  ----------------------------------------

kruskal.test(Carab.ChPin.Ptrap$Stot ~ Carab.ChPin.Ptrap$MEL_cercle_cat)
  
kruskal.test(Carab.ChPin.Ptrap$Stot ~ Carab.ChPin.Ptrap$Mel_cat)


kruskal.test(Carab.ChPin.Ptrap.fullper$Stot ~ Carab.ChPin.Ptrap.fullper$MEL_cercle_cat)
  
kruskal.test(Carab.ChPin.Ptrap.fullper$Stot ~ Carab.ChPin.Ptrap.fullper$Mel_cat)


kruskal.test(Carab.ChPin.Ptrap.fullper$St.woodland ~ Carab.ChPin.Ptrap.fullper$MEL_cercle_cat)
  
kruskal.test(Carab.ChPin.Ptrap.fullper$St.woodland ~ Carab.ChPin.Ptrap.fullper$Mel_cat)
  

kruskal.test(Carab.ChPin.Ptrap.fullper$St.woodland.stenotopic ~ Carab.ChPin.Ptrap.fullper$MEL_cercle_cat)
  
kruskal.test(Carab.ChPin.Ptrap.fullper$St.woodland.stenotopic ~ Carab.ChPin.Ptrap.fullper$Mel_cat)


kruskal.test(Carab.ChPin.Ptrap.fullper$St.woodland.eurytopic ~ Carab.ChPin.Ptrap.fullper$MEL_cercle_cat)
  
kruskal.test(Carab.ChPin.Ptrap.fullper$St.woodland.eurytopic ~ Carab.ChPin.Ptrap.fullper$Mel_cat)


kruskal.test(Carab.ChPin.Ptrap.fullper$St.openland ~ Carab.ChPin.Ptrap.fullper$MEL_cercle_cat)
  
kruskal.test(Carab.ChPin.Ptrap.fullper$St.openland ~ Carab.ChPin.Ptrap.fullper$Mel_cat)


kruskal.test(Carab.ChPin.Ptrap.fullper$St.generalist ~ Carab.ChPin.Ptrap.fullper$MEL_cercle_cat)
  
kruskal.test(Carab.ChPin.Ptrap.fullper$St.generalist ~ Carab.ChPin.Ptrap.fullper$Mel_cat)

# -----------------------------------------------------------------------------------------

summary(lm(Carab.ChPin.Ptrap.fullper$Stot ~ Carab.ChPin.Ptrap.fullper$MEL_cercle))
  
summary(lm(Carab.ChPin.Ptrap.fullper$Stot ~ Carab.ChPin.Ptrap.fullper$MEL))


summary(lm(Carab.ChPin.Ptrap.fullper$St.woodland ~ Carab.ChPin.Ptrap.fullper$MEL_cercle))
  
summary(lm(Carab.ChPin.Ptrap.fullper$St.woodland ~ Carab.ChPin.Ptrap.fullper$MEL))
  

summary(lm(Carab.ChPin.Ptrap.fullper$St.woodland.stenotopic ~ Carab.ChPin.Ptrap.fullper$MEL_cercle))
  
summary(lm(Carab.ChPin.Ptrap.fullper$St.woodland.stenotopic ~ Carab.ChPin.Ptrap.fullper$MEL))


summary(lm(Carab.ChPin.Ptrap.fullper$St.woodland.eurytopic ~ Carab.ChPin.Ptrap.fullper$MEL_cercle) )
  
summary(lm(Carab.ChPin.Ptrap.fullper$St.woodland.eurytopic ~ Carab.ChPin.Ptrap.fullper$MEL))


summary(lm(Carab.ChPin.Ptrap.fullper$St.openland ~ Carab.ChPin.Ptrap.fullper$MEL_cercle))
  
summary(lm(Carab.ChPin.Ptrap.fullper$St.openland ~ Carab.ChPin.Ptrap.fullper$MEL))


summary(lm(Carab.ChPin.Ptrap.fullper$St.generalist ~ Carab.ChPin.Ptrap.fullper$MEL_cercle))
  
summary(lm(Carab.ChPin.Ptrap.fullper$St.generalist ~ Carab.ChPin.Ptrap.fullper$MEL))









# ----------------------------------------------------------------------------------------------
# Descripteurs bdv au niveau placette : 
            # RS tot
Stot.plot<-as.data.frame(tapply(Nt.Ptrap.sp.list$Nt>0, as.character(Nt.Ptrap.sp.list$plot), sum))
names(Stot.plot)<-c("Stot")
Stot.plot

            # vecteur groupe ?cologique associ? au tableau faunistique liste
as.character( ListeSp[as.character(Carab.Ptrap.per.list$Code_sp),"closeness.1"])
            # hab.pref: closeness.1
                  # abondance totale par groupe  closeness.1
Ntot.plot.closeness1<-tapply(Carab.Ptrap.per.list$Nombre, list(as.character(Carab.Ptrap.per.list$Placette), as.character( ListeSp[as.character(Carab.Ptrap.per.list$Code_sp),"closeness.1"])), sum)            
Ntot.plot.closeness1               
                  
                  # richesse totale par groupe  closeness.1
Nt.Ptrap.sp.list$Nt>0
sum(Nt.Ptrap.sp.list$Nt>0)
as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),"closeness.1"])
tapply(Nt.Ptrap.sp.list$Nt>0, list(as.character(Nt.Ptrap.sp.list$plot), as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),"closeness.1"])), sum)
Stot.plot.closeness1<-as.data.frame(tapply(Nt.Ptrap.sp.list$Nt>0, list(as.character(Nt.Ptrap.sp.list$plot_Ptrap), as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),"closeness.1"])), sum))
Stot.plot.closeness1

 # hab.pref: closeness.2
                  # abondance totale par groupe  closeness.2
Ntot.plot.closeness2<-tapply(Carab.Ptrap.per.list$Nombre, list(as.character(Carab.Ptrap.per.list$Placette), as.character( ListeSp[as.character(Carab.Ptrap.per.list$Code_sp),"closeness.2"])), sum)            
Ntot.plot.closeness2               
                  
                  # richesse totale par groupe  closeness.2
Stot.plot.closeness2<-as.data.frame(tapply(Nt.Ptrap.sp.list$Nt>0, list(as.character(Nt.Ptrap.sp.list$plot), as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),"closeness.2"])), sum))
Stot.plot.closeness2


# wing-developmental-type-desender-etal-2008
                  # abondance totale par groupe  wing-developmental-type-desender-etal-2008
Ntot.plot.wing<-tapply(Carab.Ptrap.per.list$Nombre, list(as.character(Carab.Ptrap.per.list$Placette), as.character( ListeSp[as.character(Carab.Ptrap.per.list$Code_sp),"wing.developmental.type.desender.etal.2008"])), sum)            
Ntot.plot.wing               
                  
                  # richesse totale par groupe  wing-developmental-type-desender-etal-2008
Stot.plot.wing<-as.data.frame(tapply(Nt.Ptrap.sp.list$Nt>0, list(as.character(Nt.Ptrap.sp.list$plot), as.character( ListeSp[as.character(Nt.Ptrap.sp.list$sp),"wing.developmental.type.desender.etal.2008"])), sum))
Stot.plot.wing

# tableau des richesses par groupe pour chaque placette
S.carab.ecol<-data.frame(Points_GNB08_Fbleau[as.character(row.names(Stot.plot)),], Stot.plot, Ntot.plot.closeness1[as.character(row.names(Stot.plot)),], Stot.plot.closeness1[as.character(row.names(Stot.plot)),], Ntot.plot.closeness2[as.character(row.names(Stot.plot)),], Stot.plot.closeness2[as.character(row.names(Stot.plot)),], Ntot.plot.wing[as.character(row.names(Stot.plot)),], Stot.plot.wing[as.character(row.names(Stot.plot)),])
S.carab.ecol 
dim(S.carab.ecol)

write.table(S.carab.ecol, file = "S.carab.ecol.csv")
S.carab.ecol<-read.csv("S.carab.ecol_v00.csv", sep=";", header=T)
attach(S.carab.ecol)
fix(S.carab.ecol)
str(S.carab.ecol)
rownames(S.carab.ecol) <- S.carab.ecol$Plot 
fix(S.carab.ecol)
dim(S.carab.ecol)

# tests sur richesses par groupe selon gestion
S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]
S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]



wilcox.test(Stot~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(Stot~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(Stot~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(Stot~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(Stot~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(Stot~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(N.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(N.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(N.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(N.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(N.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(N.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(N.forest~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(N.forest~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(N.forest~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(N.forest~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(N.forest~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(N.forest~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(N.open.habitat~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(N.open.habitat~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(N.open.habitat~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(N.open.habitat~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(N.open.habitat~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(N.open.habitat~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(S.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(S.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(S.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(S.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(S.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(S.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(S.forest~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(S.forest~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(S.forest~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(S.forest~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(S.forest~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(S.forest~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(S.open.habitat~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(S.open.habitat~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(S.open.habitat~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(S.open.habitat~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(S.open.habitat~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(S.open.habitat~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(N.forest.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(N.forest.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(N.forest.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(N.forest.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(N.forest.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(N.forest.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(N.forest.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(N.forest.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(N.forest.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(N.forest.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(N.forest.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(N.forest.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(N.open.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(N.open.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(N.open.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(N.open.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(N.open.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(N.open.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(N.open.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(N.open.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(N.open.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(N.open.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(N.open.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(N.open.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(S.forest.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(S.forest.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(S.forest.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(S.forest.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(S.forest.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(S.forest.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(S.forest.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(S.forest.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(S.forest.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(S.forest.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(S.forest.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(S.forest.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(S.open.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(S.open.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(S.open.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(S.open.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(S.open.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(S.open.eurytopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(S.open.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(S.open.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(S.open.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(S.open.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(S.open.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(S.open.stenotopic~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(N.brachypter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(N.brachypter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(N.brachypter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(N.brachypter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(N.brachypter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(N.brachypter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(N.dipolymorf~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(N.dipolymorf~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(N.dipolymorf~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(N.dipolymorf~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(N.dipolymorf~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(N.dipolymorf~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(N.macropter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(N.macropter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(N.macropter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(N.macropter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(N.macropter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(N.macropter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(S.brachypter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(S.brachypter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(S.brachypter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(S.brachypter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(S.brachypter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(S.brachypter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(S.dipolymorf~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(S.dipolymorf~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(S.dipolymorf~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(S.dipolymorf~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(S.dipolymorf~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(S.dipolymorf~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))


wilcox.test(S.macropter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,])
wilcox.test(S.macropter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,])

summary(lm(S.macropter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
anova(lm(S.macropter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.flo==1,]))
summary(lm(S.macropter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))
anova(lm(S.macropter~Gestion, data=S.carab.ecol[S.carab.ecol$selec.carab.feuillus==1,]))

# Fait ? partir de l? seulement : 

----------------------------------------------------------
# Descripteurs bdv au niveau piege (RS moyenne par placette, sur chaque occasion de capture : piege * periode) : 

            # RS 
S.Ptrap.per<-as.data.frame(tapply(Carab.Ptrap.per.list$Nombre>0, as.character(Carab.Ptrap.per.list$Plot_Ptrap_Period), sum))
names(S.Ptrap.per)<-c("S")
S.Ptrap.per
dim(S.Ptrap.per)


            # vecteur groupe ?cologique associ? au tableau faunistique liste
as.character( ListeSp[as.character(Carab.Ptrap.per.list$Code_sp),"closeness.1"])
            # hab.pref: closeness.1
                  # abondance par Ptrap.per selon groupe  closeness.1
N.Ptrap.per.closeness1<-tapply(Carab.Ptrap.per.list$Nombre, list(as.character(Carab.Ptrap.per.list$Plot_Ptrap_Period), as.character( ListeSp[as.character(Carab.Ptrap.per.list$Code_sp),"closeness.1"])), sum)            
N.Ptrap.per.closeness1  
dimnames(N.Ptrap.per.closeness1)[[2]]<-c("N.eurytopic", "N.forest", "N.open-habitat")             
N.Ptrap.per.closeness1 
dim(N.Ptrap.per.closeness1)
                  
                  # richesse par Ptrap.per selon groupe  closeness.1
Carab.Ptrap.per.list$Nombre>0
sum(Carab.Ptrap.per.list$Nombre>0)
as.character( ListeSp[as.character(Carab.Ptrap.per.list$Code_sp),"closeness.1"])
tapply(Carab.Ptrap.per.list$Nombre>0, list(as.character(Carab.Ptrap.per.list$Plot_Ptrap_Period), as.character( ListeSp[as.character(Carab.Ptrap.per.list$Code_sp),"closeness.1"])), sum)
S.Ptrap.per.closeness1<-as.data.frame(tapply(Carab.Ptrap.per.list$Nombre>0, list(as.character(Carab.Ptrap.per.list$Plot_Ptrap_Period), as.character( ListeSp[as.character(Carab.Ptrap.per.list$Code_sp),"closeness.1"])), sum))
S.Ptrap.per.closeness1
dimnames(S.Ptrap.per.closeness1)[[2]]<-c("S.eurytopic", "S.forest", "S.open-habitat")        
S.Ptrap.per.closeness1
dim(S.Ptrap.per.closeness1)


 # hab.pref: closeness.2
                  # abondance par Ptrap.per selon groupe  closeness.2
N.Ptrap.per.closeness2<-tapply(Carab.Ptrap.per.list$Nombre, list(as.character(Carab.Ptrap.per.list$Plot_Ptrap_Period), as.character( ListeSp[as.character(Carab.Ptrap.per.list$Code_sp),"closeness.2"])), sum)            
N.Ptrap.per.closeness2          
dimnames(N.Ptrap.per.closeness2)[[2]]<-c("N.eurytopic", "N.forest-eurytopic", "N.forest-stenotopic", "N.open-eurytopic", "N.open-stenotopic")
N.Ptrap.per.closeness2   
dim(N.Ptrap.per.closeness2)
                  
                  # richesse par Ptrap.per selon groupe  closeness.2
S.Ptrap.per.closeness2<-as.data.frame(tapply(Carab.Ptrap.per.list$Nombre>0, list(as.character(Carab.Ptrap.per.list$Plot_Ptrap_Period), as.character( ListeSp[as.character(Carab.Ptrap.per.list$Code_sp),"closeness.2"])), sum))
S.Ptrap.per.closeness2
dimnames(S.Ptrap.per.closeness2)[[2]]<-c("S.eurytopic", "S.forest-eurytopic", "S.forest-stenotopic", "S.open-eurytopic", "S.open-stenotopic")
S.Ptrap.per.closeness2
dim(S.Ptrap.per.closeness2)

# wing-developmental-type-desender-etal-2008
                  # abondance par Ptrap.per selon groupe wing-developmental-type-desender-etal-2008
N.Ptrap.per.wing<-tapply(Carab.Ptrap.per.list$Nombre, list(as.character(Carab.Ptrap.per.list$Plot_Ptrap_Period), as.character( ListeSp[as.character(Carab.Ptrap.per.list$Code_sp),"wing.developmental.type.desender.etal.2008"])), sum)            
N.Ptrap.per.wing     
dimnames(N.Ptrap.per.wing)[[2]]<-c("N.V1", "N.brachypter", "N.dimorf", "N.dimorf?", "N.macropter", "N.polymorf", "N.polymorf?")
N.Ptrap.per.wing               
dim(N.Ptrap.per.wing)
                  
                  # richesse par Ptrap.per selon groupe    wing-developmental-type-desender-etal-2008
S.Ptrap.per.wing<-as.data.frame(tapply(Carab.Ptrap.per.list$Nombre>0, list(as.character(Carab.Ptrap.per.list$Plot_Ptrap_Period), as.character( ListeSp[as.character(Carab.Ptrap.per.list$Code_sp),"wing.developmental.type.desender.etal.2008"])), sum))
S.Ptrap.per.wing
dimnames(S.Ptrap.per.wing)[[2]]<-c("S.V1", "S.brachypter", "S.dimorf", "S.dimorf?", "S.macropter", "S.polymorf", "S.polymorf?")
S.Ptrap.per.wing
dim(S.Ptrap.per.wing)

# tableau des richesses par groupe pour chaque Plot_Ptrap_Period
#S.carab.ecol.Ptrap.per<-data.frame(Pieges_GNB08_Fbleau[as.character(row.names(S.Ptrap.per)),], S.Ptrap.per, N.Ptrap.per.closeness1[as.character(row.names(S.Ptrap.per)),], S.Ptrap.per.closeness1[as.character(row.names(S.Ptrap.per)),], N.Ptrap.per.closeness2[as.character(row.names(S.Ptrap.per)),], S.Ptrap.per.closeness2[as.character(row.names(S.Ptrap.per)),], N.Ptrap.per.wing[as.character(row.names(S.Ptrap.per)),], S.Ptrap.per.wing[as.character(row.names(S.Ptrap.per)),])
#marche pas : S.carab.ecol.Ptrap.per<-data.frame(merge(S.Ptrap.per, N.Ptrap.per.closeness1[as.character(row.names(S.Ptrap.per)),], S.Ptrap.per.closeness1[as.character(row.names(S.Ptrap.per)),], N.Ptrap.per.closeness2[as.character(row.names(S.Ptrap.per)),], S.Ptrap.per.closeness2[as.character(row.names(S.Ptrap.per)),], N.Ptrap.per.wing[as.character(row.names(S.Ptrap.per)),], S.Ptrap.per.wing[as.character(row.names(S.Ptrap.per)),], by="row.names"))
S.carab.ecol.Ptrap.per<-cbind(S.Ptrap.per, N.Ptrap.per.closeness1[as.character(row.names(S.Ptrap.per)),], S.Ptrap.per.closeness1[as.character(row.names(S.Ptrap.per)),], N.Ptrap.per.closeness2[as.character(row.names(S.Ptrap.per)),], S.Ptrap.per.closeness2[as.character(row.names(S.Ptrap.per)),], N.Ptrap.per.wing[as.character(row.names(S.Ptrap.per)),], S.Ptrap.per.wing[as.character(row.names(S.Ptrap.per)),])
S.carab.ecol.Ptrap.per 
dim(S.carab.ecol.Ptrap.per)

# Ajout info pieges

#pb : commande qui garde les lignes pi?ges dupliqu?es autant que de nb d'esp?ces par pi?ge + pas ordonn? selon l'ordre demand? : Carab.Ptrap.per.list[Plot_Ptrap_Period==dimnames(S.carab.ecol.Ptrap.per)[[1]], c(1, 3:7)]
#Carab.Ptrap.per.list[Plot_Ptrap_Period==dimnames(S.carab.ecol.Ptrap.per)[[1]], c(1, 3:7)]
#S.carab.ecol.Ptrap.per<-cbind(S.carab.ecol.Ptrap.per,Carab.Ptrap.per.list

write.csv(S.carab.ecol.Ptrap.per, file = "S.carab.ecol.Ptrap.per.csv")
# sous excel: remplacer les NA, rajouter les infos placettes, etc 
S.carab.ecol.Ptrap.per<-read.csv("S.carab.ecol.Ptrap.per.csv", sep=";", header=T)
attach(S.carab.ecol.Ptrap.per)
fix(S.carab.ecol.Ptrap.per)
str(S.carab.ecol.Ptrap.per)
rownames(S.carab.ecol.Ptrap.per) <- S.carab.ecol.Ptrap.per$Plot_Ptrap_Period 
fix(S.carab.ecol.Ptrap.per)
dim(S.carab.ecol.Ptrap.per)
dimnames(S.carab.ecol.Ptrap.per)



# Richesse moyenne par placette et par groupe
tapply(S.carab.ecol.Ptrap.per$S.forest.stenotopic, S.carab.ecol.Ptrap.per$Placette, mean)
tapply(1:437, as.character(S.carab.ecol.Ptrap.per$Placette), function(x,y){colMeans(y[x,])}, S.carab.ecol.Ptrap.per[,8:38]) 
t(sapply(( tapply(1:437, as.character(S.carab.ecol.Ptrap.per$Placette), function(x,y){unlist(colMeans(y[x,]))}, S.carab.ecol.Ptrap.per[,8:38])),I))
Sm.carab.ecol<- as.data.frame(t(sapply(( tapply(1:437, as.character(S.carab.ecol.Ptrap.per$Placette), function(x,y){unlist(colMeans(y[x,]))}, S.carab.ecol.Ptrap.per[,8:38])),I)))
dim(Sm.carab.ecol)
dimnames(Sm.carab.ecol)
#marche pas :
#Sm.carab.ecol<- as.data.frame( tapply(1:437, as.character(S.carab.ecol.Ptrap.per$Placette), function(x,y){colMeans(y[x,])}, S.carab.ecol.Ptrap.per[,8:38]) )
#Sm.carab.ecol
#dim(Sm.carab.ecol)
#dimnames(Sm.carab.ecol)[[2]]
#dimnames(S.carab.ecol.Ptrap.per[,8:38])[[2]]
#dimnames(Sm.carab.ecol)[[2]]<-dimnames(S.carab.ecol.Ptrap.per[,8:38])[[2]] 
#si echec transformation liste en data.frame, passage par excel, puis Sm.carab.ecol<-as.data.frame(read.csv("Sm.carab.ecol.csv", sep=";", header=T))

Sm.carab.ecol<-data.frame(Points_GNB_Fbleau_Aub_Ventr, Sm.carab.ecol[as.character(row.names(Points_GNB_Fbleau_Aub_Ventr)),])
dim(Sm.carab.ecol)
dimnames(Sm.carab.ecol)
fix(Sm.carab.ecol)
#Il y aune ligne en plus car Fbleau0604 a ?t? supprimm? du jeu de donn?es faunistiques (trop de d?gats)
dimnames(Sm.carab.ecol[-40,])
dim(Sm.carab.ecol[-40,])
Sm.carab.ecol<-Sm.carab.ecol[-40,]

fix(Sm.carab.ecol)
dim(Sm.carab.ecol)
dimnames(Sm.carab.ecol)
str(Sm.carab.ecol)

write.csv(Sm.carab.ecol, file="Sm.carab.ecol.csv")


# tests sur richesses moyennes par placette de chaque groupe selon gestion
is.na(Sm.carab.ecol$Biais.stationnel)
length(is.na(Sm.carab.ecol$Biais.stationnel))
dimnames(Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel),])[[1]]
dim(Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel),])[[1]]

is.na(Sm.carab.ecol$Biais.essence)
length(is.na(Sm.carab.ecol$Biais.essence))
dimnames(Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.essence),])[[1]]
dim(Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.essence),])[[1]]

is.na(Sm.carab.ecol$Sample.YOUNG)
length(is.na(Sm.carab.ecol$Sample.YOUNG))
dimnames(Sm.carab.ecol[is.na(Sm.carab.ecol$Sample.YOUNG),])[[1]]
dim(Sm.carab.ecol[is.na(Sm.carab.ecol$Sample.YOUNG),])[[1]]

dim(Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),])
dim(Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),])


dimnames(Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",])
dim(Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",])

dimnames(Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",])
dim(Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",])

dimnames(Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",])
dim(Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",])


# -------------------------------------------- S --------------------------------------------------------------------
  # tous sites confondus

wilcox.test(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),])
wilcox.test(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),])

summary(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))

summary(lm(S~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))


summary(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))

summary(lm(S~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))


  # Aub-chalm

wilcox.test(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",])
wilcox.test(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",])

summary(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))

summary(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))


  # Fbleau

wilcox.test(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",])
wilcox.test(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",])

summary(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))

summary(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))


  # Ventron

wilcox.test(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",])
wilcox.test(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",])

summary(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))

summary(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))




# -------------------------------------------- S.forest --------------------------------------------------------------------
  # tous sites confondus

wilcox.test(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),])
wilcox.test(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),])

summary(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))

summary(lm(S.forest~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.forest~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))


summary(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))

summary(lm(S.forest~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.forest~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))


  # Aub-chalm

wilcox.test(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",])
wilcox.test(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",])

summary(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))

summary(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))


  # Fbleau

wilcox.test(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",])
wilcox.test(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",])

summary(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))

summary(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))


  # Ventron

wilcox.test(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",])
wilcox.test(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",])

summary(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))

summary(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.forest~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))


# -------------------------------------------- S.eurytopic --------------------------------------------------------------------
  # tous sites confondus

wilcox.test(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),])
wilcox.test(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),])

summary(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))

summary(lm(S.eurytopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.eurytopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))


summary(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))

summary(lm(S.eurytopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.eurytopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))


  # Aub-chalm

wilcox.test(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",])
wilcox.test(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",])

summary(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))

summary(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))


  # Fbleau

wilcox.test(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",])
wilcox.test(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",])

summary(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))

summary(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))


  # Ventron

wilcox.test(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",])
wilcox.test(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",])

summary(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))

summary(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))



# -------------------------------------------- S.open.habitat --------------------------------------------------------------------
  # tous sites confondus

wilcox.test(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),])
wilcox.test(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),])

summary(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))

summary(lm(S.open.habitat~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.open.habitat~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))


summary(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))

summary(lm(S.open.habitat~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.open.habitat~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))


  # Aub-chalm

wilcox.test(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",])
wilcox.test(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",])

summary(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))

summary(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))


  # Fbleau

wilcox.test(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",])
wilcox.test(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",])

summary(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))

summary(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))


  # Ventron

wilcox.test(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",])
wilcox.test(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",])

summary(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))

summary(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.open.habitat~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))




# -------------------------------------------- S.forest.eurytopic --------------------------------------------------------------------
  # tous sites confondus

wilcox.test(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),])
wilcox.test(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),])

summary(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))

summary(lm(S.forest.eurytopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.forest.eurytopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))


summary(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))

summary(lm(S.forest.eurytopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.forest.eurytopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))


  # Aub-chalm

wilcox.test(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",])
wilcox.test(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",])

summary(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))

summary(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))


  # Fbleau

wilcox.test(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",])
wilcox.test(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",])

summary(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))

summary(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))


  # Ventron

wilcox.test(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",])
wilcox.test(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",])

summary(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))

summary(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.forest.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))





# -------------------------------------------- S.forest.stenotopic --------------------------------------------------------------------
  # tous sites confondus

wilcox.test(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),])
wilcox.test(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),])

summary(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))

summary(lm(S.forest.stenotopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.forest.stenotopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))


summary(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))

summary(lm(S.forest.stenotopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.forest.stenotopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))


  # Aub-chalm

wilcox.test(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",])
wilcox.test(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",])

summary(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))

summary(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))


  # Fbleau

wilcox.test(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",])
wilcox.test(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",])

summary(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))

summary(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))


  # Ventron

wilcox.test(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",])
wilcox.test(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",])

summary(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))

summary(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.forest.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))



# -------------------------------------------- S.open.eurytopic --------------------------------------------------------------------
  # tous sites confondus

wilcox.test(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),])
wilcox.test(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),])

summary(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))

summary(lm(S.open.eurytopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.open.eurytopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))


summary(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))

summary(lm(S.open.eurytopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.open.eurytopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))


  # Aub-chalm

wilcox.test(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",])
wilcox.test(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",])

summary(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))

summary(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))


  # Fbleau

wilcox.test(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",])
wilcox.test(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",])

summary(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))

summary(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))


  # Ventron

wilcox.test(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",])
wilcox.test(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",])

summary(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))

summary(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.open.eurytopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))




# -------------------------------------------- S.open.stenotopic --------------------------------------------------------------------
  # tous sites confondus

wilcox.test(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),])
wilcox.test(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),])

summary(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))

summary(lm(S.open.stenotopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.open.stenotopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))


summary(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))

summary(lm(S.open.stenotopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.open.stenotopic~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))


  # Aub-chalm

wilcox.test(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",])
wilcox.test(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",])

summary(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))

summary(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))


  # Fbleau

wilcox.test(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",])
wilcox.test(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",])

summary(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))

summary(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))


  # Ventron

wilcox.test(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",])
wilcox.test(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",])

summary(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))

summary(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.open.stenotopic~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))




# -------------------------------------------- S.brachypter --------------------------------------------------------------------
  # tous sites confondus

wilcox.test(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),])
wilcox.test(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),])

summary(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))

summary(lm(S.brachypter~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.brachypter~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))


summary(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))

summary(lm(S.brachypter~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.brachypter~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))


  # Aub-chalm

wilcox.test(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",])
wilcox.test(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",])

summary(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))

summary(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))


  # Fbleau

wilcox.test(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",])
wilcox.test(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",])

summary(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))

summary(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))


  # Ventron

wilcox.test(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",])
wilcox.test(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",])

summary(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))

summary(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.brachypter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))




# -------------------------------------------- S.di.polymorf --------------------------------------------------------------------
  # tous sites confondus

wilcox.test(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),])
wilcox.test(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),])

summary(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))

summary(lm(S.di.polymorf~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.di.polymorf~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))


summary(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))

summary(lm(S.di.polymorf~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.di.polymorf~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))


  # Aub-chalm

wilcox.test(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",])
wilcox.test(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",])

summary(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))

summary(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))


  # Fbleau

wilcox.test(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",])
wilcox.test(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",])

summary(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))

summary(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))


  # Ventron

wilcox.test(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",])
wilcox.test(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",])

summary(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))

summary(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.di.polymorf~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))




# -------------------------------------------- S.macropter --------------------------------------------------------------------
  # tous sites confondus

wilcox.test(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),])
wilcox.test(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),])

summary(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))

summary(lm(S.macropter~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))
anova(lm(S.macropter~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence),]))


summary(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))

summary(lm(S.macropter~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))
anova(lm(S.macropter~Massif*gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG),]))


  # Aub-chalm

wilcox.test(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",])
wilcox.test(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",])

summary(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Aub-chalm",]))

summary(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))
anova(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Aub-chalm",]))


  # Fbleau

wilcox.test(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",])
wilcox.test(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",])

summary(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Fbleau",]))

summary(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))
anova(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Fbleau",]))


  # Ventron

wilcox.test(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",])
wilcox.test(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",])

summary(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)& Sm.carab.ecol$Massif=="Ventron",]))

summary(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))
anova(lm(S.macropter~gestion, data=Sm.carab.ecol[is.na(Sm.carab.ecol$Biais.stationnel)&is.na(Sm.carab.ecol$Biais.essence)&is.na(Sm.carab.ecol$Sample.YOUNG)& Sm.carab.ecol$Massif=="Ventron",]))






---------------------------------------------------
         # Creation tableau prs/abs sp/placette
#is.na(Nm.plot.sp)
#Occ.plot.sp <- is.na(Nm.plot.sp)
#Occ.plot.sp


