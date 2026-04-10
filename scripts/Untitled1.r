#-----------------------------------------------------------------------------------------------------

# IMPORT DONNEES ENVT

 #ChPin.Ptrap<-read.csv("PlanEchantillonnage_carab_FDOrléans_MelChPin_2009.csv", sep=";", header=T)
  #  fix(ChPin.Ptrap)
   # str(ChPin.Ptrap)
    #dim(ChPin.Ptrap)
    dimnames(ChPin.Ptrap)
    dimnames(ChPin.Ptrap)[[1]] <-  ChPin.Ptrap$plot_Ptrap
    dimnames(ChPin.Ptrap)

Carab.ChPin.Ptrap <-as.data.frame(cbind(Carab.Ptrap[as.character(row.names(Carab.Ptrap)),], ChPin.Ptrap[as.character(row.names(Carab.Ptrap)),]))
    fix(Carab.ChPin.Ptrap)
    str(Carab.ChPin.Ptrap)
    dim(Carab.ChPin.Ptrap)
    dimnames(Carab.ChPin.Ptrap)

dim(Carab.ChPin.Ptrap[Carab.ChPin.Ptrap$Nper_plot.Ptrap=="6",]   )
dim(Carab.ChPin.Ptrap[Carab.ChPin.Ptrap$Nper_plot.Ptrap=="5",]   )
dim(Carab.ChPin.Ptrap[Carab.ChPin.Ptrap$Nper_plot.Ptrap=="4",]   )
dim(Carab.ChPin.Ptrap[Carab.ChPin.Ptrap$Nper_plot.Ptrap=="3",]   )
dim(Carab.ChPin.Ptrap[Carab.ChPin.Ptrap$Nper_plot.Ptrap=="2",]   )
dim(Carab.ChPin.Ptrap[Carab.ChPin.Ptrap$Nper_plot.Ptrap=="1",]   )
dim(Carab.ChPin.Ptrap[(Carab.ChPin.Ptrap$Nper_plot.Ptrap=="6") | (Carab.ChPin.Ptrap$Nper_plot.Ptrap=="5"),]   )

Carab.ChPin.Ptrap.fullper <- Carab.ChPin.Ptrap[(Carab.ChPin.Ptrap$Nper_plot.Ptrap=="6") | (Carab.ChPin.Ptrap$Nper_plot.Ptrap=="5"),]
dim(Carab.ChPin.Ptrap.fullper)
str(Carab.ChPin.Ptrap.fullper)
dimnames(Carab.ChPin.Ptrap.fullper)
fix(Carab.ChPin.Ptrap.fullper)

#--------------------------------------------