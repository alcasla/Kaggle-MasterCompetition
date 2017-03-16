#data reading
accidentes = read.csv2("./data/accidentes-kaggle-V2.0.csv", sep=";", dec=",")
accidentes.test = read.csv2("./data/accidentes-kaggle-test-V2.0.csv", sep=";", dec=",")
accidentes[,1]=NULL; accidentes.test[,1]=NULL;

##################### DATA IMPUTATION ##############################
#NA per instance
MV = sapply(1:dim(accidentes)[1], function(x) sum(is.na(accidentes[x,])==TRUE))

require(ggplot2)
#histogram of NA per instances
MV = as.data.frame(MV)
ggplot(data = MV) + ggtitle(paste("Histograma NAs por instancia")) +
  geom_bar(mapping = aes(x=MV, fill=factor(MV)))

require(VIM)
#visualice patern of NA and barplot
subset = accidentes[sample(1:dim(accidentes)[1], 2000), c(15,20,21,25,26,27,28,29)]
VIM::aggr(subset, col=c("blue", "red"), numbers=TRUE, sortVars=TRUE, 
     labels=c("CARRETERA", "ACOND CALZADA", "PRIORIDAD", "VISIB REDU", "OTRAS CIRSC", "ACERAS", "DENSIDAD CIRC", "MEDIDAS ESP"), 
     cex.axis=0.5, gap=1, ylab=c("Gráfico NAs", "Patrón"))

require(mice)
accidentesSub = accidentes[, -15]    #subset from dataset to impute, without "Carretera"
#Compute missing values before impute
mice::ncc(accidentesSub)         #2599 complete instances from full original dataset
sum(mice::ici(accidentesSub))    #27403 incomplete instances from full original dataset

#Impute missing values
imputados = mice::mice(accidentesSub, m=4, method="pmm")
datosimputados.full = mice::complete(imputados)

imputados = mice::mice(accidentesSub, m=6, method="pmm")
datosimputados.full6 = mice::complete(imputados)

#Compute instances with missing values after impute
mice::ncc(datosimputados)
sum(mice::ici(datosimputados))

write.csv2(datosimputados.full, "./data/accidentes-tra-mice_full_m4.csv")  #save training data
write.csv2(datosimputados.full6, "./data/accidentes-tra-mice_full_m6.csv")


#calcule number of different imputed values between imputed data with MICE m=4 and m=6
dif.imputa = sapply(1:dim(datosimputados.full)[2], function(x) sum(!(datosimputados.full[,x]==datosimputados.full6[,x])))
sum(differentImputations)

#Compare both models from MICE imputation
require(caret)
require(randomForest)
rf1 = randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=accidentesT1, ntree=30)
rf1.2 = randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=accidentesT1, ntree=50)
rf2 = randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=accidentesT2, ntree=30)
rf2.2 = randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=accidentesT2, ntree=50)

rfCompare <- data.frame(tipo=factor(c('Atropello', 'Col Obs', 'Col Veh', 'Otro', 'Salida vía', 'Vuelco')), 
                        rf30=rf1$confusion[,7], rf50=rf1.2$confusion[,7], rftwo30=rf2$confusion[,7], 
                        rftwo50=rf2.2$confusion[,7])
apply(rfCompare,2,mean)

#Selected imputation, can apply same techniques to test
accidentes.testSub = accidentes.test[, -15]    #subset from dataset to impute, without "Carretera"
#Impute missing values
mice::ncc(accidentes.testSub)      #complete instances before imputation
imputados = mice::mice(accidentes.testSub, m=4, method="pmm")
imputadosTest.full = mice::complete(imputados)
mice::ncc(imputadosTest.full)      #complete instances after imputation
write.csv2(imputadosTest.full, "./data/accidentes-tst-mice_full_m4.csv")  #save test data
