#data reading
accidentesTra = read.csv2("./data/accidentes-tra-fs6.csv", sep=";", dec=",")
accidentesTst = read.csv2("./data/accidentes-tst-fs6.csv", sep=";", dec=",")

accidentesTra[which(accidentesTra$HORA==24),"HORA"]=0

######## Ensemble applying One Vs One technique for multiclissification #########
#"Colisión obstáculo" and "Vuelco" subset
iS1 = c(which(accidentesTra$TIPO_ACCIDENTE=="Colision_Obstaculo"), 
      which(accidentesTra$TIPO_ACCIDENTE=="Vuelco"))              #indexes

#"Atropello" and "Otro" subset
iS2 = c(which(accidentesTra$TIPO_ACCIDENTE=="Atropello"), 
      which(accidentesTra$TIPO_ACCIDENTE=="Otro"))                #indexes

#"Colision_Vehiculos" and "Salida_Via" subset
iS3 = c(which(accidentesTra$TIPO_ACCIDENTE=="Colision_Vehiculos"), 
      which(accidentesTra$TIPO_ACCIDENTE=="Salida_Via"))          #indexes


require(randomForest)
classCol = dim(accidentesTra)[2]    #class column number

#Model fitted to subset 1 - Colisión obstáculo" and "Vuelco"
rfS1 = randomForest::randomForest(accidentesTra[,-c(classCol,col2RemRFS1)], accidentesTra[,classCol], subset=iS1, ntree=60)
predictionS1prob = predict(rfS1, newdata=accidentesTst, type="prob")
predictionS1 = predict(rfS1, newdata=accidentesTst)

#Model fitted to subset 2 - "Atropello" and "Otro"
rfS2 = randomForest::randomForest(accidentesTra[,-c(classCol,col2RemRFS2)], accidentesTra[,classCol], subset=iS2, ntree=60)
predictionS2prob = predict(rfS2, newdata=accidentesTst, type="prob")
predictionS2 = predict(rfS2, newdata=accidentesTst)

#Model fitted to subset 3 - "Colision_Vehiculos" and "Salida_Via"
rfS3 = randomForest::randomForest(accidentesTra[,-c(classCol,col2RemRFS3)], accidentesTra[,classCol], subset=iS3, ntree=60)
predictionS3prob = predict(rfS3, newdata=accidentesTst, type="prob")
predictionS3 = predict(rfS3, newdata=accidentesTst)

#features importance for each model, remenber each model train two class labels only
randomForest::importance(rfS1)
randomForest::importance(rfS2)
randomForest::importance(rfS3)
#for each model select lower importance features to remove in next try
col2RemRFS1 = c(6)
col2RemRFS2 = c(6,21)
col2RemRFS3 = c(6)


#join predictións for different models in the same structure next to final prediction
predMostVoted = data.frame(rf1=predictionS1, rf2=predictionS2, rf3=predictionS3, pred=1:dim(accidentesTst)[1])
#Get index prediction from specialized model
indexPredRFS3.ColVeh = which(predMostVoted[,3]=="Colision_Vehiculos")
indexPredRFS3.SalVia = which(predMostVoted[,3]=="Salida_Via")

indexPredRFS2.Atr = which(predMostVoted[,2]=="Atropello")
indexPredRFS2.Otr = which(predMostVoted[,2]=="Otro")

indexPredRFS1.ColObs = which(predMostVoted[,1]=="Colision_Obstaculo")
indexPredRFS1.Vue = which(predMostVoted[,1]=="Vuelco")

#Get index without prediction
uRF3 = union(indexPredRFS3.ColVeh, indexPredRFS3.SalVia)
uRF2 = union(indexPredRFS2.Atr, indexPredRFS2.Otr)
uRF1 = union(indexPredRFS1.ColObs, indexPredRFS1.Vue)
iNoPred = setdiff( 1:dim(accidentesTst)[1], union( union(uRF3, uRF2), uRF1 ) )    #All index minus index with prediction

#set specialixed final prediction in column
predMostVoted[indexPredRFS3.ColVeh,4] = "Colision_Vehiculos"
predMostVoted[indexPredRFS3.SalVia,4] = "Salida_Via"
predMostVoted[indexPredRFS2.Atr,4] = "Atropello"
predMostVoted[indexPredRFS2.Otr,4] = "Otro"
predMostVoted[indexPredRFS1.ColObs,4] = "Colision_Obstaculo"
predMostVoted[indexPredRFS1.Vue,4] = "Vuelco"

#There are istances without prediction by specialised model, then I use most voted

#function return most repeated label from a row
MostVoted = function(row){
  r = t(row)            #get like a column
  labls = unique(r)     #labels without repeat
  mv = sapply(labls, function(x) sum(r==x))     #count label quips
  indx = which(mv == max(sapply(labls, function(x) sum(r==x))))   #index from most voted
  if(length((indx))>1)      #in case of tied vote select classifier has biggest class labels, higher success probability
    indx = length(indx)
  return(labls[indx])
}

predMostVoted[iNoPred,4] = sapply(iNoPred, function(x) MostVoted(predMostVoted[x,1:3]) )


#Most Voted pure (don´t do 49-73 line range) 0.82938
predMostVoted[,4] = sapply(1:dim(predMostVoted)[1], function(x) MostVoted(predMostVoted[x,1:3]) )


######## Code submission ##########
submission = data.frame(Id=c(1:dim(accidentesTst)[1]), Prediction=predMostVoted[,4], row.names=NULL)
write.table(submission, "./submit/m6.5-ensembleRF-MV.csv", row.names=FALSE, quote=FALSE, sep=",")

