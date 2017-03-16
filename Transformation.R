#data reading
accidentesTra = read.csv2("./data/accidentes-tra-mice_full_m4.csv", sep=";", dec=",")
accidentesTst = read.csv2("./data/accidentes-tst-mice_full_m4.csv", sep=";", dec=",")
accidentesTra[,1]=NULL; accidentesTst[,1]=NULL;


##### DATA TRANSFORMATION #######
require(caret)
#Center and scale numeric features
preprocessValues = caret::preProcess(accidentesTra[,8:12], methods=c("center", "scale"))
transformedValues = predict(preprocessValues, accidentesTra[,8:12])

#insert transformed data
accidentesTra.prep = accidentesTra
accidentesTra.prep[,8:12] = transformedValues

#show changes
summary(accidentesTra[,8:12])
summary(accidentesTra.prep[,8:12])


#Process test data
preprocessValues = caret::preProcess(accidentesTst[,8:12], methods=c("center", "scale"))
transformedValues = predict(preprocessValues, accidentesTst[,8:12])
accidentesTst.prep = accidentesTst
accidentesTst.prep[,8:12] = transformedValues

#save data processed
write.csv2(accidentesTra.prep, "./data/accidentes-tra-transformed.csv")  #save training data
write.csv2(accidentesTst.prep, "./data/accidentes-tst-transformed.csv")  #save test data
