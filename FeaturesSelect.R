#data reading
accidentesTra = read.csv2("./data/accidentes-tra-transformed.csv", sep=";", dec=",")
accidentesTst = read.csv2("./data/accidentes-tst-transformed.csv", sep=";", dec=",")
accidentesTra[,1]=NULL; accidentesTst[,1]=NULL;

require(FSelector)    #Features selection package
#Estimate information gain and order by importance
weights = FSelector::information.gain(TIPO_ACCIDENTE ~ ., accidentesTra)
weights = data.frame(importance = weights[order(weights$attr_importance, decreasing=T),], 
                     row.names = row.names(weights)[order(weights$attr_importance, decreasing=T)])
print(weights)


#After check results remove needless features 
#Feature selection subsets - RF4.2
accidentesTraFS = accidentesTra[,-c(1,7,28,9,2,19)]
accidentesTstFS = accidentesTst[,-c(1,7,28,9,2,19)]

#save data processed
write.csv2(accidentesTraFS, "./data/accidentes-tra-fs6.csv", row.names=F)  #save training data
write.csv2(accidentesTstFS, "./data/accidentes-tst-fs6.csv", row.names=F)  #save test data



# FEATURE SELECTION - V.2 -----------------------------
#Read large feature dataset from *DataTransformation V2*
accidentesTra = read.csv2("./data/accidentes-tra-transfV2.csv", sep=";", dec=",")
accidentesTst = read.csv2("./data/accidentes-tst-transfV2.csv", sep=";", dec=",")

require(randomForest)
rfModel = randomForest::randomForest(tipo_accidente ~ ., data=accidentesTra, ntree=50)
randomForest::importance(rfModel)     #print importance of each feature

#Remove minor features to reduce dimensionality
#less than 10 importance features
rmv = which(randomForest::importance(rfModel)[,1] < 10)
datTra102 = accidentesTra[,-rmv]
datTst102 = accidentesTst[,-rmv]
#less than 20 importance features
rmv = which(randomForest::importance(rfModel)[,1] < 20)
datTra92 = accidentesTra[,-rmv]
datTst92 = accidentesTst[,-rmv]
#less than 40 importance features
rmv = which(randomForest::importance(rfModel)[,1] < 40)
datTra74 = accidentesTra[,-rmv]
datTst74 = accidentesTst[,-rmv]
#______________________________________________________
