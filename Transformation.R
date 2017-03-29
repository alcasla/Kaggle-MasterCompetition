#data reading
accidentesTra = read.csv2("./data/accidentes-tra-mice_full_m4.csv", sep=";", dec=",")
accidentesTst = read.csv2("./data/accidentes-tst-mice_full_m4.csv", sep=";", dec=",")
accidentesTra[,1]=NULL; accidentesTst[,1]=NULL;


############ DATA TRANSFORMATION - V.1 #############
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
  #There are a problem because sd and mean is close to zero due to features distribution and the scale 
    #won´t work properly, anyway I only want apply this methods and I´m interested on remove  nothing yet


#Process test data
preprocessValues = caret::preProcess(accidentesTst[,8:12], methods=c("center", "scale"))
transformedValues = predict(preprocessValues, accidentesTst[,8:12])
accidentesTst.prep = accidentesTst
accidentesTst.prep[,8:12] = transformedValues

#save data processed
write.csv2(accidentesTra.prep, "./data/accidentes-tra-transformed.csv")  #save training data
write.csv2(accidentesTst.prep, "./data/accidentes-tst-transformed.csv")  #save test data
#___________________________________________________


# DATA TRANSFORMATION - V.2 -----------------------------------------------

# Tranform a categoric feature to binary features, for each label generate a new binary feature
#   Params:   feature - categoric feature with factors
#   Return:   data.frame with column number equal to 'feature' levels number
categoric2binary = function(feature){
  labls= levels(feature)    #get labels
  output = data.frame(matrix(nrow=length(feature), ncol=length(labls)))   #prealocate output structure
  
  output = sapply(1:length(labls), function(lbl){ generateBinByLabel(feature, labls[lbl]) })
  output = as.data.frame(output)
  
  return(output)
}

# Generate a binary column called as 'label' and the cases which 'label' is in 'data'
#   Params:   data - categoric feature with factors
#             label - one of the 'data' labels
#   Return:   list contains 1 if 'data' equal to 'label' or 0 otherwise, for each row
generateBinByLabel = function(data, label){
  binarized = data.frame(matrix(nrow=length(data), ncol=1))
  colnames(binarized) = label
  
  binarized[which(data==label),] = 1
  binarized[which(data!=label),] = 0
  
  return(binarized)
}

#Preprocessing categoric features with low level number
#TRAINING
training = accidentesTra
training = cbind(training, categoric2binary(training$ISLA))
training$ISLA = NULL
training = cbind(training, categoric2binary(training$ZONA))
training$ZONA = NULL
training = cbind(training, categoric2binary(training$ZONA_AGRUPADA))
training$ZONA_AGRUPADA = NULL
training = cbind(training, categoric2binary(training$RED_CARRETERA))
training$RED_CARRETERA = NULL
training = cbind(training, categoric2binary(training$TIPO_VIA))
training$TIPO_VIA = NULL
training = cbind(training, categoric2binary(training$TRAZADO_NO_INTERSEC))
training$TRAZADO_NO_INTERSEC = NULL
training = cbind(training, categoric2binary(training$TIPO_INTERSEC))
training$TIPO_INTERSEC = NULL
training = cbind(training, categoric2binary(training$ACOND_CALZADA))
training$ACOND_CALZADA = NULL
training = cbind(training, categoric2binary(training$PRIORIDAD))
training$PRIORIDAD = NULL
training = cbind(training, categoric2binary(training$SUPERFICIE_CALZADA))
training$SUPERFICIE_CALZADA = NULL
training = cbind(training, categoric2binary(training$LUMINOSIDAD))
training$LUMINOSIDAD = NULL
training = cbind(training, categoric2binary(training$FACTORES_ATMOSFERICOS))
training$FACTORES_ATMOSFERICOS = NULL
training = cbind(training, categoric2binary(training$VISIBILIDAD_RESTRINGIDA))
training$VISIBILIDAD_RESTRINGIDA = NULL
training = cbind(training, categoric2binary(training$OTRA_CIRCUNSTANCIA))
training$OTRA_CIRCUNSTANCIA = NULL
training = cbind(training, categoric2binary(training$DENSIDAD_CIRCULACION))
training$DENSIDAD_CIRCULACION = NULL
training = cbind(training, categoric2binary(training$MEDIDAS_ESPECIALES))
training$MEDIDAS_ESPECIALES = NULL
#TEST
test = accidentesTst
test = cbind(test, categoric2binary(test$ISLA))
test$ISLA = NULL
test = cbind(test, categoric2binary(test$ZONA))
test$ZONA = NULL
test = cbind(test, categoric2binary(test$ZONA_AGRUPADA))
test$ZONA_AGRUPADA = NULL
test = cbind(test, categoric2binary(test$RED_CARRETERA))
test$RED_CARRETERA = NULL
test = cbind(test, categoric2binary(test$TIPO_VIA))
test$TIPO_VIA = NULL
test = cbind(test, categoric2binary(test$TRAZADO_NO_INTERSEC))
test$TRAZADO_NO_INTERSEC = NULL
test = cbind(test, categoric2binary(test$TIPO_INTERSEC))
test$TIPO_INTERSEC = NULL
test = cbind(test, categoric2binary(test$ACOND_CALZADA))
test$ACOND_CALZADA = NULL
test = cbind(test, categoric2binary(test$PRIORIDAD))
test$PRIORIDAD = NULL
test = cbind(test, categoric2binary(test$SUPERFICIE_CALZADA))
test$SUPERFICIE_CALZADA = NULL
test = cbind(test, categoric2binary(test$LUMINOSIDAD))
test$LUMINOSIDAD = NULL
test = cbind(test, categoric2binary(test$FACTORES_ATMOSFERICOS))
test$FACTORES_ATMOSFERICOS = NULL
test = cbind(test, categoric2binary(test$VISIBILIDAD_RESTRINGIDA))
test$VISIBILIDAD_RESTRINGIDA = NULL
test = cbind(test, categoric2binary(test$OTRA_CIRCUNSTANCIA))
test$OTRA_CIRCUNSTANCIA = NULL
test = cbind(test, categoric2binary(test$DENSIDAD_CIRCULACION))
test$DENSIDAD_CIRCULACION = NULL
test = cbind(test, categoric2binary(test$MEDIDAS_ESPECIALES))
test$MEDIDAS_ESPECIALES = NULL

#Remove class from midle of dataset
tipo_accidente = training$TIPO_ACCIDENTE
training$TIPO_ACCIDENTE = NULL

#match train and test features
which(!(colnames(training) %in% colnames(test)))    #features in train and not in test
training[,c(119,123,124)] = NULL    #let class feature at the end


#save data processed
write.csv2(training, "./data/accidentes-tra-transfV2.csv", row.names=F)  #write training data
write.csv2(test, "./data/accidentes-tst-transfV2.csv", row.names=F)  #write test data
