#data reading
accidentesTra = read.csv2("./data/accidentes-tra-mice_full_m4.csv", sep=";", dec=",")
accidentesTst = read.csv2("./data/accidentes-tst-mice_full_m4.csv", sep=";", dec=",")
accidentesTra[,1]=NULL; accidentesTst[,1]=NULL;

###############
levels(accidentesTra$ISLA) <- levels(accidentesTst$ISLA)
levels(accidentesTst$MEDIDAS_ESPECIALES) <- levels(accidentesTra$MEDIDAS_ESPECIALES)
###############

##### 5-kfold cross validation #####
accidentesTra = accidentesTra.outliers
# Make sure to respect the class imbalance in the folds. Separate each class
class1 <- (1:dim(accidentesTra)[1])[accidentesTra$TIPO_ACCIDENTE=="Colision_Vehiculos"]
class2 <- (1:dim(accidentesTra)[1])[accidentesTra$TIPO_ACCIDENTE=="Salida_Via"]
class3 <- (1:dim(accidentesTra)[1])[accidentesTra$TIPO_ACCIDENTE=="Atropello"]
class4 <- (1:dim(accidentesTra)[1])[accidentesTra$TIPO_ACCIDENTE=="Vuelco"]
class5 <- (1:dim(accidentesTra)[1])[accidentesTra$TIPO_ACCIDENTE=="Otro"]
class6 <- (1:dim(accidentesTra)[1])[accidentesTra$TIPO_ACCIDENTE=="Colision_Obstaculo"]

#Combine position samples to create matrix 5col-fold
CVperm_c1 <- matrix(sample(class1,length(class1)), ncol=5, byrow=T)
CVperm_c2 <- matrix(sample(class2,length(class2)), ncol=5, byrow=T)
CVperm_c3 <- matrix(sample(class3,length(class3)), ncol=5, byrow=T)
CVperm_c4 <- matrix(sample(class4,length(class4)), ncol=5, byrow=T)
CVperm_c5 <- matrix(sample(class5,length(class5)), ncol=5, byrow=T)
CVperm_c6 <- matrix(sample(class6,length(class6)), ncol=5, byrow=T)

CVperm <- rbind(CVperm_c1, CVperm_c2, CVperm_c3, CVperm_c4, CVperm_c5, CVperm_c6)     #join matrixes


################## MODELOS TRAIN ############################
require(randomForest)
### PARAMS: change dataset value and RandomForest class formula, also algorithm params
dataset = accidentesTra     #unify use values
prediction = NULL;
for(i in 1:5){     #Cross Validation 5kfold - for each col classify and insert into an object
  subsetTra = dataset[-CVperm[,i],]
  subsetTst = dataset[CVperm[,i],-dim(dataset)[2]]
  rfModel = randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=dataset, ntree=50)
  pred = predict(rfModel, newdata=subsetTst)
  prediction = c(prediction, pred)
  cat("Iteración:", i)
}
#Accuracy - percentage of success
1-length(which(!(as.numeric(accidentesTra$TIPO_ACCIDENTE[as.vector(CVperm)])==prediction)))/length(prediction)
        #0.8283192 best - MICE and nTree=50



################## MODELOS TEST ############################
####### M1 - Random Forest ##########
require(randomForest)
rfModel = randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=accidentesTra, ntree=50)
randomForest::importance(rfModel)
plot(rfModel)     #each class label mean error

prediction = predict(rfModel, newdata=accidentesTst)

####### M2 - Random Forest ##########
require(randomForest)
rfModel = randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=accidentesTra, ntree=80)
randomForest::importance(rfModel)
plot(rfModel)     #each class label mean error

prediction = predict(rfModel, newdata=accidentesTst)



######## Code submission ##########
submission = data.frame(Id=c(1:length(prediction)), Prediction=prediction, row.names=NULL)
write.table(submission, "./submit/m2-rf.csv", row.names=FALSE, quote=FALSE, sep=",")
