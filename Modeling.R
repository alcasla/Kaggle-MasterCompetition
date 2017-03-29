#data reading
accidentesTra = read.csv2("./data/accidentes-tra-transformed.csv", sep=";", dec=",")
accidentesTst = read.csv2("./data/accidentes-tst-transformed.csv", sep=";", dec=",")
accidentesTra[,1]=NULL; accidentesTst[,1]=NULL;

###############
levels(accidentesTra$ISLA) <- levels(accidentesTst$ISLA)
levels(accidentesTst$MEDIDAS_ESPECIALES) <- levels(accidentesTra$MEDIDAS_ESPECIALES)
###############

##### 5-kfold cross validation #####
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
dataset = accidentesTraFS     #unify use values
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
        #0.8283192 MICE and nTree=50
        #0.8809595 FeatSel -c(1,7,28,9) and nTree=50
        #0.8683325 FeatSel -c(1,7,28,9,2,19) and nTree=50 **Best Test**
        #0.8638014 FeatSel -c(1,7,28,9,2,19,25,10) and nTree=50
  


################## MODELOS TEST ############################
####### Random Forest 1.0 ##########
require(randomForest)
rfModel = randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=accidentesTra, ntree=50)
randomForest::importance(rfModel)
plot(rfModel)     #each class label mean error

prediction = predict(rfModel, newdata=accidentesTst)

####### Random Forest 2.0 ##########
require(randomForest)
rfModel = randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=accidentesTra, ntree=80)
randomForest::importance(rfModel)
plot(rfModel)     #each class label mean error

prediction = predict(rfModel, newdata=accidentesTst)

####### Random Forest 4.1 - 4.2 - 4.3 ##########
require(randomForest)
rfModel = randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=accidentesTraFS, ntree=50)
randomForest::importance(rfModel)
plot(rfModel)     #each class label mean error

prediction = predict(rfModel, newdata=accidentesTstFS)

################### XGBoost 1.0 ##################
require(xgboost)
require(data.table)   #data reading
require(magrittr)     #operation functions
train = fread('./data/accidentes-tra-fs6.csv', header=T, stringsAsFactors=T)
test = fread('./data/accidentes-tst-fs6.csv', header=T, stringsAsFactors=T)
train$V1=NULL; test$V1=NULL

testLabels = levels(train$TIPO_ACCIDENTE)         #class labels
y = as.numeric(factor(train$TIPO_ACCIDENTE))-1    #class column as numbers
train$TIPO_ACCIDENTE = NULL                       #delete class column from data structure

trainMatrix = apply(train, 2, function(x) as.numeric(factor(x)))  #trin as matrix
testMatrix = apply(test, 2, function(x) as.numeric(factor(x)))    #test as matrix

numberOfClasses <- max(y)+1
param <- list("objective" = "multi:softmax",
              "eval_metric" = "merror",
              "num_class" = numberOfClasses)
cv.nround <- 30
cv.nfold <- 5
#training with Cross Validation
xgbst.cv = xgb.cv(param=param, data=trainMatrix, label=y, nfold=cv.nfold, nrounds=cv.nround)

#train + test
xgbModel = xgboost(param=param, data = trainMatrix, label = y, nrounds = cv.nround)
prediction = predict(xgbModel, testMatrix)

#parse back to character class labels 
prediction[which(prediction==0)] = testLabels[1]
prediction[which(prediction==1)] = testLabels[2]
prediction[which(prediction==2)] = testLabels[3]
prediction[which(prediction==3)] = testLabels[4]
prediction[which(prediction==4)] = testLabels[5]
prediction[which(prediction==5)] = testLabels[6]
#______________________________________________________

############## Random Forest 5.0 ################
accidentesTra = read.csv2("./data/accidentes-tra-8NAs.csv", sep=";", dec=",")
accidentesTst = read.csv2("./data/accidentes-tst-transformed.csv", sep=";", dec=",")
require(randomForest)
rfModel = randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=accidentesTra, ntree=50)
randomForest::importance(rfModel)
plot(rfModel)     #each class label mean error

prediction = predict(rfModel, newdata=accidentesTst)
#________________________________________________

############## Random Forest 6.0 ################
accidentesTra = read.csv2("./data/accidentes-tra-transfV2.csv", sep=";", dec=",")
accidentesTst = read.csv2("./data/accidentes-tst-transfV2.csv", sep=";", dec=",")
require(randomForest)
rfModel = randomForest::randomForest(tipo_accidente ~ ., data=accidentesTra, ntree=50)
randomForest::importance(rfModel)
plot(rfModel)     #each class label mean error

prediction = predict(rfModel, newdata=accidentesTst)
#________________________________________________0.82671



######## Code submission ##########
submission = data.frame(Id=c(1:length(prediction)), Prediction=prediction, row.names=NULL)
write.table(submission, "./submit/m6-rf6.0.csv", row.names=FALSE, quote=FALSE, sep=",")
