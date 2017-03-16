#data reading
accidentesTra = read.csv2("./data/accidentes-tra-transformed.csv", sep=";", dec=",")
accidentesTst = read.csv2("./data/accidentes-tst-transformed.csv", sep=";", dec=",")
accidentesTra[,1]=NULL; accidentesTst[,1]=NULL;


require(mvoutlier)
###### OUTLIERS DETECTION ########
outliers = uni.plot(accidentesTra[,8:12], alpha=0.005)    #select only extreme outliers
length(which(outliers$outliers))                          #outliers counting
table(accidentesTra[which(outliers$outliers),"TIPO_ACCIDENTE"])   #outliers by class

#get new training dataset and feedback
accidentesTra.outliers = accidentesTra[-which(outliers$outliers),]

#show changes
cat('Training dataset had', dim(accidentesTra)[1], 'before remove outliers')
cat('CURRENT DATASET after remove outliers has', dim(accidentesTra.outliers)[1], 'instances')

#save data without outliers
write.csv2(accidentesTra.outliers, "./data/accidentes-tra-outliers.csv")  #save training data only
