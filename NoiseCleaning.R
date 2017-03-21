#data reading
accidentesTra = read.csv2("./data/accidentes-tra-fs6.csv", sep=";", dec=",")


#original data to get noisy rows
originalData = read.csv2("data/accidentes-kaggle.csv", sep=",", dec=",", encoding='utf-8')
#get number of NAs by rows and substract indexes
NAbyRow = sapply(1:dim(originalData)[1], function(numrow) sum(is.na(originalData[numrow,])==TRUE))

index8NA = which(NAbyRow==8)    #indexes of instances with 8 NAs
message(ifelse(length(index8NA) == table(NAbyRow)[[9]], 'Index collection right', 
               paste('ERROR: different instances number getting indexes with', 8, 'NAs')))     #check subset selection

index7NA = which(NAbyRow==7)    #indexes of instances with 7 NAs
message(ifelse(length(index7NA) == table(NAbyRow)[[8]], 'Index collection right', 
               paste('ERROR: different instances number getting indexes with', 7, 'NAs')))     #check subset selection

index6NA = which(NAbyRow==6)    #indexes of instances with 6 NAs
message(ifelse(length(index6NA) == table(NAbyRow)[[7]], 'Index collection right', 
               paste('ERROR: different instances number getting indexes with', 6, 'NAs')))     #check subset selection

index5NA = which(NAbyRow==5)    #indexes of instances with 5 NAs
message(ifelse(length(index5NA) == table(NAbyRow)[[6]], 'Index collection right', 
               paste('ERROR: different instances number getting indexes with', 5, 'NAs')))     #check subset selection

#subset with noisy instances
subset8NA = accidentesTra[index8NA,]
table(subset8NA$TIPO_ACCIDENTE)

subset7NA = accidentesTra[index7NA,]
table(subset7NA$TIPO_ACCIDENTE)

subset6NA = accidentesTra[index6NA,]
table(subset6NA$TIPO_ACCIDENTE)

subset5NA = accidentesTra[index5NA,]
table(subset5NA$TIPO_ACCIDENTE)

#select noisy instances which 8 NAs and class label not "Colision_Obstaculo" (few instances)
index8NAtoRemove = index8NA[which(!subset8NA$TIPO_ACCIDENTE=="Colision_Obstaculo")]
#all instances with 7 NAs belong to abundant class labels
index7NAtoRemove = index7NA
#select noisy instances which 6 NAs and class label not "Colision_Obstaculo" or "vuelco"
index6NAtoRemove = index6NA[which(!subset6NA$TIPO_ACCIDENTE=="Colision_Obstaculo")]
index6NAtoRemove = index6NAtoRemove[-which(subset6NA$TIPO_ACCIDENTE=="Vuelco")]
#select noisy instances which 6 NAs and class label not "Colision_Obstaculo" or "vuelco"
index5NAtoRemove = index5NA[which(subset5NA$TIPO_ACCIDENTE=="Colision_Vehiculos")]
index5NAtoRemove = c(index5NAtoRemove, index5NA[which(subset5NA$TIPO_ACCIDENTE=="Salida_Via")])


#Remove instances too noisy and save dataset
accidentesTra.8NA = accidentesTra[-index8NAtoRemove,]
write.csv2(accidentesTra.8NA, "./data/accidentes-tra-8NAs.csv", row.names=F)  #save training data without instances 8 NAs
