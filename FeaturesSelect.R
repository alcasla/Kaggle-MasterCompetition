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
write.csv2(accidentesTraFS, "./data/accidentes-tra-fs6.csv")  #save training data
write.csv2(accidentesTstFS, "./data/accidentes-tst-fs6.csv")  #save test data
