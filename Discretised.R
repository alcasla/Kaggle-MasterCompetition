#data reading
accidentes = read.csv2("data/accidentes-kaggle.csv", sep=",", dec=",", encoding='utf-8')  #Original data
accidentesTra = read.csv2("./data/accidentes-tra-transformed.csv", sep=";", dec=",")      #Transformed data
accidentesTra[,1]=NULL

#plot HORA distribution discretised and original
par(mfrow=c(1,2))
hist(accidentes$HORA, main="Histograma original Hora", xlab="Hora")
abline(v=mean(accidentes$HORA), col="Red", lwd=2)
abline(v=median(accidentes$HORA), col="Green", lwd=2)

hist(accidentesTra$HORA, main="Histograma transformed Hora", xlab="Hora")
abline(v=mean(accidentesTra$HORA), col="Red", lwd=2)
abline(v=median(accidentesTra$HORA), col="Green", lwd=2)
par(mfrow=c(1,1))