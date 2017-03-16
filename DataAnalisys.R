#data reading
accidentes = read.csv2("data/accidentes-kaggle.csv", sep=",", dec=",", encoding='utf-8')
accidentes.test = read.csv2("data/accidentes-kaggle-test.csv", sep=",", dec=",", encoding='utf-8')

#general information
dim(accidentes);  dim(accidentes.test);

summary(accidentes)
summary(accidentes.test)

head(accidentes, n=3)

#factor levels
apply(accidentes, 2, unique)
length(apply(accidentes, 2, unique)[[8]])

#round Hora feature to reduce factor levels
accidentes$HORA = round(accidentes$HORA)
accidentes.test$HORA = round(accidentes.test$HORA)

#check columns with missing values
lapply(1:dim(accidentes)[2], function(x) length(which(is.na(accidentes[,x]))))

#view data
boxplot(accidentes[,1], main="Boxplot Accidentes, Año", las=2)
boxplot(accidentes[,c(3,8,9,10,11,12)], main="Boxplot Accidentes", las=2, cex.axis=0.4)


#histogram editing labels function, draw red mean and green median - Accidentes dataset
histogramaAbline <- function (x) {
  hist(accidentes[,x], main=paste("Histograma ", names(accidentes)[x]), xlab=names(accidentes)[x])
  abline(v=mean(accidentes[,x]), col="Red", lwd=2)
  abline(v=median(accidentes[,x]), col="Green", lwd=2)
}

#distribution for each numeric feature
par(mfrow=c(3,3))
sapply(c(1,3,8,9,10,11,12), histogramaAbline)
par(mfrow=c(1,1))

#plot feature distribution - Accidentes dataset
#default visualization
plotFeatureDefault <- function (x) {
  plot(accidentes[,x], main=paste("Histograma ", names(accidentes)[x]), las=2, cex.axis=0.4)
}
#distribution for each numeric feature
par(mfrow=c(2,2))
sapply(c(2,4,5,6,7,13,14,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30), plotFeatureDefault)
par(mfrow=c(1,1))


#ggplot visualization
require(ggplot2)
require(gridExtra)

plotFeature <- function (x) {
  #plot(accidentes[,x], main=paste("Histograma ", names(accidentes)[x]), las=2, cex.axis=0.4)
  ggplot(data = accidentes) + ggtitle(paste("Histograma", names(accidentes)[x])) +
    geom_bar(mapping = aes(x=accidentes[,x])) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8))
}

#plot 4x4 plot categoric features
grid.arrange(plotFeature(2), plotFeature(4), plotFeature(5), plotFeature(6), nrow=2, ncol=2)
grid.arrange(plotFeature(7), plotFeature(13), plotFeature(14), plotFeature(16), nrow=2, ncol=2)
grid.arrange(plotFeature(17), plotFeature(18), plotFeature(19), plotFeature(20), nrow=2, ncol=2)
grid.arrange(plotFeature(21), plotFeature(22), plotFeature(23), plotFeature(24), nrow=2, ncol=2)
grid.arrange(plotFeature(25), plotFeature(26), plotFeature(27), plotFeature(28), nrow=2, ncol=2)
grid.arrange(plotFeature(29), plotFeature(30), nrow=2, ncol=2)


#Class variable
summary(accidentes$TIPO_ACCIDENTE)


#function plotting feature vs class, use conment line instead of its above line for too long labels in x axis
plotFeatureVsClass = function(x) {
  ggplot(data = accidentes) + ggtitle(paste("Histograma", names(accidentes)[x], "Vs TIPO_ACCIDENTE")) +
  geom_bar(mapping = aes(x=accidentes[,x], fill=TIPO_ACCIDENTE)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8))
  #theme(axis.text.x = element_blank())
}

#plot 4x4 plot features vs class
grid.arrange(plotFeatureVsClass(1), plotFeatureVsClass(2), plotFeatureVsClass(3), plotFeatureVsClass(4), nrow=2, ncol=2)
grid.arrange(plotFeatureVsClass(5), plotFeatureVsClass(6), plotFeatureVsClass(7), plotFeatureVsClass(8), nrow=2, ncol=2)
grid.arrange(plotFeatureVsClass(9), plotFeatureVsClass(10), plotFeatureVsClass(11), plotFeatureVsClass(12), nrow=2, ncol=2)
grid.arrange(plotFeatureVsClass(13), plotFeatureVsClass(14), plotFeatureVsClass(16), plotFeatureVsClass(17), nrow=2, ncol=2)
grid.arrange(plotFeatureVsClass(18), plotFeatureVsClass(19), plotFeatureVsClass(20), plotFeatureVsClass(21), nrow=2, ncol=2)
grid.arrange(plotFeatureVsClass(22), plotFeatureVsClass(23), plotFeatureVsClass(24), plotFeatureVsClass(25), nrow=2, ncol=2)
grid.arrange(plotFeatureVsClass(26), plotFeatureVsClass(27), plotFeatureVsClass(28), plotFeatureVsClass(29), nrow=2, ncol=2)

#Hora VS TotalVíctimas - Tipo de Accidente
ggplot(data=accidentes) + 
  geom_point(mapping = aes(x=HORA, y=TOT_VICTIMAS)) +
  facet_wrap(~TIPO_ACCIDENTE, nrow=2)

#Hora VS TotalVíctimas - Tipo de Accidente
ggplot(data=accidentes) + 
  geom_point(mapping = aes(x=TIPO_INTERSEC, y=TOT_VICTIMAS)) +
  facet_wrap(~TIPO_ACCIDENTE, nrow=2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=7))

#Victims type intersection
apply(table(accidentes$TOT_VICTIMAS, accidentes$TIPO_INTERSEC), 2, sum)/dim(accidentes)[1]


#write data
write.csv2(accidentes, "./data/accidentes-kaggle-V2.0.csv")
write.csv2(accidentes.test, "./data/accidentes-kaggle-test-V2.0.csv")
