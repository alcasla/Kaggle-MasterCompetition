#data reading
accidentesTra = read.csv2("./data/accidentes-tra-8NAs.csv", sep=";", dec=",")
accidentesTst = read.csv2("./data/accidentes-tst-fs6.csv", sep=";", dec=",")

require(unbalanced)
train = accidentesTra
#Replicate instances with label 'Atropello' to balance
atropello = accidentesTra[which(accidentesTra$TIPO_ACCIDENTE == "Atropello"),]
train = rbind(train, atropello, atropello)
#Replicate instances with label 'Colision_Obstaculo' to balance
col.obs = accidentesTra[which(accidentesTra$TIPO_ACCIDENTE == "Colision_Obstaculo"),]
train = rbind(train, col.obs, col.obs, col.obs, col.obs, col.obs, col.obs)
#Replicate instances with label 'Otro' to balance
otro = accidentesTra[which(accidentesTra$TIPO_ACCIDENTE == "Otro"),]
train = rbind(train, otro, otro, otro, otro, otro)
#Replicate instances with label 'Salida_Via' to balance
salidaVia = accidentesTra[which(accidentesTra$TIPO_ACCIDENTE == "Salida_Via"),]
train = rbind(train, salidaVia[sample(1:dim(salidaVia)[1], size=3000),])
#Replicate instances with label 'Vuelco' to balance
vuelco = accidentesTra[which(accidentesTra$TIPO_ACCIDENTE == "Vuelco"),]
train = rbind(train, vuelco, vuelco, vuelco, vuelco, vuelco)

