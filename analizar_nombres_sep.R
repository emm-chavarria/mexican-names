library(plyr)
library(tidyr)

removibles <- c("NULL","","-",".","0","/","\\","_")

sep <- read.csv("data/sep/PERSONAL_FEDERALIZADO_2T2012.txt",sep="|")
sep <- sep[!duplicated(sep$CURP),] 
sep$GENERO <- substring(sep[,"CURP"],11,11)
sep$ANONAC <- 1900 + as.numeric(substring(sep[,"CURP"],5,6))
sep$EDAD <- 2012 - sep$ANONAC #Año de actualizacion del archivo de origen.

ap1 <- count(sep,"PRIMERAPELLIDO")
ap1 <- ap1[ap1$freq >= 5,]
ap1 <- ap1[!(ap1$PRIMERAPELLIDO %in% removibles),]

ap2 <- count(sep,"SEGUNDOAPELLIDO")
ap2 <- ap2[ap2$freq >= 5,]
ap2 <- ap2[!(ap2$SEGUNDOAPELLIDO %in% removibles),]

apsep <- merge(ap1,ap2,all=T,by.x = "PRIMERAPELLIDO", by.y="SEGUNDOAPELLIDO")
colnames(apsep) <- c("apellido","frec_pri","frec_seg")
apsep[is.na(apsep)] <- 0
apsep <- apsep[order(apsep$frec_pri,decreasing = T),]

##Consider First and Second name frequencies by separate (i.e. "Primer_nombre" and "Seg_nombre")
sep2 = sep[c("NOMBRESTRABAJADOR", "GENERO", "EDAD")]
nombres = separate(sep2, NOMBRESTRABAJADOR, into=c("Primer_nombre", "Seg_nombre"),  sep="\\s", extra="merge")	#Splits first and second name
nombres$Primer_nombre = gsub("\\bMA.\\b|\\bMA\\b", "MARIA", nombres$Primer_nombre) #Replaces abbreviations of "MARIA"

##Counts "Primer_nombre" for women
mujeres = count(nombres[nombres$GENERO=="M",],"Primer_nombre")
mujeres <- mujeres[mujeres$freq >=5,]
mujeres <- mujeres[!(mujeres$Primer_nombre %in% removibles),]
mujeres <- mujeres[order(mujeres$freq,decreasing = T),]

##Counts "Primer_nombre" for men
hombres = count(nombres[nombres$GENERO=="H",],"Primer_nombre")
hombres <- hombres[hombres$freq >=5,]
hombres <- hombres[!(hombres$Primer_nombre %in% removibles),]
hombres <- hombres[order(hombres$freq,decreasing = T),]

##Counts "Seg_nombre" for women
mujeres2 = count(nombres[nombres$GENERO=="M",],"Seg_nombre")
mujeres2 <- mujeres2[mujeres2$freq >=5,]
mujeres2 <- mujeres2[!(mujeres2$Seg_nombre %in% removibles),]
mujeres2 = na.omit(mujeres2) #Ignores individuals with only "Primer_nombre"
mujeres2 <- mujeres2[order(mujeres2$freq,decreasing = T),]

##Counts "Seg_nombre" for men
hombres2 = count(nombres[nombres$GENERO=="H",],"Seg_nombre")
hombres2 <- hombres2[hombres2$freq >=5,]
hombres2 <- hombres2[!(hombres2$Seg_nombre %in% removibles),]
hombres2 = na.omit(hombres2) #Ignores individuals with only "Primer_nombre"
hombres2 <- hombres2[order(hombres2$freq,decreasing = T),]

##Writes output as txt
write.table(mujeres,"data/sep/mujeres_1erNombre.txt",row.names = F,quote = F, sep=";")
write.table(mujeres2,"data/sep/mujeres_2doNombre.txt",row.names = F,quote = F, sep=";")
write.table(hombres,"data/sep/hombres_2doNombre.txt",row.names = F,quote = F, sep=";")
write.table(hombres2,"data/sep/hombres2_2doNombre.txt",row.names = F,quote = F, sep=";")
