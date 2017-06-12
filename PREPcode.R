##############################                                ########################################################################################
##############################  State of Mexico PREP Results  ########################################################################################
##############################       (Notes in Spanish)       ########################################################################################
######## Elena Badillo June 5, 2017 ##################################################################################################################
##################################################################### Info about SoM PREP: http://www.ieem.org.mx/ ###################################

setwd("C:/Users/G13519/Documents/EB/EM2017") #set your own working directory

library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)    
library(downloader) 

fileURL<-"http://www.prepieem.org.mx/MEX_GOB_2017.zip" #descargar los datos del sitio
download(fileURL, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "C:/Users/G13519/Documents/EB/EM2017")

prep<-read.csv("C:/Users/G13519/Documents/EB//EM2017/MEX_GOB_2017.csv",header = TRUE, skip=6)
prep<-data.frame(prep)
prep<-prep[order(prep$fecha_hora_registro),] #ordenar base por tiempo
prepsub <- prep[,14:37] #sólo columnas relevantes
prepsub$t<-prep[,44]

for (j in 1:ncol(prepsub)){ #sustituir casillas que digan "ILEGIBLE" o "SIN DATO" con cero
    for (i in 1:nrow(prep)) {
        if (prepsub[i,j]=="ILEGIBLE" | prepsub[i,j]=="SIN DATO") {
        prepsub[i,j]=0
}}}
            
 for (j in (1:ncol(prepsub))) {
     prepsub[,j]<-as.numeric(paste(prepsub[,j]))
 }

n<-nrow(prepsub)
prepsub$pritot<- prepsub$pri+prepsub$pes+prepsub$pvem+prepsub$na+prepsub$c_pri_pvem_na_pes+prepsub$c_pri_pvem_na+prepsub$c_pri_pvem_pes+
                 prepsub$c_pri_na_pes+prepsub$c_pri_pvem+prepsub$c_pri_na+prepsub$c_pri_pes+prepsub$c_pvem_na_pes+prepsub$c_pvem_na+
                 prepsub$c_pvem_pes+prepsub$c_na_pes    #sumar votos del PRI y todas sus coaliciones

##Votos Totales Acumulados en el tiempo

#PRI
sumpritot<-as.numeric(c(1:n))
# prepsub$pritot<-as.numeric(paste(prepsub$pritot))
sumpritot[1]<-prepsub$pritot[1]

    for (i in 2:n) {
     sumpritot[i] = sumpritot[i-1]+prepsub$pritot[i]
    }
sumpritot<-data.frame(sumpritot)

#MORENA
summorena<-as.numeric(c(1:n))
prepsub$morena<-as.numeric(paste(prepsub$morena))
summorena[1]<-prepsub$morena[1]

for (i in 2:n) {
    summorena[i] = summorena[i-1]+prepsub$morena[i]
}
summorena2<-data.frame(summorena)

#PRD
sumprd<-as.numeric(c(1:n))
prepsub$prd<-as.numeric(paste(prepsub$prd))
sumprd[1]<-prepsub$prd[1]

for (i in 2:n) {
    sumprd[i] = sumprd[i-1]+prepsub$prd[i]
}
sumprd2<-data.frame(sumprd)

#PAN
sumpan<-as.numeric(c(1:n))
prepsub$pan<-as.numeric(paste(prepsub$pan))
sumpan[1]<-prepsub$pan[1]

for (i in 2:n) {
    sumpan[i] = sumpan[i-1]+prepsub$pan[i]
}
sumpan2<-data.frame(sumpan)

#TOTAL
sumtot<-as.numeric(c(1:n))
prepsub$tot<-as.numeric(paste(prepsub$total))
sumtot[1]<-prepsub$total[1]

for (i in 2:n) {
    sumtot[i] = sumtot[i-1]+prepsub$total[i]
}
sumtot2<-data.frame(sumtot)

sumas<-as.data.frame(cbind(prepsub$t,sumpritot, summorena2,sumpan2,sumprd2, sumtot2))

# Construir los porcentajes

por<-sumas[,1:5]
for (j in 1:5){
    por[,j]<-sumas[,j]/sumas[,6]
}
por<-por[,-1]
por<-por*100
por$t<-prep[,44]
por<-por[-(1:435),] #quitar las observaciones sin fecha y de sólo ceros
colnames(por) <- c("PRI","Morena", "PAN", "PRD", "fecha")

resFinal<-por[nrow(por),] #resultados finales (Los cuales deben coincidir con los del Conteo Final)

# Grafica Series de Tiempo

melt<-melt(por, id="fecha")                       # base reorganizada (transpuesta y con fecha repetida) para graficar serie por partido
colnames(melt) <- c("Fecha","Partido", "Valor")

time1<- as.numeric(as.POSIXct(melt$Fecha[1], TZ="gmt", origin="1970-01-01"))
time2 <- as.numeric(as.POSIXct(melt$Fecha[nrow(melt)], TZ="gmt", origin="1970-01-01"))
gmt <- as.POSIXct(time1:time2, tz="gmt", origin="1970-01-01")

group.colors <- c(PRI = "red2", Morena = "red4", PAN ="royalblue1", PRD = "goldenrod2")
ggplot(melt, aes(x =Fecha,y = Valor, group= Partido, colour= Partido)) +
        geom_line(size=1, alpha=0.5)+
        scale_colour_manual(values=group.colors)+
        labs(title="PREP Edomex",
             subtitle="Votación porPartido",
             caption="Fuente: IEEM",
             y="%")

ggsave(filename="ResultadosPREP.jpg", width=11, height=7) #guarda la grafica en jpg, para visualizarse con mejor resolución
