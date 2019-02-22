# figuras letrario

# chequeo de directorio
getwd()
# setwd("directorio/ella/elle/yo")

# tope 10 de ejercicio letrario
library(readxl)
topt<-read_excel("letrario-tope10.xls",col_names=T)
names(topt)

# sugerencia
# configurar tamaño de ventana de gráficos

###
library(ggplot2)

# figura letrario-tope10-suma
# letrario-sumario 1
# posición por puntaje de prioridad asignado (interés individual)
# exportar pdf us legal u oficio apaisado
ggplot(topt, aes(asunto, tope10)) +
  geom_bar(stat = "identity", aes(fill = clase))+
  scale_fill_brewer( palette = "Set1" ) +
  theme(axis.text.x = element_text(size=18,colour="black",angle = 0,  hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size=18,colour="black",angle = 0,  hjust = 1, vjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.justification=c(0,0), 
        legend.position="top",
        legend.key.height=unit(1,"line"),
        legend.key.width=unit(1,"line"),
        legend.spacing.x = unit(0.25, 'cm')) +
  labs(y="",x="")+
  coord_flip() 
 
# figura letrario-tope10-votos
# letrario-sumario 2
# posición por número de votos (interés colectivo)
# top10 corregido por número de alumnos que eligieron el asunto
ggplot(topt, aes(asuntovotos, tope10votos)) +
  geom_bar(stat = "identity", aes(fill = clase))+
  scale_fill_brewer( palette = "Set1" ) +
  theme(axis.text.x = element_text(size=18,colour="black",angle = 0,  hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size=18,colour="black",angle = 0,  hjust = 1, vjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.justification=c(0,0), 
        legend.position="top",
        legend.key.height=unit(1,"line"),
        legend.key.width=unit(1,"line"),
        legend.spacing.x = unit(0.25, 'cm')) +
  labs(y="",x="")+
  coord_flip() 

# figura letrario-tope10-votos-puntaje-full ***
# letrario-sumario 3
names(topt)
topt$clase<-as.factor(topt$clase)
topt$subtema<-as.factor(topt$subtema)
colores<-ifelse(topt$clase=="conceptos","red","blue")
plot(topt$tope10 ~ topt$tope10votos,
     xlab="votos",ylab="puntaje",cex.lab=1.5,cex.axis=1.5,
     col=colores, cex=2.5,pch=16)
# modelo para línea de ajuste
m1<- lm(topt$tope10 ~ topt$tope10votos)
# agregar linea
abline(m1,col="green",lwd=2)
# correlación
cor.test(topt$tope10,topt$tope10votos)
# agregar texto
text(15,170,labels="r=0.849, p=0.002")
abline(h=median(topt$tope10),
       v=median(topt$tope10votos))
legend(8,160,unique(topt$clase),
       col=colores,pch=16,bg = "white")
#text(15,130,"importantes",cex=0.8,pos=1,srt=0)
with(topt,text(tope10~tope10votos, labels = topt$subtema, pos = 2,cex=0.8,offset=0.7))

# clínica de datos
# Mariano Ordano
