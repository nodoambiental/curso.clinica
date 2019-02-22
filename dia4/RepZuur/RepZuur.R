# Curso
# Clínica de datos e investigación en biología
# fac
# unt
# Mariano Ordano

### Reproducibilidad de Zuur et al. 2010 
### y "otros menesteres"

# Código R modificado de:
# A protocol for data exploration to avoid common statistical problems
# Methods in Ecology and Evolution 1: 3-14
# Zuur A.F., E.N. Ieno & C.S. Elphick
# otros detalles citados en el artículo y apéndice S1

### 1 lectura de base de datos en R
# averiguar directorio
getwd()
# establecer nuevo directorio
# setwd("E:/Cursos/imbaclide2017/")
# setwd("/home/mordano/Documentos/clinica2018/docente/dia4/RepZuur")
# atajo : File > Change dir...
# abrimos archivo sin indicar directorio
Sparrows<-read.table("SparrowsElphick.txt",header=T)
# lectura en RStudio con inicio desde carpeta de archivos
Sparrows<-read.table("SparrowsElphick.txt",header=T)

# nombres de variables
names(Sparrows)

# encabezado de base
head(Sparrows)
# ver base
# View(Sparrows)

# estructura de datos
str(Sparrows)

# sumarios
summary(Sparrows)

# Year no tiene sentido para una media
summary(Sparrows$wingcrd)

# redondeo
round(summary(Sparrows$wingcrd),0)
# tablitas
tablita1<-with(Sparrows,tapply(wingcrd,list(Sex),mean))
tablita2<-with(Sparrows,tapply(wingcrd,list(Year,Sex),mean))
tablita2

# exportar tablita2
write.csv2(tablita2,"tablita2.csv",quote=F,row.names=T)
write.table(tablita2,"tablita2.txt",row.names=F)

library(openxlsx)
write.xlsx(tablita2,"tablita2.xlsx",col.names=T)


# reproducir Figura 2 Zuur et al. 2010
par(mfrow=c(1,2),mar=c(5,4,2,1)) # volver a rutina
boxplot(Sparrows$wingcrd,ylab="cuerda de ala (mm)")
dotchart(Sparrows$wingcrd,xlab="cuerda de ala (mm)",
         ylab="orden de los datos")

# reproducir Figura 3 Zuur et al. 2010
Z<-cbind(Sparrows$wingcrd, Sparrows$tarsus,  Sparrows$head,
         Sparrows$culmen,  Sparrows$nalospi, Sparrows$wt)
colnames(Z)<-c("ala","tarso","cabeza",
                 "culmen","nalospi","peso")
library(lattice)
dotplot(as.matrix(Z),groups = FALSE,
        strip = strip.custom(bg = 'white',
        par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = "blue", cex  = 0.5, pch = 21,
        xlab = "valor de la variable",
        ylab = "orden de los datos del archivo txt")
# para guardar gráfico: File > Save as > ...

# reproducir Figura 4 Zuur et al. 2010
# Godwit intake rates
Godwits<-read.table("Godwits.txt",header=T)
# Godwits<-read.table("E:/Cursos/imbaclide2017/Godwits.txt",header=T)
# Data info:
# Sex #sex 1=female	0 should go out #sex 2=male
# Age #1= adult #2= juvenile #0=UNKNOWN
# Location #locationa=0 #locationb=1
# Period #period=0 ;southern summr #period=1; prepare or migration #period=2; sourthern winter
names(Godwits)
str(Godwits)

# establecer variable como factor
Godwits$fSEX <- factor(Godwits$SEX, levels = c(0, 1, 2),
                       labels = c("x", "hembra", "macho"))
Godwits$fPERIOD <- factor(Godwits$PERIOD, levels = c(0, 1, 2),
                          labels = c("verano","pre-migración","invierno"))
# library(lattice)
bwplot(mgconsumed ~ fPERIOD | fSEX, data = Godwits,
   strip = strip.custom(bg = 'white'),   subset = SEX!=0,
   cex = .5, layout = c(2, 1),
   xlab = "período de migración", ylab = "tasa de ingesta",
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))
# cerrar ventana de gráfico

# hacemos algo similar con Sparrows
names(Sparrows)
# chequeo de variable factor
levels(Sparrows$Sex)
Sparrows$fsex<-factor(Sparrows$Sex,levels=c(0,4,5),
                       labels = c("x","macho","hembra"))
str(Sparrows$fsex)
levels(Sparrows$fsex)
boxplot(wingcrd~fsex,data=Sparrows,col="orange",
        xlab ="sexo",ylab="cuerda de ala",
        las=1,main="")

# cambiar aspecto 1 # las damas primero
Sparrows$fsex<-factor(Sparrows$fsex,levels=c("hembra","macho","x"))
boxplot(wingcrd~fsex,data=Sparrows,col="orange",
        xlab ="sexo",ylab="cuerda de ala",las=1,main="")

# cambiar aspecto 2
?boxplot
boxplot(wingcrd~fsex,data=Sparrows,col="orange",
        xlab ="",ylab="cuerda de ala",las=1,main="",
	  outline=F,pars=list(boxwex=0.5,staplewex=0.25))

# cambiar aspecto 3
colores<-c("red","blue","green")
boxplot(wingcrd~fsex,data=Sparrows,col=colores,
        xlab ="cuerda de ala (mm)",ylab="",las=1,main="",
	  outline=F,pars=list(boxwex=0.5,staplewex=0.25),
        notch=T,horizontal=T)
# agregado de lineas comparativas
# antes calculamos la ubicaci?n de las l?neas
# objeto sumario
m<-with(Sparrows,tapply(wingcrd,fsex,summary))
m
# rango intercuartilo de x
riq<-m[[3]][[5]]-m[[3]][[2]]
ns<-nrow(subset(Sparrows,fsex=="x")) # n muestra
# l?mites low & upper: mediana -/+ (1.58*(rango iq / ra?z de n))
Lnotch<-m[[3]][[3]]-(1.58*(riq/(sqrt(ns))))
Unotch<-m[[3]][[3]]+(1.58*(riq/(sqrt(ns))))
# agregar l?nea en gr?fico activo
abline(v=Unotch,lwd=1,lty=2,col="chartreuse4")
abline(v=Lnotch,lwd=1,lty=2,col="chartreuse4")

# repite la funci?n boxplot aplicado a Godwits
names(Godwits)
boxplot(mgconsumed~fSEX,data=Godwits,col="blue",
        xlab ="sexo",ylab="tasa de ingesta",
        las=1,main="")
# ...

# reproducir Figure 5 Zuur et al. 2010
Sparrows$fMonth<-factor(Sparrows$Month,
    levels = c(5, 6, 7, 8, 9, 10),
    labels=c("Mayo","Junio","Julio","Agosto",
             "Septiembre","Octubre"))
hist(Sparrows$wt,
     xlab = "peso (g)", breaks = 30,
     main = "", ylab = "frecuencia")

# supongamos que queremos saber que pasa en un periodo (el que indica Zuur)
Sparrows$I1 <- Sparrows$fMonth =="Junio" |
               Sparrows$fMonth =="Julio" |
               Sparrows$fMonth =="Agosto"
hist(Sparrows$wt[Sparrows$I1],
     xlab = "peso (g)", breaks = 30,
     main = "", ylab = "frecuencia",add=T,col="lightblue")

# descomposici?n por mes
# library(lattice)
histogram( ~ wt | fMonth, type = "count",
    xlab = "peso (g)",
    ylab = "frecuencia",
    nint=30,layout=c(1,3),
    strip.left = strip.custom(bg = 'white'),
    strip = F,
    col.line = "black", col = "white",
    scales = list(x = list(relation = "same"),
                  y = list(relation = "same"),
                  draw = TRUE),
    subset = fMonth =="Junio" | fMonth == "Julio" |fMonth == "Agosto",
    data = Sparrows)
    
# reproducir Figura 7 Zuur et al. 2010
RiceField <- read.table(file="ElphickBirdData.txt", header = TRUE)
names(RiceField)
str(RiceField)
head(RiceField)[1:16]
tail(RiceField)

par() # par?metros gr?ficos iniciales (default)
opar<-par() # los renombramos para cambiarlos
par(mar = c(4, 4, 3, 2))
plot(table(round(RiceField$AREA * RiceField$AQBIRDS)),
    type = "h",
    xlim = c(0, 100),
    xlab = "valores observados", ylab = "frecuencia")
par(opar) # retorna a par?metros gr?ficos iniciales (default)

# reproducir Figura 8 Zuur et al. 2010
names(RiceField)
todas <- c(
"TUSW",     "GWFG",     "WHGO",     "CAGO",     "MALL",
"GADW",     "GWTE",     "CITE",     "UNTE",     "AMWI",     "NOPI",
"NOSH",     "RIDU",     "CANV",     "BUFF",     "WODU",     "RUDU",
"EUWI",     "UNDU",     "PBGB",     "SORA",     "COOT",     "COMO",
"AMBI",     "BCNH",     "GBHE",     "SNEG",     "GREG",     "WFIB",
"SACR",     "AMAV",     "BNST",     "BBPL",     "KILL",     "LBCU",
"GRYE",     "LEYE",     "LBDO",     "SNIP",     "DUNL",     "WESA",
"LESA",     "PEEP",     "RUFF",     "UNSH",     "RBGU",     "HEGU",
"CAGU",     "GUSP")

# abundancia de especies
abu <- colSums(RiceField[,todas] > 0, na.rm = TRUE)
abu

# quitar covariables
aves<-RiceField[,todas]

# To reduce the number of variables in the figure, we only used the
# 20 species that occured at more than 40 sites.
# As a result, N = 20. Else it becomes a mess.
aves2<-aves[,abu>100]
N <- ncol(aves2)
AllNames <- names(aves2)
A <- matrix(nrow = N, ncol = N)
for (i in 1:N){
  for (j in 1:N){
    A[i,j] <- sum(RiceField[,todas[i]]==0  & RiceField[,todas[j]]==0, na.rm=TRUE)
    }}
A1 <- A/2035
print(A1, digits = 2)

rownames(A1) <- AllNames
colnames(A1) <- AllNames

library(lattice)
panel.corrgram.2 <- function(x, y, z, subscripts, at = pretty(z), scale = 0.8, ...)
{
    require("grid", quietly = TRUE)
    x <- as.numeric(x)[subscripts]
    y <- as.numeric(y)[subscripts]
    z <- as.numeric(z)[subscripts]
    zcol <- level.colors(z, at = at, ...)
    for (i in seq(along = z))
    {
        lims <- range(0, z[i])
        tval <- 2 * base::pi *
            seq(from = lims[1], to = lims[2], by = 0.01)
        grid.polygon(x = x[i] + .5 * scale * c(0, sin(tval)),
                     y = y[i] + .5 * scale * c(0, cos(tval)),
                     default.units = "native",
                     gp = gpar(fill = zcol[i]))
        grid.circle(x = x[i], y = y[i], r = .1 * scale,
                    default.units = "native")
    }
}

levelplot(A1,xlab=NULL,ylab=NULL,
    at=do.breaks(c(0.5,1.01),101),
    panel=panel.corrgram.2,
    scales=list(x=list(rot=90)),
    colorkey=list(space="top"),
    col.regions=colorRampPalette(c("red","white","blue")))

# en byn
levelplot(A1,xlab=NULL,ylab=NULL,
    at=do.breaks(c(0.5,1.01),101),
    panel=panel.corrgram.2,
    scales=list(x=list(rot=90)),
    colorkey=list(space="top"),
    col.regions=colorRampPalette(c(grey(0.8),grey(0.5),grey(0.2))))

# reproducir Figura 9 & Tabla 1 Zuur et al. 2010
# objeto Sparrow distinto
Sparrows2 <- read.table("VegSamplesV1.txt", header = TRUE)
names(Sparrows2)
# carga tu propia biblioteca
source("HighstatLib.R")
# selecciona covariantes
Z<-Sparrows2[,c("Avgmaxht", "Avgdens", "ht.thatch",
                "S.patens", "Distichlis", "S.alternifloraShort",
                "S.alternifloraTall", "Juncus", "Bare", "Other",
                "Phragmites", "Shrub", "Tallsedge", "Water")]
# colinealidad               
corvif(Z)      #Part of Table 1

# regresi?n lineal
M1<-lm(Banded~Avgmaxht + Avgdens + ht.thatch + S.patens +
              Distichlis + S.alternifloraShort + S.alternifloraTall +
              Juncus + Bare + Other + Phragmites + Shrub + Tallsedge +
               Water, data = Sparrows2)
summary(M1)    #Part of Table 1


# Achurar covariantes
Z<-Sparrows2[,c("ht.thatch", "S.patens", "Distichlis",
                "S.alternifloraShort", "Juncus", "Bare", "Other",
                "Phragmites", "Shrub", "Tallsedge", "Water")]
corvif(Z)      #Part of Table 1


# regresi?n lineal sobre un subconjunto
M2 <- lm(Banded ~ ht.thatch + S.patens +
                  Distichlis + S.alternifloraShort +
                  Juncus + Bare + Other + Phragmites + Shrub + Tallsedge +
                  Water, data = Sparrows2)
summary(M2)    #Part of Table 1

M2 <- lm(Banded ~ Juncus + Shrub, data = Sparrows2)
drop1(M2, test = "F")
coef(M1)
step(M2)

M3 <- lm(Banded ~ Juncus+Shrub, data = Sparrows2)
summary(M3)    #Part of Table 1

# reproducir Figura 9 Zuur et al. 2010
Z <- as.vector(as.matrix(Sparrows2[, c("Avgmaxht", "Avgdens",
              "ht.thatch", "S.patens", "Distichlis",
              "S.alternifloraShort", "S.alternifloraTall", "Juncus",
              "Bare", "Other", "Phragmites", "Shrub", "Tallsedge", "Water")]))

# datos en formato vector para xyplot
Y10 <- rep(Sparrows2$Banded, 14)

MyNames <- names(Sparrows2[,c("Avgmaxht", "Avgdens", "ht.thatch",
                "S.patens", "Distichlis", "S.alternifloraShort",
                "S.alternifloraTall", "Juncus", "Bare", "Other",
                "Phragmites", "Shrub", "Tallsedge", "Water")])

ID10 <- rep(MyNames, each = length(Sparrows2$Banded))
#library(lattice)


ID11 <- factor(ID10, labels = c("% Juncus gerardii",
               "% Shrub", "Height of thatch", "% Spartina patens",
               "% Distichlis", "% Bare ground", "% Other vegetation",
               "% Phragmites australis", "% Tall sedge", "% Water",
               "% Spartina alterniflora (short)",
               "% Spartina alterniflora (tall)",
               "Maximum vegetation height",
               "Vegetation stem density"),
               levels = c("Juncus", "Shrub", "Avgmaxht", "S.patens",
                          "Distichlis", "Bare", "Other", "Phragmites",
                          "Tallsedge", "Water", "S.alternifloraShort",
                          "S.alternifloraTall", "ht.thatch", "Avgdens"))


xyplot(Y10 ~ Z | ID11, col = 1,
  strip = function(bg='white',...) strip.default(bg='white',...),
  scales = list(alternating = T,
                x = list(relation = "free"),
                y = list(relation = "same")),
  xlab = "Covariates",
  par.strip.text = list(cex = 0.8),
  ylab = "Banded",
  panel=function(x, y, subscripts,...){
    panel.grid(h =- 1, v = 2)
    panel.points(x, y, col = 1, pch = 16)
    if(ID10[subscripts][1] != "Tallsedge") {panel.loess(x,y,col=1,lwd=2)}
    })

# con visreg
library(visreg)
visreg(M3,"Juncus",line=list(col="blue",lwd=3),
       points=list(col="lightblue",cex=1.5),
       cex.lab=1.2)
visreg(M3,"Shrub",line=list(col="red",lwd=3),
       points=list(col="pink",cex=1.5),
       cex.lab=1.2)
library(ggplot2)
visreg(M3,"Juncus",gg=T)

# reproducir Figura 10 Zuur et al 2010
# Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)
# source(file = "HighstatLib.R")
MyNames <- c("wing","tarsus","head",
             "culmen","nalospi","weight")
pairs(Sparrows[,c(1, 3, 4, 5, 6, 7)],
      lower.panel = panel.cor,
      cex.labels=1.3,
      labels=MyNames)


# reproducir Figura 11 Zuur et al. 2010
# Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)
# Take the data from species 1, Sex = 0 and Wing length >= 65
I1 <- Sparrows$SpeciesCode == 1 & Sparrows$Sex != "0" & Sparrows$wingcrd < 65
ala<- Sparrows$wingcrd[I1]
peso<- Sparrows$wt[I1]
Mon1<- factor(Sparrows$Month[I1])
Sex1<- factor(Sparrows$Sex[I1])
# Define Month and Sex as categorical variables
mes <- factor(Mon1,levels=c(5,6,7,8,9),
                labels=c("May","Jun","Jul","Ago","Sep"))
sexo   <- factor(Sex1, levels=c(4,5),labels=c("macho","hembra"))

M1 <- lm(peso ~ ala*mes*sexo)
summary(M1)
anova(M1)

# Make the coplot
coplot(peso ~ ala | mes * sexo, ylab = "peso (g)",
       xlab = "ala (mm)",
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })


# reproducir Figura 12 Zuur et al 2010
Waders <- read.table(file = "wader.txt", header = TRUE)
names(Waders)
# Define the time axis
Time <- seq(1,25)

par(mfrow = c(2, 2), mar = c(5, 4, 3, 2))
plot(Time, Waders$C.fuscicolis, type = "l", xlab = "Time (2 weeks)",
     ylab = "C. fuscicollis abundance")
acf(Waders$C.fuscicolis, main = "C. fuscicollis ACF")

plot(Time, Waders$L.dominicanus, type = "l", xlab = "Time (2 weeks)",
     ylab = "L. dominicanus abundance")
acf(Waders$L.dominicanus, main = "L. dominicanus ACF")

### fin 
