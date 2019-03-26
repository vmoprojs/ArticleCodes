###########################################################################
#                       Small Area Estimators                             #
#                              USFQ                                       #
#   VMO (https://sites.google.com/site/moralesonatevictor/)               #
# Codigo adaptado de Nicholas Longford                                    #  
###########################################################################

rm(list=ls())
gc()
graphics.off()
Time <- proc.time()
###################
# Librerias:
###################
# install.packages("rgdal")
# install.packages("spdep")
# install.packages("RColorBrewer")
# install.packages("classInt")
# install.packages("xtable")
library(rgdal)
library(spdep)
library(RColorBrewer)
library(classInt)
library(xtable)

###############################################################################
#                         PROVINCIAS
###############################################################################
dir.global <- "~/Documents/USFQ/Tesis/CodSpacial/Final"
dirmapas <- "~/Documents/DataBase/Mapas1/2012_nxcantones"
setwd(dirmapas)
poligonos <- readOGR("nxcantones.shp",layer="nxcantones")
dir.resul <- paste(dir.global,"/Results",sep = "")
dir.dat <- paste(dir.global,"/Data",sep = "")
setwd(dir.resul)

### Número de simulaciones:
n.sim <- 500
################ Obtengo los subconjuntos #############

############## Subconjunto de los cantones de Guayas:
poligonos <- subset(poligonos,poligonos@data$DPA_CANTON=="0901" |
                      poligonos@data$DPA_CANTON=="0902" | poligonos@data$DPA_CANTON=="0903" |
                      poligonos@data$DPA_CANTON=="0904" | poligonos@data$DPA_CANTON=="0905" |
                      poligonos@data$DPA_CANTON=="0906" |
                      poligonos@data$DPA_CANTON=="0907" |poligonos@data$DPA_CANTON=="0908" |
                      poligonos@data$DPA_CANTON=="0909" |poligonos@data$DPA_CANTON=="0910" |
                      poligonos@data$DPA_CANTON=="0911" |poligonos@data$DPA_CANTON=="0912" |
                      poligonos@data$DPA_CANTON=="0913" |poligonos@data$DPA_CANTON=="0914" |
                      poligonos@data$DPA_CANTON=="0916" |poligonos@data$DPA_CANTON=="0918" |
                      poligonos@data$DPA_CANTON=="0919" |poligonos@data$DPA_CANTON=="0920" |
                      poligonos@data$DPA_CANTON=="0921" |poligonos@data$DPA_CANTON=="0922" |
                      poligonos@data$DPA_CANTON=="0923" |poligonos@data$DPA_CANTON=="0924" |
                      poligonos@data$DPA_CANTON=="0925" )

# Calculo los centroides
centroides <- coordinates(poligonos)
# Acronimos de Provincias:

# QUITO           QU
# CAYAMBE         CA
# MEJIA   	      ME
# PEDRO MONCAYO	  PM
# RUMINAHUI	      RU
# SAN MIGUEL DE LOS BANCOS	SA
# PEDRO VICENTE MALDONADO	  PV
# PUERTO QUITO	            PQ

AcrProv <- c("GU","AL","BO","BA","CO","DA","DU","EM","EL","MI","NR","NA","PA","PE","SB","SA","UR","YA","PL","SI","CM","LO","NO")

graphics.off()

postscript("MapaAp1Gye.ps", width=5.5+2, height=3.9+2, pointsize=8, 
           horizontal=FALSE)
plot(poligonos)
text(centroides,AcrProv,cex=0.7,col="blue")
dev.off()
############################################################################################
############################################################################################
############################################################################################

"Las Provincias de Ecuador y sus vecinas"

# ProVec: CatVei
ProVec<-c("GUAYAQUIL",  "GU",	"PL",	"NR",	"DU",	"SB",	"DA",	"NO",	
          "ALFREDO BAQUERIZO MORENO",	"AL",	"SI",	"MI",	"YA",				
          "BALAO",	"BO",	"NR",						
          "BALZAR",	"BA",	"EM",	"CO",					
          "COLIMES",	"CO",	"BA",	"PA",	"SA",	"PE",			
          "DAULE",	"DA",	"LO",	"NO",	"GU",	"SB",	"UR",	"SA",	
          "DURAN",	"DU",	"GU",	"NR",	"YA",				
          "EMPALME",	"EM",	"BA",						
          "EL TRIUNFO",	"EL",	"NR",	"CM",	"YA",				
          "MILAGRO",	"MI",	"YA",	"CM",	"NA",	"SI",	"AL",		
          "NARANJAL",	"NR",	"BO",	"GU",	"DU",	"EL",	"YA",		
          "NARANJITO",	"NA",	"MI",	"CM",	"SI",				
          "PALESTINA",	"PA",	"UR",	"SA",	"CO",				
          "PEDRO CARBO",	"PE",	"SA",	"CO",					
          "SAMBORONDON",	"SB",	"GU",	"YA",	"UR",	"DA",			
          "SANTA LUCIA",	"SA",	"PE",	"LO",	"DA",	"UR",	"PA",	"CO",	
          "URBINA JADO",	"UR",	"SB",	"DA",	"SA",	"PA",			
          "YAGUACHI",	"YA",	"DU",	"EL",	"NR",	"CM",	"MI",	"AL",	"SB",
          "PLAYAS (GENERAL VILLAMIL",	"PL",	"GU",						
          "SIMON BOLIVAR",	"SI",	"NA",	"MI",	"AL",				
          "CORONEL MARCELINO MARIDUENA",	"CM",	"EL",	"NA",	"MI",	"YA",			
          "LOMAS DE SARGENTILLO",	"LO",	"NO",	"DA",	"SA",				
          "NOBOL (VICENTE PIEDRAHITA)",	"NO",	"GU",	"DA",	"LO")


"Los nombres de los cantones y sus ubicaciones en el vector"
"CatNam: ProNam"
"CatLoc: ProNom"
ProNam <- ProVec[nchar(ProVec)>2]
ProLoc <- c(match(ProNam, ProVec), length(ProVec)+1)
names(ProLoc) <- c(ProNam, c())

"Numero de Provincias"
"nCat: nProv"
nProv <- length(ProNam)

"Los acronimos de las provincias"
"CatAcr: ProAcr"
ProAcr <- ProVec[ProLoc[seq(nProv)]+1]

"Donde termina la lista previa de vecinos"
end <- 0

"Lista de Provincias y sus vecinos"
ProVecL <- list()

lc <- 1
for (cm in ProNam)
{
  lc <- lc+1
  sta <- end + 3
  end <- ProLoc[lc]-1
  
  "Los vecinos de la Porvincia cm"
  ProVecL[[cm]] <- ProVec[sta:end]
}


"Formar la matriz de vecinos/similaridad"
"CatSim: ProSim"
ProSim <- matrix(0, nProv, nProv, dimnames=list(ProAcr, ProAcr))

"Marcamos los vecinos de cada Provincia"
for (lc in seq(nProv))
  ProSim[lc,ProVecL[[lc]]] <- 1


"Encontramos los vecinos una vez removidos"
"CatRem: ProRem"

ProRem <- function(cm, Mat)
{
  
  "  cm   Numero de fila de nombre (acronimo de la Prov)"
  "  Mat  Matriz de similaridad"
  
  
  "Convierte a numero si es necesario"
  if (!is.numeric(cm)) cm <- match(cm, dimnames(Mat)[[1]], nomatch=0)
  
  "Los indices"
  ind <- seq(dim(Mat)[1])
  
  "Los vecinos"
  vei <- ind[Mat[cm,]>0]
  
  "Los vecinos de los vecinos"
  vei2 <- Mat[vei,]==1
  
  if (length(vei)>1)  vei2 <- apply(vei2,2,max)
  
  Mat[cm,] + (max(Mat)+vei2) * (Mat[cm,]==0)*(vei2>0)
}
"Fin de la funcion ProRem"


#// PRUEBA: ProRem#
# a <- c(0,1,0,1,0);b <- c(1,0,1,0,0);c <- c(0,1,0,0,0); d <- c(1,0,0,0,1) ; e <- c(0,0,0,1,0)
# Mat <- matrix(c(a,b,c,d,e),ncol=5)
# Mat <- apply(matrix(1:5,nrow=1),2,ProRem, Mat)
# while (min(Mat)==0)
#  Mat <- apply(matrix(1:5,nrow=1),2,ProRem, Mat) # ProSim es el argumento de ProRem
#// FIN PRUEBA:ProRem#

while (min(ProSim)==0)
  ProSim <- apply(matrix(seq(nProv),nrow=1),2,ProRem, ProSim) # ProSim es el argumento de ProRem

"Ponemos etiquetas"
dimnames(ProSim) <- list(ProAcr, ProAcr)

"Distancia consigo misma es cero"
diag(ProSim) <- 0

############################################################################################
############################################################################################
############################################################################################

##  datos: Datos del Censo de Poblacion y Vivienda
##  Anio: 2010



##  Entrada de datos y tomo 2010
setwd(dir.dat)
datos <- read.csv("datosCPV10Gye.csv",header=TRUE,sep=";")
datos$Cod <- poligonos@data$DPA_PROVIN

# > names(datos)
# [1] "Cod"                    "Nombre.de.la.Provincia" "Indig"                  "Afro"                  
# [5] "Negrx"                  "Mulatx"                 "Montubix"               "Mestizx"               
# [9] "Blancx"                 "Otrx"

LlarC <- as.matrix(datos[,3:4])
rownames(LlarC) <- AcrProv

##  Chequeo el calculo de la Proporcion de Adultos mayores
LlarH <- LlarC[,2]/LlarC[,c(seq(2))] %*% matrix(rep(1,2))


############################################################################################
############################################################################################
############################################################################################
# Mapa de distribucion provincial de indigenas:


indig <- as.data.frame(as.vector(LlarH))
names(indig) <- "indig"
row.names(indig) <- row.names(poligonos)
poligonos.data <- SpatialPolygonsDataFrame(poligonos,indig)


plotvar <- poligonos.data$indig
nclr <- 5 # Numero de colores
plotclr <- brewer.pal(nclr,"Blues")
class <- classIntervals(round(plotvar,3)*100,nclr,style="kmeans") # Aqui fijo el numero de decimales

colcode <- findColours(class,plotclr) # defino paleta de colores

setwd(dir.resul)
jpeg("PropIndMapGye.jpeg",quality=100,height = 600,width = 800)
plot (poligonos.data, col=colcode, border="grey", axes=T)
title(main = "Prop. de Adultos mayores",cex=3)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=1.25)
text(centroides,AcrProv,cex=1)
dev.off()


graphics.off()

postscript("MapaGye.ps", width=5.5+2, height=3.9+2, pointsize=8, 
           horizontal=FALSE)
plot (poligonos.data, col=colcode, border="grey", axes=T)
# title(main = "Prop. de Adultos mayores",cex=3)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=1.25)
text(centroides,AcrProv,cex=1)
dev.off() 

############################################################################################
############################################################################################
############################################################################################

## Figura 2  Pag. 6


##  Convierte a porcentajes:
Perc <- function(vec)
  100*vec/sum(vec)

graphics.off()

postscript("Fig2Ap1Gye.ps", width=5.5, height=3.9, pointsize=8, 
           horizontal=FALSE)

par(mfrow=c(2,1), mar=c(4,4,1,1), mgp=c(3,1,0)*0.7, lab=c(8,3,1))

##  Tamano de la poblacion:
LlarH1 <- as.vector(LlarH)
names(LlarH1) <- AcrProv
barplot((LlarC[,1]),space=0,col=rainbow(nProv),
        xlab="Cantones", ylab="Num. de AM",cex.names=0.6)
barplot(LlarH1,space=0,col=rainbow(nProv),
        xlab="Cantones", ylab="Porp AM",cex.names=0.6)

dev.off()

############################################################################################
############################################################################################
############################################################################################


## Figura 3 pag 19

## Estimacion compuesta Minimax de la media de las medias nacional


NParc <- function(nD=round(CatC07/4000), B2=seq(0,0.03,length=101), B2m=0.02)
{
  ##  El estimador Minmax, asumiendo que vD=1/nD
  
  ##  nD    Tamanos de las muestras dentro de los distritos
  ##  B2    Puntos a plotearse del sesgo al cuadrado
  ##  B2m   El sesgo al cuadrado mas grande posible (posibles varios valores)
  
  
  ##  Puntos para el calculo y grafica
  B2 <- sort(unique(B2))
  
  ##  Numero de distritos y tamano de muestra total
  D <- length(nD)
  nDs <- sum(nD)
  
  ##  El factor de varianza
  fct <- 10^4
  
  ##  La varianza de un estimador insesgado (constante)
  vrA <- sum(fct/nD)/D^2
  
  ##  El EMC (MSE) del estimador sesgado (en funcion de B2)
  msB <- fct/nDs + B2
  
  ##  El coeficiente de composicion minimax y su EMC
  cbu <- (vrA - fct/nDs)/(vrA - fct/nDs + B2m)
  
  msu <- c()
  for (cb in cbu)
    msu <- cbind(msu, (1-cb)^2*vrA + 2*cb*(1-cb)*fct/nDs + cb^2*msB)
  
  if (length(cbu)==1)
    msu <- matrix(msu)
  
  plot(range(B2)*c(1,1.075), range(c(vrA, msB, msu))/c(1,4), type="n", 
       xlab="Sesgo al cuadrado", ylab="EMC", 
       cex.lab=0.9, cex.axis=0.9)
  
  ##  La varianza
  segments(min(B2), vrA, max(B2), vrA, lty=5, lwd=0.6)
  
  ##  Los EMCs (no-constante)
  lines(B2, msB, lty=5, lwd=0.6)
  
  for (ii in seq(length(cbu)))
  {
    lines(B2, msu[,ii], lty=1, lwd=0.25)
    whi <- (B2 < B2m[ii])
    lines(B2[whi], msu[whi,ii], lty=1, lwd=1.75)
  }
  
  ##  EL liminte inferior del EMC
  LBs <- vrA - (vrA - fct/nDs)^2 / (vrA - fct/nDs + B2)
  lines(B2, LBs, lty=3, lwd=1.25)
  
  ##  Identifa las lineas
  text(6.5, 8.75, "B", cex=0.7)
  
  text(rep(max(B2)*1.02,1+length(B2m)), c(vrA, apply(msu,2,max))+0.08, 
       c("A", paste("C", rev(seq(length(B2m))), sep="")), cex=0.7, adj=0)  
  
  text(5, 4.6, "L",  cex=0.7, adj=0)  
  
  ##  El sesgo al cuadrado mas grande posible
  abline(v=B2m, lty=2, lwd=0.3)
  
  
  ##  Los pesos en la composicion
  nC <- (1-cbu[1]) + cbu[1]*nD
  
  plot(c(0,D+1), range(c(1,nD)), type="n", 
       xlab="Distrito", ylab="Peso", log="y", 
       sub="(orden ascendente del tama?o de la poblacion)", 
       cex.lab=0.9, cex.axis=0.9, cex.sub=0.75
  )
  
  segments(1,1,D,1, lwd=1)
  points(seq(D), sort(nD), pch=6, cex=0.5)
  points(seq(D), sort(nC), pch=4, cex=0.5)
  
  nCx <- c()
  ##  Para el otro limite superior
  ##  los pesos en la composicion
  if (length(cbu)>1)
  {
    nCx <- (1-cbu[2]) + cbu[2]*nD
    
    segments(1,1,D,1, lwd=1)
    points(seq(D), sort(nCx), pch=1, cex=0.35)
    
  }  ##  fin del loop
  
  list(MSEs=cbind(A=vrA, B=msB, C=msu), Weights=cbind(nA=1, nB=nD, nC=nC, 
                                                      nC=nCx))
}  ##  Fin de la funcion  NParc

graphics.off()

postscript("Fig3Ap1Gye.ps", 
           width=5.5, height=3.1, pointsize=10, horizontal=F)

# LlarC[,1] Son los totales de poblacion por Provincia
par(mfrow=c(1,2), mar=c(4.4,3.6,1,0.8), mgp=0.7*c(3,1,0), lab=c(3,3,1))
NParcR <- NParc(nD=round(LlarC[,1]/20), 
                B2=seq(0,8, length=101), B2m=c(25, 9))

legend(0,100000, pch=c(-1,6,4,1), c("Pesos", "Prop. al tamano de n", 
                                    "Optimo (minimax); B<5)", "Optimo (minimax); B<3)"), 
       cex=0.725)

dev.off()


############################################################################################
############################################################################################
############################################################################################

##  Estimacion directa
Subt <- function (vc1, vc2)   vc1 - vc2

##  Ubicar datos en una matriz.
ExtrE <- function(mat, rw=1, cl=1)
  ##  Funcion para extraer un elemento o submatriz
  mat[rw, cl]

# Obtiene la media y la varianza:
MeVa <- function(vec)
  c(Mean=mean(vec), St.dev.=sqrt(var(vec)))



############################################################################################
############################################################################################
############################################################################################

##  Funion para comparar dos vectores de EMCs

CompV <- function(vc1, vc2, dgt=5, plt=T, smb=ProAcr)
{
  ##   Funcion para comparar dos vectores con entradas positivas y del mismo tama?o
  ##   Uso princial:  salida de la funcion SAcs
  ##   vc1,  vc2    vectores
  ##   dgt      Numero de digitos para el redondeo 
  ##   plt      Logico, TRUE si se quiere plotear
  ##   smb      Simbolos usados
  
  ##  Compara longitudes
  if (length(vc1) != length(vc2)) 
    stop(paste("Los vectores tienen tamano desigual -", length(vc1), length(vc2),"."))
  
  if (min(vc1) < 0) stop("El Vector 1 tiene una entrada no positiva")
  if (min(vc2) < 0) stop("El Vector 2 tiene una entrada no positiva")
  
  ##  Trabajamos con las raices de los EMCs
  vc1 <- sqrt(vc1)
  vc2 <- sqrt(vc2)
  
  if (plt)
  {
    rng <- range(c(vc1, vc2))
    
    par(mfrow=c(1,2), mar=c(4,4,1,1), mgp=0.7*c(3,1,0), lab=c(2,2,0))
    
    xy <- c("","xy")
    
    ##  Para las lineas del plot
    pts <- seq(rng[1], rng[2], length=200)
    
    for (i in seq(2))
    {
      plot(vc1, vc2, xlim=rng, ylim=rng, type="n", log=xy[i], 
           xlab="Vector 1", ylab="Vector 2")
      
      lines(pts, pts*1.20, lty=2, lwd=0.4)
      lines(pts, pts*0.80, lty=2, lwd=0.4)
      
      lines(pts, pts*2, lty=5, lwd=0.4)
      lines(pts, pts/2, lty=5, lwd=0.4)
      
      abline(0,1,lty=3, lwd=0.8)
      
      text(vc1, vc2, smb, cex=0.6)
    }  ##  Final del grafico - loop
  }  ##  Final del grafico
  
  
  lapply(list(
    Listing=rbind(vc1, vc2, Ratio=vc1/vc2)[,sort.list(vc1)], 
    Sort.by.ratio=sort(vc1/vc2), 
    Abs.MSE=sum(abs(vc1-vc2)), 
    Mean.diff.=MeVa(vc2-vc1),
    Desc1 = c(quantile(vc1, c(0,25,50,75,100)/100), mean=mean(vc1)), 
    Desc2 = c(quantile(vc2, c(0,25,50,75,100)/100), mean=mean(vc2))
  ), round, dgt)
}  ##  Final de la funcion  CompV



############################################################################################
############################################################################################
############################################################################################

##  Estimacion de area pequena para las proporciones de ind?genas
##  Paso 1. --  generar 500 muestras

SSRSe <- function(tbl, fra, ctg, vls)
{
  ##  Toma una muestra aleatoria estratificada y evalua el estimador directo
  ##       para la proporcion de indigenas y para la media
  ##  tbl     La base de datos (vector/tabulacion de conteos)
  ##  fra     Las fracciones muestrales (provincias)
  ##  ctg     Las categorias objetivo, para las cuales se estima la proporcion 
  ##  vls     Los valores asociados a cada clase
  
  ##  La poblacion total
  sN <- sum(tbl)
  
  ##  Las unidades selecionadas
  whi <- seq(sN)[runif(sN) < fra]
  
  ##  Los puntos de corte
  ctp <- cumsum(tbl)
  
  ##  Configuracion Inicial
  smp <- rep(1, length(whi))
  
  ##  Asinacion a las categorias
  for (ct in ctp)
    smp <- smp + (whi>ct)
  
  ## Tabulacion
  tb <- table(smp)
  
  ##  Prevision de entradas con cero 
  names(tbl) <- seq(length(tbl))
  tbl <- tbl*0
  tbl[names(tb)] <- tb
  
  ##  El tamano de la muestra
  Ssz <- sum(tbl)
  
  ##  La proporcion de observaciones en las categorias objetivo
  Spr <- sum(tbl[match(ctg, names(tbl), nomatch=0)])/(Ssz + (Ssz==0))
  
  ##  Prevision de una submuestra vacia
  
  Smn <- sum(tbl*vls)/Ssz
  Svr <- (sum(tbl*vls^2)/Ssz - Smn^2)/(Ssz-1)
  if (Ssz<1.1)  Svr <- 1
  if (Ssz<0.1)  Smn <- 2
  
  list(Sample=tbl, Smean=Smn, Svar=Svr, Ssize=Ssz)
  
  c(Sprop=Spr, Smean=Smn, Svar=Svr, Ssize=Ssz)
}   ##  Final de la funcion   SSRSe


SSrwV <- list()

for (i in seq(n.sim))
  SSrwV[[i]] <- apply(LlarC[,1:2], 1, SSRSe, fra=1/200, ctg=1, vls=rep(1,2))


############################################################################################
############################################################################################
############################################################################################

##  Composicion Univariada para las proporciones -- varianza agrupada


SAcsAb <- function(drE, trg=SAproTrg)
{
  ##  Estimacion compuesta  (simetria compuesta)
  ##  Proporcion nacional estimada
  ##    drE     Estimadores directos y tamanos muestrales
  ##    trg     Los objetivos
  
  ##  Los tamanos muestrales
  nD <- dim(drE)[2]
  nObs <- sum(drE[4,])
  
  ##  La proporcion nacional, componentes del estimador
  theA <- mean(drE[1,])
  theB <- sum(drE[1,]*drE[4,])/nObs
  
  ##  Varianza agrupada dentro de los distritos
  sWp <- theB*(1-theB)
  
  ##  Estimador de las varianzas agrupadas de los estimadores directos
  vd <- sWp/(drE[4,]+0.1*(drE[4,]==0))
  
  ##  Las varianzas de los estimadores nacionales
  vA <- mean(vd)/length(vd)
  vB <- sWp/nObs
  
  ##  Los coeficientes de la composicion
  bthe <- (vA - vB) / (vA - vB + (theA-theB)^2)
  
  ##  El estimador compuesto de la media nacional
  theC <- (1-bthe)*theA + bthe*theB
  
  ##  La covarianza en el coeficiente truncado
  qd <- (1-bthe)/nD + bthe*drE[4,]/nObs
  Cd <- vd * qd
  
  ##  Estimadores de la varianza a nivel de distrito
  dvs <- matrix(drE[1,]-theC)
  siA <- mean(dvs^2) - (theA-theB)^2 - mean(vd) + 2 * mean(Cd)
  siA <- max(siA,0)
  
  siB <- sum(drE[4,]*dvs^2)/nObs - (theA-theB)^2 - (nD-2)/nObs*sWp
  siB <- max(siB,0)
  
  ##  Las varianzas muestrales de los estimadores de varianza
  Imat <- diag(rep(1, nD)) - matrix(1, nD) %*% matrix(qd,1)
  Amat <- t(Imat) %*% Imat / nD
  
  Bmat <- t(Imat) %*% diag(drE[4,]) %*% Imat / nObs
  
  ##  La varianza de siA
  VA <- 2 * matrix(vd,1) %*% Amat %*% Amat %*% matrix(vd) + 
    4 * t(dvs) %*% Amat %*% diag(vd) %*% Amat %*% dvs
  
  ##  La varianza de siB
  VB <- 2 * matrix(vd,1) %*% Bmat  %*% Bmat %*% matrix(vd) + 
    4 * t(dvs) %*% Bmat %*% diag(vd) %*% Bmat  %*% dvs
  
  ##  La covarianza de siA y siB
  CAB <- 2 * matrix(vd,1) %*% Amat  %*% Bmat %*% matrix(vd) + 
    4 * t(dvs) %*% Amat %*% diag(vd) %*% Bmat %*% dvs
  
  ##  El coeficiente de la composicion para  $\sigma^2_0$
  bsi <- (VA-CAB) / (VA - 2*CAB + VB + (siA-siB)^2)
  bsi <- min(bsi,1)
  
  ##  El estimador compuesto de $\sigma^2_0$
  si0 <- (1-bsi)*siA + bsi*siB
  
  ##  La varianza de theC
  vv <- mean(vd)/nD 
  v <- vv - (vv - sWp/nObs)^2/(vv - sWp/nObs +(theA-theB)^2)
  
  ##  Los coeficientes truncados
  bds <- (vd-Cd) / (vd-2*Cd+v + bthe^2*(theA-theB)^2+ si0)
  bds[bds<0] <- 0
  
  ##  Los errores de estimacion
  (1-bds) * drE[1,] + bds * theC - trg
}  ##  Fin de la funcion  SAscAb



############################################################################################
############################################################################################
############################################################################################

##  Estimacion compuesta univariada (similaridad de distancia)
##  Estimacion de las proporciones 

SAcsFb <- function(drE, trg=SAproTrg, Psz=apply(LlarC,1,sum), Dmat=ProSim, mxD=3)
{
  ##  Estimador compuesto  (simetria compuesta)
  ##  Estimador de la media nacional
  ##    drE     Estimadores directos
  ##    trg     Los objetivos
  ##    Psz     Los tamanos poblacionales de los distritos
  ##   Dmat   Matriz de distancia
  ##   mxD    La maxima distancia (trunProvion)
  
  ##  Los tamanos muestrales
  nD <- dim(drE)[2]
  nObs <- sum(drE[4,])
  
  ##  La propporcion nacional
  theA <- mean(drE[1,])
  theB <- sum(drE[1,]*drE[4,])/nObs
  
  ##  La varianza agrupada dentro de los distritos 
  sWp <- theB*(1-theB)
  
  ##  La estimacion de la varianza agrupada de los estimadores directos
  vd <- sWp/(drE[4,]+0.1*(drE[4,]==0))
  
  ##  Las varianzas de los estimadores nacionales
  vA <- mean(vd)/length(vd)
  vB <- sWp/nObs
  
  ##  El coeficiente de la composicion
  bthe <- (vA - vB) / (vA - vB + (theA-theB)^2)
  
  ##  El estimador compuesto de la media nacional
  theC <- (1-bthe)*theA + bthe*theB
  
  ##  Los pesos de cada estimador directo en  $\tilde{\theta}$
  qd <- (1-bthe)/nD + bthe*drE[4,]/nObs
  
  ##  La covarianza en el ceficiente truncado
  Cd <- vd * qd
  
  ##  Estimadores de la varianza a nivel de distrito
  dvs <- matrix(drE[1,]-theC)
  siA <- mean(dvs^2) - (theA-theB)^2 - mean(vd) + 2 * mean(Cd)
  siA <- max(siA,0)
  
  siB <- sum(drE[4,]*dvs^2)/nObs - (theA-theB)^2 - (nD-2)/nObs*sWp
  siB <- max(siB,0)
  
  ##  Las varianzas muestrales de los estimadores de varianza
  Imat <- diag(rep(1, nD)) - matrix(1, nD) %*% matrix(qd,1)
  Amat <- t(Imat) %*% Imat / nD
  
  Bmat <- t(Imat) %*% diag(drE[4,]) %*% Imat / nObs
  
  ##  La varianza de siA
  VA <- 2 * matrix(vd,1) %*% Amat %*% Amat %*% matrix(vd) + 
    4 * t(dvs) %*% Amat %*% diag(vd) %*% Amat %*% dvs
  
  ##  La varianza de siB
  VB <- 2 * matrix(vd,1) %*% Bmat  %*% Bmat %*% matrix(vd) + 
    4 * t(dvs) %*% Bmat %*% diag(vd) %*% Bmat  %*% dvs
  
  ##  La covarianza de de siA y siB
  CAB <- 2 * matrix(vd,1) %*% Amat  %*% Bmat %*% matrix(vd) + 
    4 * t(dvs) %*% Amat %*% diag(vd) %*% Bmat %*% dvs
  
  ##  El coeficiente de la composicion para $\sigma^2_0$
  bsi <- (VA-CAB) / (VA - 2*CAB + VB + (siA-siB)^2)
  bsi <- min(bsi,1)
  
  ##  El estimador compuesto de $\sigma^2_0$
  si0 <- (1-bsi)*siA + bsi*siB
  
  ##  La varianza de theC
  vv <- mean(vd)/nD 
  v <- vv - (vv - sWp/nObs)^2/(vv - sWp/nObs +(theA-theB)^2)
  
  ##  Los coeficientes truncados
  bds <- (vd-Cd) / (vd-2*Cd+v + bthe^2*(theA-theB)^2+ si0)
  bds[bds<0] <- 0
  
  ##  Truncar las entradas de la matriz de distancias
  Dmat[Dmat>mxD] <- mxD
  
  ##  Exclusion de vacios
  whi <- drE[4,]>0 
  nE <- sum(whi)
  
  ##  Estimacion de las covarianzas de la distancia
  DS1 <- 
    tapply((rep(drE[1,whi], nE) - rep(drE[1,whi], rep(nE,nE)))^2, Dmat[whi,whi], sum)[-1]
  DS2 <- 2 * sWp * 
    tapply(rep(1/drE[4,whi], nE) + rep(1/drE[4,whi],rep(nE,nE)), Dmat[whi,whi], sum)[-1]
  DS3 <- table(Dmat[whi,whi])[-1]
  
  ##  Las covarianzas
  gaH <- si0 - (DS1 - DS2)/DS3/2
  gaH[gaH<0] <- 0
  gaH[gaH>0.5*si0] <- 0.5*si0
  
  ##  La matriz de covarianza ajustada
  CVmat <- matrix(c(si0,gaH)[Dmat+1], nD, nD)
  
  theDS <- c()
  
  
  ##  Calculos separados para cada distrito
  
  ##  Calculos separados para cada sujeto
  for (i in seq(nD))
  {
    ##  Los tamanos de la poblacion para los h-anillos (denominadores)
    PszH <- tapply(Psz, Dmat[i,], sum)
    
    ##  Las medias muestrales de los h-anillos
    theH <- tapply(Psz*drE[1,], Dmat[i,], sum) / PszH
    
    ##  Las varianzas muestrales de los h-anillos
    vdH <- sWp * tapply(Psz^2/(drE[4,]+0.1*(drE[4,]==0)), Dmat[i,], sum) / PszH^2
    
    ##  Estimacion de la desviacion al cuadrado de los h-anillos del objetivo
    EvdH <- 0
    
    for (di in sort(unique(Dmat[i,]))[-1])
    {
      ##  Los distritos en los h-anillos
      whi <- Dmat[i,]==di
      
      ##  Las fracciones del tamano poblacional
      rdh <- matrix(Psz[whi]/sum(Psz[whi]))
      
      ##  El estimador de la distancia al cuadrado
      EvdH <- c(EvdH, as.vector(t(rdh) %*% CVmat[whi,whi] %*% rdh) + si0 - 2*gaH[di])
      
    }  ##  Fin del loop sobre las distancias (di)
    
    ##  Los coeficientes truncados
    udh <- sWp/(drE[4,i]+0.1*(drE[4,i]==0))
    udh <- udh/(vdH+ EvdH)
    
    ##  El estimador DS
    theDS <- c(theDS, sum(udh*theH) / sum(udh))
  }  ##  Fin del loop sobre los distritos
  
  ##  Los errores de estimacion
  theDS - trg
  
}  ##  Fin de la funcion  SAscFb


############################################################################################
############################################################################################
############################################################################################

# Se importa la informacion auxiliar del 2001
setwd(dir.dat)
datos01 <- read.csv("datosCPV01Gye.csv",sep=";",header=TRUE)
datos01$Cod <- poligonos@data$DPA_PROVIN

Llar01 <- datos01[,3:4]
rownames(Llar01) <- AcrProv

############################################################################################
############################################################################################
############################################################################################
##  Estimaci?n compuesta bivariada (simetr?a compuesta)
##  Estimaci?n de las proporciones con informaci?n auxiliar del censo


SAcs2Ab <- function(drE, Aux=Llar01[,2]/apply(Llar01,1,sum), trg=SAproTrg, tkn=0)
{
  ##  Estimaci?n compuesta bivariada (simetria compuesta)
  ##  Estimaci?n de la media nacional
  ##    drE     Estimadores directos
  ##    Aux    Informacion auxiliar (asumiendo la variancion de la muestra)
  ##    trg     Los objetivos
  ##    tkn    La varianza de la muestra para la informacion auxiliar
  
  ##  Los tamanos muestrales
  nD <- dim(drE)[2]
  nObs <- sum(drE[4,])
  
  ##  La media nacional, componentes del estimador
  theA <- mean(drE[1,])
  theB <- sum(drE[1,]*drE[4,])/nObs
  
  ##  La varianza agrupada dentro de los distritos
  sWp <- theB*(1-theB)
  
  ##  La varianza agrupada estimada de los estimadores directos
  vd <- sWp/(drE[4,]+0.1*(drE[4,]==0))
  
  ##  La media a nivel de distrito y la varianza para la variable auxiliar
  mAux <- mean(Aux)
  vAux <- var(Aux)
  
  ##  Las varianzas de los estimadores nacionales
  vA <- mean(vd)/length(vd)
  vB <- sWp/nObs
  
  ##  La covarianza a nivel de distrito
  cAux <- cov(drE[1,], Aux)
  
  ##  El coeficiente de la composicion
  bthe <- (vA - vB) / (vA - vB + (theA-theB)^2)
  
  ##  El estimador compuesto de la media nacional
  theC <- (1-bthe)*theA + bthe*theB
  
  ##  La covarianza en el coeficiente truncado
  qd <- (1-bthe)/nD + bthe*drE[4,]/nObs
  Cd <- vd * qd
  
  ##  Estimadores de la varianza a nivel de distrito
  dvs <- matrix(drE[1,]-theC)
  siA <- mean(dvs^2) - (theA-theB)^2 - mean(vd) + 2 * mean(Cd)
  siA <- max(siA,0)
  
  siB <- sum(drE[4,]*dvs^2)/nObs - (theA-theB)^2 - (nD-2)/nObs*sWp
  siB <- max(siB,0)
  
  ##  Las varianzas muestrales de los estimadores de varianza
  Imat <- diag(rep(1, nD)) - matrix(1, nD) %*% matrix(qd,1)
  Amat <- t(Imat) %*% Imat / nD
  
  Bmat <- t(Imat) %*% diag(drE[4,]) %*% Imat / nObs
  
  ##  La varianza de siA
  VA <- 2 * matrix(vd,1) %*% Amat %*% Amat %*% matrix(vd) + 
    4 * t(dvs) %*% Amat %*% diag(vd) %*% Amat %*% dvs
  
  ##  La varianza de siB
  VB <- 2 * matrix(vd,1) %*% Bmat  %*% Bmat %*% matrix(vd) + 
    4 * t(dvs) %*% Bmat %*% diag(vd) %*% Bmat  %*% dvs
  
  ##  La covarianza de siA y siB
  CAB <- 2 * matrix(vd,1) %*% Amat  %*% Bmat %*% matrix(vd) + 
    4 * t(dvs) %*% Amat %*% diag(vd) %*% Bmat %*% dvs
  
  ##  El coeficiente de la composicion para  $\sigma^2_0$
  bsi <- (VA-CAB) / (VA - 2*CAB + VB + (siA-siB)^2)
  bsi <- min(bsi,1)
  
  ##  El estimador compuesto de $\sigma^2_0$
  si0 <- (1-bsi)*siA + bsi*siB
  
  ##  La varianza de theC
  vv <- mean(vd)/nD 
  v <- vv - (vv - sWp/nObs)^2/(vv - sWp/nObs +(theA-theB)^2)
  
  ## Incrementa  si0  tal que la correlacion con Aux no sea mayor a 0.95
  Sig <- matrix(c(max(si0, 1/0.95^2 * cAux^2 / vAux), cAux, cAux, vAux), 2,2)
  
  ##  La varianza muestral "total"
  VV <- matrix(c(vv,0,0,tkn/nD),2,2)
  
  ##  Los coeficientes truncados
  bds <- (vd-Cd) / (vd-2*Cd+v + bthe^2*(theA-theB)^2+ si0)
  bds[bds<0] <- 0
  
  ##  Para guardar los estimadores
  Esti <- c()
  
  ##  Calculos diferentes para cada distrito
  for (di in seq(nD))
  {
    VVd <- matrix(c(vd[di],0,0,tkn), 2,2)
    CCd <- matrix(c(Cd[di],rep(0,3)), 2,2)
    
    ##  El truncamiento bivariado
    Bd <- solve(VVd + VV - 2*CCd + Sig, VVd - CCd)[,1]
    
    Esti <- c(Esti, 
              sum((c(1,0) - Bd) * c(drE[1,di], Aux[di])) + sum(Bd*c(theC, mAux))  )
  }  ##  Fin de los calculos para los distritos
  
  ##  Los errores de estimacion
  Esti - trg
}  ##  Fin de la funci?n  SAsc2Ab



############################################################################################
############################################################################################
############################################################################################


##  Estimacion compuesta bivariada (similaridad de distancia)
##  Resultados binarios (si es ind?gena)


SAcs2Fb <- function(drE, Aux=Llar01[,2]/apply(Llar01,1,sum), trg=SAproTrg, 
                    Psz=apply(LlarC,1,sum), Dmat=ProSim, tkn=0.00001, mxD=3)
{
  ##  Estimacion compuesta  (simetria compuesta)
  ##  Estimacion de la media nacional
  ##    drE     Estimadores directos
  ##    Aux    Variable auxiliar
  ##    trg      Los objetivos
  ##    Psz     Tamanos de la poblaci?n por distrito
  ##   Dmat   La matriz de distancia
  ##    tkn      La varianza de la muestra (a ser anadida a la var. de aux)
  ##   mxD    La maxima distancia (trunProvion)
  
  ##  Los tamanos muestrales
  nD <- dim(drE)[2]
  nObs <- sum(drE[4,])
  
  ##  La proporcion nacional, componentes del estimador
  theA <- mean(drE[1,])
  theB <- sum(drE[1,]*drE[4,])/nObs
  
  ##  La varianza agrupada dentro de los distritos
  whi <- drE[4,] > 1
  sWp <- theB*(1-theB)
  
  ##  El estimador de varianza agrupada de los estimadores directos
  vd <- sWp/(drE[4,]+0.1*(drE[4,]==0))
  
  ##  Las varianzas de los estimadores nacionales
  vA <- mean(vd)/length(vd)
  vB <- sWp/nObs
  
  ##  El coeficiente de la composicion
  bthe <- (vA - vB) / (vA - vB + (theA-theB)^2)
  
  ##  El estimador compuesto de la media nacional
  theC <- (1-bthe)*theA + bthe*theB
  
  ##  Los pesos de cada estimador directo en $\tilde{\theta}$
  qd <- (1-bthe)/nD + bthe*drE[4,]/nObs
  
  ##  La covarianza en los coeficientes truncados
  Cd <- vd * qd
  
  ##  Estimadores de la varianza a nivel de distrito
  dvs <- matrix(drE[1,]-theC)
  siA <- mean(dvs^2) - (theA-theB)^2 - mean(vd) + 2 * mean(Cd)
  siA <- max(siA,0)
  
  siB <- sum(drE[4,]*dvs^2)/nObs - (theA-theB)^2 - (nD-2)/nObs*sWp
  siB <- max(siB,0)
  
  ##  Las varianzas muestrales de los estimadores de varianza
  Imat <- diag(rep(1, nD)) - matrix(1, nD) %*% matrix(qd,1)
  Amat <- t(Imat) %*% Imat / nD
  
  Bmat <- t(Imat) %*% diag(drE[4,]) %*% Imat / nObs
  
  ##  La varianza de siA
  VA <- 2 * matrix(vd,1) %*% Amat %*% Amat %*% matrix(vd) + 
    4 * t(dvs) %*% Amat %*% diag(vd) %*% Amat %*% dvs
  
  ##  La varianza de siB
  VB <- 2 * matrix(vd,1) %*% Bmat  %*% Bmat %*% matrix(vd) + 
    4 * t(dvs) %*% Bmat %*% diag(vd) %*% Bmat  %*% dvs
  
  ##  La covarianza de siA y siB
  CAB <- 2 * matrix(vd,1) %*% Amat  %*% Bmat %*% matrix(vd) + 
    4 * t(dvs) %*% Amat %*% diag(vd) %*% Bmat %*% dvs
  
  ##  El coeficiente compuesto para  $\sigma^2_0$
  bsi <- (VA-CAB) / (VA - 2*CAB + VB + (siA-siB)^2)
  bsi <- min(bsi,1)
  
  ##  El estimador compuesto de $\sigma^2_0$
  si0 <- (1-bsi)*siA + bsi*siB
  
  ##  La varianza de theC
  vv <- mean(vd)/nD 
  v <- vv - (vv - sWp/nObs)^2/(vv - sWp/nObs +(theA-theB)^2)
  
  ##  Los coeficientes truncados
  bds <- (vd-Cd) / (vd-2*Cd+v + bthe^2*(theA-theB)^2+ si0)
  bds[bds<0] <- 0
  
  ##  Entradas de la matriz de distancia truncadas
  Dmat[Dmat>mxD] <- mxD
  
  ##  Excluimos vacios
  whi <- drE[4,]>0 
  nE <- sum(whi)
  
  ##  Estimacion de las corarianzas relacionadas con la distancia
  DS1 <- 
    tapply((rep(drE[1,whi], nE) - rep(drE[1,whi], rep(nE,nE)))^2, Dmat[whi,whi], sum)[-1]
  
  DS2 <- 2 * sWp * 
    tapply(rep(1/drE[4,whi], nE) + rep(1/drE[4,whi],rep(nE,nE)), Dmat[whi,whi], sum)[-1]
  
  DS3 <- table(Dmat[whi,whi])[-1]
  
  
  ##  La media y varianza a nivel de distrito para la variable auxiliar
  mAux <- mean(Aux)
  vAux <- var(Aux)
  
  ##  La covarianza a nivel de distrito
  cAux <- mean((drE[1,]-theC)*(Aux-mAux))
  
  ##  La matriz de varianza a nivel de distrito
  Sig <- matrix(c(max(si0, 1/0.95^2 * cAux^2 / vAux), rep(cAux,2), vAux),2,2)
  
  ##  Las covarianzas
  gaH1 <- si0 - (DS1 - DS2)/DS3/2
  gaH1[gaH1<0] <- 0
  gaH1[gaH1>0.5*si0] <- 0.5*si0
  
  gaH2 <- Sig[2,2] - tapply((rep(Aux[whi], nE) - rep(Aux[whi], rep(nE, nE)) )^2, 
                            Dmat[whi,whi], sum)[-1] / table(Dmat[whi,whi])[-1]/2
  
  gaH2[gaH2 < 0] <- 0
  gaH2[gaH2>0.5*vAux] <- 0.5*vAux
  
  ##  Para las covarianzas
  gaH12 <- Sig[1,2] - tapply((rep(drE[1,whi], nE) - rep(drE[1,whi], rep(nE,nE))) * 
                               (rep(Aux[whi], nE) - rep(Aux[whi], rep(nE, nE))), Dmat[whi,whi], sum)[-1] /
    table(Dmat)[-1]/2
  
  
  whi <- gaH12 > 0.95*sqrt(gaH1 * gaH2) 
  gaH12[whi] <- 0.95*sqrt(gaH1 * gaH2)[whi]
  
  whi <- gaH12 < -0.95*sqrt(gaH1 * gaH2) 
  gaH12[whi] <- -0.95*sqrt(gaH1 * gaH2)[whi]
  
  ##  La matriz de covarianza ajustada para las dos variables
  CVmat1 <- matrix(c(si0,gaH1)[Dmat+1], nD, nD)
  CVmat2 <- matrix(c(Sig[2,2],gaH2)[Dmat+1], nD, nD)
  CVmat12 <- matrix(c(Sig[1,2],gaH12)[Dmat+1], nD, nD)
  
  theDS <- c()
  ##  Calculos separados para cada distrito
  
  ##  Calculos separados para cada sujeto
  
  for (i in seq(nD))
  {
    ##  Los tamanos poblacionales de los h-anillos (denominadores)
    PszH <- tapply(Psz, Dmat[i,], sum)
    
    ##  Las medias muestrales de los h-anillos
    theH <- tapply(Psz*drE[1,], Dmat[i,], sum) / PszH
    auxH <- tapply(Psz*Aux, Dmat[i,], sum) / PszH
    
    ##  Las varianzas muestrales de los h-anillos
    vdH <- sWp * tapply(Psz^2/(drE[4,]+0.1*(drE[4,]==0)), Dmat[i,], sum) / PszH^2
    
    ##  Estimador de la desviacion al cuadrado de los h-anillos del objetivo
    EvdH1 <- EvdH2 <- 0
    
    ##  Para los coeficientes truncados
    udh <- sWp/(drE[4,i]+0.1*(drE[4,i]==0))
    
    EvdH <- list()
    EvdH[[1]] <- matrix(c(udh,0,0,tkn),2,2) 
    EvdI <- 0
    
    
    for (di in sort(unique(Dmat[i,]))[-1])
    {
      ##  Los distritos en los h-anillos
      whi <- Dmat[i,]==di
      
      ##  Las fracciones de los tamanos poblacionales
      rdh <- matrix(Psz[whi]/sum(Psz[whi]))
      
      ##  El estimador de la distancia al cuadrado
      EvdH[[di+1]] <- 
        matrix( c(t(rdh) %*% CVmat1[whi,whi] %*% rdh + si0 - 2*gaH1[di] + vdH[di],  
                  rep(t(rdh) %*% CVmat12[whi,whi] %*% rdh + Sig[1,2] - 2*gaH12[di], 2),
                  t(rdh) %*% CVmat2[whi,whi] %*% rdh + Sig[2,2]  - 2*gaH2[di]+tkn),2,2)
      
      EvdH[[di+1]][1,2] <- EvdH[[di+1]][2,1] <- 
        min(EvdH[[di+1]][1,2], 0.9*sqrt(prod(diag(EvdH[[di+1]]))) )
      
      EvdH[[di+1]][1,2] <- EvdH[[di+1]][2,1] <- 
        max(EvdH[[di+1]][1,2], -0.9*sqrt(prod(diag(EvdH[[di+1]]))) )
      
      
      EvdH[[di+1]] <- solve(EvdH[[di+1]],  EvdH[[1]])
      
      EvdI <- EvdI + EvdH[[di+1]]
    }  ##  Fin del loop sobre las distancias (di)
    
    ##  Sumamos la matriz identidad
    EvdH[[1]] <- diag(rep(1,2))
    EvdI <- EvdI + EvdH[[1]]
    
    Esti <- 0
    for (di in 1+sort(unique(Dmat[i,])))
    {
      Esti <- Esti + (matrix(c(theH[di], auxH[di]),1) %*% solve(EvdI, EvdH[[di]]))[1]
      
    }
    
    ##  El estimador DS
    theDS <- c(theDS, Esti)
  }  ##  Fin del loop sobre los distritos
  
  ##  Los errore de estimaci?n
  theDS - trg
  
}  ##  Fin de la funcion  SAsc2Fb


############################################################################################
############################################################################################
############################################################################################


##  Aplicacion de los estimadores directos y compuestos
##  Objetivo:  La proporcion de indigenas

##  Simulaciones del estimador compuesto en el conjunto de muestras SSrwT


##  Las proporciones poblacionales 
SAproTrg <- as.vector(LlarH)

##  Estimaci?n directa de las proporciones poblacionales
SAcsDEp <- apply(abs(sapply(lapply(SSrwV, ExtrE, rw=1, cl=seq(nProv)), 
                            Subt, SAproTrg)), 1, mean)

##  Estimacion univariada (sin distancia)
SAcsAEp <- apply(abs(sapply(SSrwV, SAcsAb, trg=SAproTrg)),1,mean)

##  Estimacion univariada (distancia 2)
SAcsFEp2 <- apply(abs(sapply(SSrwV, SAcsFb, trg=SAproTrg, mxD=2)),1,mean)

##  Estimacion Univariada (distancia 3)
SAcsFEp3 <- apply(abs(sapply(SSrwV, SAcsFb, trg=SAproTrg, mxD=3)),1,mean)

##  Estimacion Bivariada (sin distancia, usando datos del 2001)  S# ESTA ES LA MEJOR
SAcs2AEp <- apply(abs(sapply(SSrwV, SAcs2Ab, Aux=Llar01[,2]/apply(Llar01,1,sum), 
                             trg=SAproTrg, tkn=0.00001)),1,mean)

##  Estimacion Bivariada (con distancia, usando datos del 2001) # Mejor para enemdu
SAcs2FEp2 <- apply(abs(sapply(SSrwV, SAcs2Fb,Aux=Llar01[,2]/apply(Llar01,1,sum), 
                              trg=SAproTrg, mxD=2, tkn=0.00001)),1,mean)
# Minimo error
which.min(apply(cbind(SAcsDEp, SAcsAEp, SAcsFEp2, SAcsFEp3, SAcs2AEp, SAcs2FEp2),2,sum))

############################################################################################
############################################################################################
############################################################################################

# TABLA 5 Pag 35

##  Resumen de las comparaciones para la tabla 5
##  Eficiencia del conjunto de estimadores para las proporciones de area pequena

SAcsAllp <- rbind(SAcsDEp, SAcsAEp, SAcsFEp2, SAcsFEp3, SAcs2AEp, SAcs2FEp2)  
nEst <- dim(SAcsAllp)[1]

SAcsResP <- list()
SAcsResP <- matrix(0, nEst, nEst)

for (i1 in seq(nEst-1))
{
  for (i2 in seq(i1, nEst)) 
  {
    res <- CompV(SAcsAllp[i2,], SAcsAllp[i1,], plt=F)
    SAcsResP[i2,i1] <- paste(round(res[[3]],3), "  [", sum(res[[2]]<1), "]",sep="") # sum(res... no esta dando bien)
    SAcsResP[i1,i2] <- paste(round(res[[4]][1],3), "  (", round(res[[4]][2],3), ")", sep="")
  }
}  ##  Fin del doble loop

(SAcsResP)
rownames(SAcsResP) <- c("Direct", "1-Comp-1", "1-Comp-2", "1-Comp-3", "2-Comp-1", "2-Comp-2")
colnames(SAcsResP) <- c("Direct", "1-Comp-1", "1-Comp-2", "1-Comp-3", "2-Comp-1", "2-Comp-2")
print(xtable(SAcsResP),floating=FALSE)

##  "2-Comp-1" ESTA ES LA MEJOR (SAcs2AEp)

############################################################################################
############################################################################################
############################################################################################

# FIGURA 6 p?g. 36

##  Gr?fico de comparaci?n directa de los EMCs de los 6 estimadores
##  Estimaci?n de la proporci?n de ind?genas  

SAcs <- sqrt(cbind(SAcsDEp, SAcsAEp, SAcsFEp2, SAcsFEp3, SAcs2AEp, SAcs2FEp2))

rSAcs <-range(SAcs)

##  Las provincias ordenadas de acuerdo al tama?o
SAcs <- SAcs[sort.list(apply(LlarC,1,sum)),]

##  Las medias y las desviaciones est?ndar
mSAcs <- apply(SAcs,2,MeVa)

pts <- seq(rSAcs[1], rSAcs[2], length=nProv)

Mtext <- function(x,y,lbl,vls,cex=2,font=1)
{
  text(0.525,0.725,lbl,cex=1.25,font, adj=0.5)
  
}

##  Los puntos de corte para los grupos de provincias
# > summary(apply(LlarC,1,sum))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 83930  170500  398200  627200  567400 3645000

clin <- c(10,100,800,1000)*1000
vlin <- 1
for (cl in clin)
  vlin <- vlin + (apply(LlarC,1,sum) > cl)

vlin <- cumsum(table(vlin))
vl <- length(vlin)

##  Los espacios a ser ubicados en estos valores
vlin <- vlin[-vl] + 2*seq(vl-1)

nds <- seq(nProv)
for (vli in vlin)
  nds <- nds + (nds>vli)*2

MySeg <- function(x,y, nDs=nds, xlim=xlim, ylim=ylim, Vlin=vlin+1)
{
  
  points(nDs, y, pch=16,  cex=abs(x-y)*200)
  
  if (length(vlin)>0) 
  {
    
    segments(Vlin, -rSAcs[1], Vlin, rSAcs[1], lwd=0.4)
    segments(Vlin, rSAcs[2]-0.0012, Vlin, rSAcs[2]+0.002, lwd=0.4)
  }
}

# Prueba MySeg
 plot(seq(23),y,t="n")
 x=SAcs[,1];y=SAcs[,2];xlim=c(1,nProv+2*vl); ylim=rSAcs
 MySeg(x,y)

MyDiag <- function(x,y, xlim=xlim, ylim=ylim)
{
  points(0.5+5*runif(length(x)), x, pch=16, cex=0.3)
}

graphics.off()
setwd(dir.resul)
postscript("Fig6Gye.ps", width=5.5, height=5.8, pointsize=8, 
           horizontal=F) 

par(mfrow=c(1,1), mar=c(3,2,2,2), mgp=c(3,1,0)*0.6, lab=c(3,3,1))

pairs(x=SAcs, xlim=c(1,nProv+2*vl), ylim=rSAcs, 
      lower.panel=MySeg, upper.panel=MySeg, 
      diag.panel=MyDiag, 
      labels=paste(
        c("Direct", "1-Comp-1", "1-Comp-2", "1-Comp-3", "2-Comp-1", "2-Comp-2"),
        "\n\n", round(mSAcs[1,],3), "\n (",round(mSAcs[2,],3), ") ", sep=""), 
      text.panel=Mtext, label.pos=0.105)

dev.off()

tiff("Fig6Gye.tif", width = 800, height = 800, compression="none")
par(mfrow=c(1,1), mar=c(3,2,2,2), mgp=c(3,1,0)*0.6, lab=c(3,3,1))

pairs(x=SAcs, xlim=c(1,nProv+2*vl), ylim=rSAcs, 
      lower.panel=MySeg, upper.panel=MySeg, 
      diag.panel=MyDiag, 
      labels=paste(
        c("Direct", "1-Comp-1", "1-Comp-2", "1-Comp-3", "2-Comp-1", "2-Comp-2"),
        "\n\n", round(mSAcs[1,],3), "\n (",round(mSAcs[2,],3), ") ", sep=""), 
      text.panel=Mtext, label.pos=0.105)
dev.off()

Time <- proc.time() - Time



############################################################################################
############################################################################################
############################################################################################

### Aplicacion con los estimadores directos tomados de la ENEMDU
setwd(dir.dat)
datosE10 <- read.csv("datosENEMDU10Gye.csv",sep=";",header=TRUE,dec=".") 
datosE10$Cod <- poligonos@data$DPA_PROVIN

# Solo datos:
LlarE10 <- datosE10[,3:4]
rownames(LlarE10) <- AcrProv

# Matriz de entrada para estimaci?n:
Sprop <- LlarE10[,2]/apply(LlarE10,1,sum)
Smean <- rep(1,nProv)
Svar <- rep(0,nProv)
Ssize <- apply(LlarE10,1,sum)  # Chequear si es mejor poner el numero de observaciones de la muestra
LlarH10 <- rbind(Sprop,Smean,Svar,Ssize)

############################################################################################
############################################################################################
############################################################################################

##  Estimacion compuesta bivariada (similaridad de distancia)
##  Resultados binarios (si es indigena)


SAcs2Fb <- function(drE, Aux=Llar01[,2]/apply(Llar01,1,sum), trg=SAproTrg, 
                    Psz=apply(LlarC,1,sum), Dmat=ProSim, tkn=0.00001, mxD=3)
{
  ##  Estimaci?n compuesta  (simetr?a compuesta)
  ##  Estimaci?n de la media nacional
  ##    drE     Estimadores directos
  ##    Aux    Variable auxiliar
  ##    trg      Los objetivos
  ##    Psz     Tama?os de la poblaci?n por distrito
  ##   Dmat   La matriz de distancia
  ##    tkn      La varianza de la muestra (a ser a?adida a la var. de aux)
  ##   mxD    La m?xima distancia (trunProvion)
  
  ##  Los tama?os muestrales
  nD <- dim(drE)[2]
  nObs <- sum(drE[4,])
  
  ##  La proporci?n nacional, componentes del estimador
  theA <- mean(drE[1,])
  theB <- sum(drE[1,]*drE[4,])/nObs
  
  ##  La varianza agrupada dentro de los distritos
  whi <- drE[4,] > 1
  sWp <- theB*(1-theB)
  
  ##  El estimador de varianza agrupada de los estimadores directos
  vd <- sWp/(drE[4,]+0.1*(drE[4,]==0))
  
  ##  Las varianzas de los estimadores nacionales
  vA <- mean(vd)/length(vd)
  vB <- sWp/nObs
  
  ##  El coeficiente de la composici?n
  bthe <- (vA - vB) / (vA - vB + (theA-theB)^2)
  
  ##  El estimador compuesto de la media nacional
  theC <- (1-bthe)*theA + bthe*theB
  
  ##  Los pesos de cada estimador directo en $\tilde{\theta}$
  qd <- (1-bthe)/nD + bthe*drE[4,]/nObs
  
  ##  La covarianza en los coeficientes truncados
  Cd <- vd * qd
  
  ##  Estimadores de la varianza a nivel de distrito
  dvs <- matrix(drE[1,]-theC)
  siA <- mean(dvs^2) - (theA-theB)^2 - mean(vd) + 2 * mean(Cd)
  siA <- max(siA,0)
  
  siB <- sum(drE[4,]*dvs^2)/nObs - (theA-theB)^2 - (nD-2)/nObs*sWp
  siB <- max(siB,0)
  
  ##  Las varianzas muestrales de los estimadores de varianza
  Imat <- diag(rep(1, nD)) - matrix(1, nD) %*% matrix(qd,1)
  Amat <- t(Imat) %*% Imat / nD
  
  Bmat <- t(Imat) %*% diag(drE[4,]) %*% Imat / nObs
  
  ##  La varianza de siA
  VA <- 2 * matrix(vd,1) %*% Amat %*% Amat %*% matrix(vd) + 
    4 * t(dvs) %*% Amat %*% diag(vd) %*% Amat %*% dvs
  
  ##  La varianza de siB
  VB <- 2 * matrix(vd,1) %*% Bmat  %*% Bmat %*% matrix(vd) + 
    4 * t(dvs) %*% Bmat %*% diag(vd) %*% Bmat  %*% dvs
  
  ##  La covarianza de siA y siB
  CAB <- 2 * matrix(vd,1) %*% Amat  %*% Bmat %*% matrix(vd) + 
    4 * t(dvs) %*% Amat %*% diag(vd) %*% Bmat %*% dvs
  
  ##  El coeficiente compuesto para  $\sigma^2_0$
  bsi <- (VA-CAB) / (VA - 2*CAB + VB + (siA-siB)^2)
  bsi <- min(bsi,1)
  
  ##  El estimador compuesto de $\sigma^2_0$
  si0 <- (1-bsi)*siA + bsi*siB
  
  ##  La varianza de theC
  vv <- mean(vd)/nD 
  v <- vv - (vv - sWp/nObs)^2/(vv - sWp/nObs +(theA-theB)^2)
  
  ##  Los coeficientes truncados
  bds <- (vd-Cd) / (vd-2*Cd+v + bthe^2*(theA-theB)^2+ si0)
  bds[bds<0] <- 0
  
  ##  Entradas de la matriz de distancia truncadas
  Dmat[Dmat>mxD] <- mxD
  
  ##  Excluimos vac?os
  whi <- drE[4,]>0 
  nE <- sum(whi)
  
  ##  Estimaci?n de las corarianzas relacionadas con la distancia
  DS1 <- 
    tapply((rep(drE[1,whi], nE) - rep(drE[1,whi], rep(nE,nE)))^2, Dmat[whi,whi], sum)[-1]
  
  DS2 <- 2 * sWp * 
    tapply(rep(1/drE[4,whi], nE) + rep(1/drE[4,whi],rep(nE,nE)), Dmat[whi,whi], sum)[-1]
  
  DS3 <- table(Dmat[whi,whi])[-1]
  
  
  ##  La media y varianza a nivel de distrito para la variable auxiliar
  mAux <- mean(Aux)
  vAux <- var(Aux)
  
  ##  La covarianza a nivel de distrito
  cAux <- mean((drE[1,]-theC)*(Aux-mAux))
  
  ##  La matriz de varianza a nivel de distrito
  Sig <- matrix(c(max(si0, 1/0.95^2 * cAux^2 / vAux), rep(cAux,2), vAux),2,2)
  
  ##  Las covarianzas
  gaH1 <- si0 - (DS1 - DS2)/DS3/2
  gaH1[gaH1<0] <- 0
  gaH1[gaH1>0.5*si0] <- 0.5*si0
  
  gaH2 <- Sig[2,2] - tapply((rep(Aux[whi], nE) - rep(Aux[whi], rep(nE, nE)) )^2, 
                            Dmat[whi,whi], sum)[-1] / table(Dmat[whi,whi])[-1]/2
  
  gaH2[gaH2 < 0] <- 0
  gaH2[gaH2>0.5*vAux] <- 0.5*vAux
  
  ##  Para las covarianzas
  gaH12 <- Sig[1,2] - tapply((rep(drE[1,whi], nE) - rep(drE[1,whi], rep(nE,nE))) * 
                               (rep(Aux[whi], nE) - rep(Aux[whi], rep(nE, nE))), Dmat[whi,whi], sum)[-1] /
    table(Dmat)[-1]/2
  
  
  whi <- gaH12 > 0.95*sqrt(gaH1 * gaH2) 
  gaH12[whi] <- 0.95*sqrt(gaH1 * gaH2)[whi]
  
  whi <- gaH12 < -0.95*sqrt(gaH1 * gaH2) 
  gaH12[whi] <- -0.95*sqrt(gaH1 * gaH2)[whi]
  
  ##  La matriz de covarianza ajustada para las dos variables
  CVmat1 <- matrix(c(si0,gaH1)[Dmat+1], nD, nD)
  CVmat2 <- matrix(c(Sig[2,2],gaH2)[Dmat+1], nD, nD)
  CVmat12 <- matrix(c(Sig[1,2],gaH12)[Dmat+1], nD, nD)
  
  theDS <- c()
  ##  C?lculos separados para cada distrito
  
  ##  C?lculos separados para cada sujeto
  
  for (i in seq(nD))
  {
    ##  Los tama?os poblacionales de los h-anillos (denominadores)
    PszH <- tapply(Psz, Dmat[i,], sum)
    
    ##  Las medias muestrales de los h-anillos
    theH <- tapply(Psz*drE[1,], Dmat[i,], sum) / PszH
    auxH <- tapply(Psz*Aux, Dmat[i,], sum) / PszH
    
    ##  Las varianzas muestrales de los h-anillos
    vdH <- sWp * tapply(Psz^2/(drE[4,]+0.1*(drE[4,]==0)), Dmat[i,], sum) / PszH^2
    
    ##  Estimador de la desviaci?n al cuadrado de los h-anillos del objetivo
    EvdH1 <- EvdH2 <- 0
    
    ##  Para los coeficientes truncados
    udh <- sWp/(drE[4,i]+0.1*(drE[4,i]==0))
    
    EvdH <- list()
    EvdH[[1]] <- matrix(c(udh,0,0,tkn),2,2) 
    EvdI <- 0
    
    
    for (di in sort(unique(Dmat[i,]))[-1])
    {
      ##  Los distritos en los h-anillos
      whi <- Dmat[i,]==di
      
      ##  Las fracciones de los tama?os poblacionales
      rdh <- matrix(Psz[whi]/sum(Psz[whi]))
      
      ##  El estimador de la distancia al cuadrado
      EvdH[[di+1]] <- 
        matrix( c(t(rdh) %*% CVmat1[whi,whi] %*% rdh + si0 - 2*gaH1[di] + vdH[di],  
                  rep(t(rdh) %*% CVmat12[whi,whi] %*% rdh + Sig[1,2] - 2*gaH12[di], 2),
                  t(rdh) %*% CVmat2[whi,whi] %*% rdh + Sig[2,2]  - 2*gaH2[di]+tkn),2,2)
      
      EvdH[[di+1]][1,2] <- EvdH[[di+1]][2,1] <- 
        min(EvdH[[di+1]][1,2], 0.9*sqrt(prod(diag(EvdH[[di+1]]))) )
      
      EvdH[[di+1]][1,2] <- EvdH[[di+1]][2,1] <- 
        max(EvdH[[di+1]][1,2], -0.9*sqrt(prod(diag(EvdH[[di+1]]))) )
      
      
      EvdH[[di+1]] <- solve(EvdH[[di+1]],  EvdH[[1]])
      
      EvdI <- EvdI + EvdH[[di+1]]
    }  ##  Fin del loop sobre las distancias (di)
    
    ##  Sumamos la matriz identidad
    EvdH[[1]] <- diag(rep(1,2))
    EvdI <- EvdI + EvdH[[1]]
    
    Esti <- 0
    for (di in 1+sort(unique(Dmat[i,])))
    {
      Esti <- Esti + (matrix(c(theH[di], auxH[di]),1) %*% solve(EvdI, EvdH[[di]]))[1]
      
    }
    
    ##  El estimador DS
    theDS <- c(theDS, Esti)
  }  ##  Fin del loop sobre los distritos
  
  ##  Los errore de estimaci?n
  theDS - trg
  list(MSE = c(theDS - trg), Esti = theDS, est.dir = drE[1,], target = trg)
  
}  ##  Fin de la funcion  SAsc2Fb


##  Estimacion compuesta bivariada (similaridad de distancia) 


ResEsti <- SAcs2Fb(LlarH10, Aux=Llar01[,1]/apply(Llar01,1,sum), trg=SAproTrg, tkn=0.00001,mxD=2)

Compara <- cbind(ResEsti$Esti,ResEsti$est.dir,ResEsti$target)
colnames(Compara) <- c("2-Comp-2","Directo","Objetivo")


dist(t(Compara),diag=TRUE,upper=TRUE)*100

