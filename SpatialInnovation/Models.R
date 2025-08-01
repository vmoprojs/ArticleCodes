#-----------------------------------------------------------------
# Institution: FLACSO (https://www.flacso.edu.ec/portal)
# Proyect: CLARABD. 
# Author: Víctor Morales-Oñate (https://sites.google.com/site/moralesonatevictor/)
# Date: 06-03-2018
# Article: Innovación en Ecuador: un enfoque espacial
#-----------------------------------------------------------------

rm(list = ls())
gc()
graphics.off()
cat("\014")


dat.f <- "~/Documents/DataBase/ACTI/Datos"
inn.f <- "/BASE DE DATOS SPSS INN"
acti.f <- "/BASE DE DATOS SPSS CT"

setwd(paste(dat.f,inn.f,sep = ""))
library(rio)
pp = 0
#inn years: 2012,2013,2014
inn <- import("bdd_INN_2015.sav")

# **********VARIABLES REGIONALES
cbind(table(inn$cod_provincia))
# creo dummies:
prov.aux <- factor(inn$cod_provincia)
inn$provdumm <- model.matrix(~prov.aux)
rm(prov.aux)

# **********VARIABLES SECTORIALES

inn$sectoremp <- NA
f1 <- with(inn,(ciiu2=="C10" | ciiu2=="C11" | ciiu2=="C12" | ciiu2=="C13" | ciiu2=="C14" | ciiu2=="C15" | ciiu2=="C16" | ciiu2=="C17" | ciiu2=="C18" | ciiu2=="C31" | ciiu2=="C32"))
f2 <- with(inn,(ciiu2=="C19" | ciiu2=="C23" | ciiu2=="C24" | ciiu2=="C25" | ciiu2=="C33" | ciiu2=="S95"))
f3 <- with(inn,(ciiu2=="C20" | ciiu2=="C22" | ciiu2=="C27" | ciiu2=="C28" | ciiu2=="C29" | ciiu2=="C30" ))
f4 <- with(inn,(ciiu2=="C21" | ciiu2=="C26"))
f5 <- with(inn,(ciiu2=="B09" | ciiu2=="G45" | ciiu2=="G46" | ciiu2=="G47" | ciiu2=="H49" | ciiu2=="H50" | ciiu2=="H51" | ciiu2=="H52" | ciiu2=="H53" | ciiu2=="I55" | ciiu2=="I56" | ciiu2=="L68" | ciiu2=="M73" | ciiu2=="N77" | ciiu2=="N78" | ciiu2=="N79" | ciiu2=="N81" | ciiu2=="N82" | ciiu2=="O84" | ciiu2=="S94" | ciiu2=="S96" | ciiu2=="T97" | ciiu2=="T98" | ciiu2=="U99"))
f6 <- with(inn,(ciiu2=="J58" | ciiu2=="J59" | ciiu2=="J60" | ciiu2=="J61" | ciiu2=="J62" | ciiu2=="J63" | ciiu2=="K64" | ciiu2=="K65" | ciiu2=="K66" | ciiu2=="M69" | ciiu2=="M70" | ciiu2=="M71" | ciiu2=="M72" | ciiu2=="M74" | ciiu2=="M75" | ciiu2=="N80" | ciiu2=="P85" | ciiu2=="Q86" | ciiu2=="Q87" | ciiu2=="Q88" | ciiu2=="R90" | ciiu2=="R91" | ciiu2=="R92" | ciiu2=="R93"))
f7 <- with(inn,(ciiu2=="D35" | ciiu2=="E35" | ciiu2=="E36" | ciiu2=="E37" | ciiu2=="E38" | ciiu2=="E39"))
f8 <- with(inn,(ciiu2=="B05" | ciiu2=="B06" | ciiu2=="B07" | ciiu2=="B08"))
f9 <- with(inn,(ciiu2=="F41" | ciiu2=="F42" | ciiu2=="F43"))
f10 <- with(inn,(ciiu2=="A01" | ciiu2=="A02" | ciiu2=="A03"))

inn$sectoremp <- with(inn,ifelse (f1,1,sectoremp))
inn$sectoremp <- with(inn,ifelse (f2,2,sectoremp))
inn$sectoremp <- with(inn,ifelse (f3,3,sectoremp))
inn$sectoremp <- with(inn,ifelse (f4,4,sectoremp))
inn$sectoremp <- with(inn,ifelse (f5,5,sectoremp))
inn$sectoremp <- with(inn,ifelse (f6,6,sectoremp))
inn$sectoremp <- with(inn,ifelse (f7,7,sectoremp))
inn$sectoremp <- with(inn,ifelse (f8,8,sectoremp))
inn$sectoremp <- with(inn,ifelse (f9,9,sectoremp))
inn$sectoremp <- with(inn,ifelse (f10,10,sectoremp))


cbind(table(inn$sectoremp))


inn$sector_1 <- 0
inn$sector_1 <- with(inn,ifelse (sectoremp == 1,1,sector_1))
inn$sector_2 <- 0
inn$sector_2 <- with(inn,ifelse (sectoremp == 2,1,sector_2))
inn$sector_3 <- 0
inn$sector_3 <- with(inn,ifelse (sectoremp == 3,1,sector_3))
inn$sector_4 <- 0
inn$sector_4 <- with(inn,ifelse (sectoremp == 4,1,sector_4))
inn$sector_5 <- 0
inn$sector_5 <- with(inn,ifelse (sectoremp == 5,1,sector_5))
inn$sector_6 <- 0
inn$sector_6 <- with(inn,ifelse (sectoremp == 6,1,sector_6))
inn$sector_7 <- 0
inn$sector_7 <- with(inn,ifelse (sectoremp == 7,1,sector_7))
inn$sector_8 <- 0
inn$sector_8 <- with(inn,ifelse (sectoremp == 8,1,sector_8))
inn$sector_9 <- 0
inn$sector_9 <- with(inn,ifelse (sectoremp == 9,1,sector_9))


table(inn$sector_9)

# *******************CARACTERISTICAS DE LAS EMPRESAS

inn$publica <- inn$ii_3   #/*si la empresa es publica*/
inn$grupo <- inn$ii_1  #/*si pertenece a un grupo*/

table(inn$ii_2)
inn$mne <- 0
f <- (inn$ii_2!=1 & !is.na(inn$ii_2))
inn$mne <- with(inn,ifelse (f,1,mne))  #/*si es una multinacional extranjera*/


inn$edad <- 2014-inn$ii_6    #/*edad de la empresa*/
inn$exportadora <- 0
f <- with(inn, ii_9_b_2012>0 | ii_9_b_2013>0 | ii_9_b_2014>0)
inn$exportadora <- with(inn,ifelse (f,1,exportadora)) #/*si la empresa es exportadora*/

inn$ltamano <- with(inn,log(ii_9_d_2014))      #/*logaritmo tamano de la empresa*/
inn$lprod_trabajo <- with(inn,log(ii_9_a_2014/ii_9_d_2014))     #/*logaritmo productividad del trabajo*/

inn$ltamano <- with(inn,log(ii_9_d_2014))      #/*logaritmo tamano de la empresa*/
inn$lprod_trabajo <- with(inn,log(ii_9_a_2014/ ii_9_d_2014))     #/*logaritmo productividad del trabajo*/


# ********************OUTPUTS INNOVADORES

# /*por tipo y grado de novedad*/
inn$bien_rad <- inn$iii_1_a#Innovación de un Bien nuevo
inn$bien_inc <- inn$iii_1_c#Innovación de un Bien significativamente mejorado
inn$serv_rad <- inn$iii_1_b#Innovación de un Servicio nuevo
inn$serv_inc <- inn$iii_1_d#Innovación de un Servicio significativamente mejorado
inn$proc_rad <- inn$iv_1_a #Innovación lograda de Proceso Nuevo
inn$proc_inc <- inn$iv_1_b#Innovación lograda de Proceso significativamente mejorado

# /*por tipo y grado de novedad*/*
inn$producto <- 0
f <- with(inn, bien_rad==1 | bien_inc==1 | serv_rad==1 | serv_inc==1)
inn$producto <- with(inn,ifelse (f,1,producto))
table(inn$producto)

inn$proceso <- 0
f <- with(inn, proc_rad==1 | proc_inc==1)
inn$proceso <- with(inn,ifelse (f,1,proceso))
table(inn$proceso)

# /*innovador exitoso*/

inn$exitoso <- 0
f <- with(inn, producto==1 | proceso==1)
inn$exitoso <- with(inn,ifelse (f,1,exitoso))
table(inn$exitoso)

# /*organizacional y marketing*/
inn$org <- inn$xi_1
inn$mkt <- inn$xii_1

# /*variables continuas sobre la innovacion de productos*/
inn$vtas_rad <- inn$iii_3_a_ventas #Porcentaje de ventas de productos Nuevos o significativamente mejorados para la empresa y para el mercado (nacional y/o internacional)
inn$vtas_inc <- inn$iii_3_b_ventas#Porcentaje de ventas de productos Nuevos o significativamente mejorados para la empresa, pero ya existentes en el mercado
inn$vtas_inn <- inn$vtas_rad+inn$vtas_inc
summary(inn$vtas_inn)

# ***********************INPUTS INNOVADORES
divi <- function(x,y)
{
  if(x ==0)
  {
    sol <- 0
  }
  
  if(y ==0)
  {
    sol <- NA
  }else
  {
    sol <- x/y
  }
  
}
divi <- Vectorize(divi)

inn$id_int <- inn$v_1_a
inn$int_int=with(inn,v_1_a_2014/ ii_9_a_2014) #Gastos en Investigación y Desarrollo (I+D) interna para el año 2014/Ventas anuales 2014
inn$int_int=with(inn,divi(v_1_a_2014,ii_9_a_2014))
summary(inn$v_1_a_2014)#Gastos en Investigación y Desarrollo (I+D) interna para el año 2014
summary(inn$ii_9_a_2014)#Ventas anuales 2014
summary(inn$int_int)

# aux <- (divi(inn$v_1_a_2014,inn$ii_9_a_2014))
# inn[is.infinite(aux),c("v_1_a_2014","ii_9_a_2014")]


inn$id_ext <- inn$v_1_b#Su empresa desarrolló "Investigación y Desarrollo (I+D) externa"
inn$int_ext <- with(inn,divi(v_1_b_2014,ii_9_a_2014))


inn$id <- 0# v_1_a:Su empresa desarrolló "Investigación y Desarrollo (I+D) interna"
f <- with(inn, v_1_a==1 | v_1_b==1)
inn$id <- with(inn,ifelse (f,1,id))

inn$int_id <- NA
inn$int_id <- with(inn,ifelse(ii_9_a_2014!=0,(v_1_a_2014+v_1_b_2014)/ii_9_a_2014,int_id))

inn$otros <- 0
f <- with(inn, v_4_a==1 | v_4_b==1 | v_4_c==1 | v_4_d==1 | v_4_e==1 | v_4_f==1 | v_4_g==1 | v_4_h==1 )
inn$otros <- with(inn,ifelse (f,1,otros))

inn$int_otros <- NA
inn$int_otros <- with(inn,ifelse(ii_9_a_2014!=0,(v_4_a_2014+v_4_b_2014+v_4_c_2014+v_4_d_2014+v_4_e_2014+v_4_f_2014+v_4_g_2014+v_4_h_2014)/ii_9_a_2014,int_id))

summary(inn$int_otros)
cbind(tapply(inn$int_otros,inn$cod_provincia,mean,na.rm =TRUE))


inputs <- c("id_int","id_ext","int_int","int_ext")
outputs <- c("producto","proceso","exitoso")

### VaR OBJETIVO (ventas sobre empleo: productividad):

summary(inn$ii_9_a_2014/inn$ii_9_d_2014)
inn$prod <- with(inn,divi(ii_9_a_2014,ii_9_d_2014))
summary(inn$prod)

# interna <- tapply(inn$id_int*inn$fexp,inn$cod_provincia,mean,na.rm = TRUE)
# externa <- tapply(inn$id_ext*inn$fexp,inn$cod_provincia,mean,na.rm = TRUE)
# 
# i.interna <- tapply(inn$int_int*inn$fexp,inn$cod_provincia,mean,na.rm = TRUE)
# i.externa <- tapply(inn$int_ext*inn$fexp,inn$cod_provincia,mean,na.rm = TRUE)
# 
# exitoso <- tapply(inn$exitoso*inn$fexp,inn$cod_provincia,mean,na.rm = TRUE)
# otros <- tapply(inn$otros*inn$fexp,inn$cod_provincia,mean,na.rm = TRUE)


interna <- tapply(inn$id_int,inn$cod_provincia,mean,na.rm = TRUE)
externa <- tapply(inn$id_ext,inn$cod_provincia,mean,na.rm = TRUE)

i.interna <- tapply(inn$int_int,inn$cod_provincia,mean,na.rm = TRUE)
i.externa <- tapply(inn$int_ext,inn$cod_provincia,mean,na.rm = TRUE)

exitoso <- tapply(inn$exitoso,inn$cod_provincia,mean,na.rm = TRUE)
otros <- tapply(inn$otros,inn$cod_provincia,mean,na.rm = TRUE)
i.otros <- tapply(inn$int_otros,inn$cod_provincia,mean,na.rm = TRUE)

###3 Suma
interna <- tapply(inn$id_int,inn$cod_provincia,mean,na.rm = TRUE)
externa <- tapply(inn$id_ext,inn$cod_provincia,mean,na.rm = TRUE)

proceso <- tapply(inn$proceso,inn$cod_provincia,mean,na.rm = TRUE)
producto <- tapply(inn$producto,inn$cod_provincia,mean,na.rm = TRUE)

i.interna <- tapply(inn$int_int,inn$cod_provincia,sum,na.rm = TRUE)
i.externa <- tapply(inn$int_ext,inn$cod_provincia,sum,na.rm = TRUE)

exitoso <- tapply(inn$exitoso,inn$cod_provincia,mean,na.rm = TRUE)
otros <- tapply(inn$otros,inn$cod_provincia,mean,na.rm = TRUE)
i.otros <- tapply(inn$int_otros,inn$cod_provincia,sum,na.rm = TRUE)
prod <- tapply(log(inn$prod+1),inn$cod_provincia,mean,na.rm = TRUE)



# economic development level (GDPP) (GDP por provincia)
{
  setwd("~/Documents/DataBase/BCE/Provinciales")
  library(readxl)
  yrs <- c(2012,2013,2014)
  vab.tot <- NULL
  
  #2012
  ann <- 1
  nom <- paste("Cab",yrs[ann],".xlsx",sep = "")
  print(nom)
  vab.aux <- read_excel(nom,range = "VAB TOTAL!A6:G227")
  vab.aux <- as.data.frame(vab.aux)
  vab <- aggregate(vab.aux$`VALOR AGREGADO BRUTO`,list(vab.aux$`CÓDIGO PROVINCIA`),sum)
  colnames(vab) <- c("cod_provincia","vab2012")
  vab12 <- vab
  #2013
  ann <- 2
  nom <- paste("Cab",yrs[ann],".xlsx",sep = "")
  print(nom)
  vab.aux <- read_excel(nom,range = "VAB TOTAL!A6:G227")
  vab.aux <- as.data.frame(vab.aux)
  vab <- aggregate(vab.aux$`VALOR AGREGADO BRUTO`,list(vab.aux$`CÓDIGO PROVINCIA`),sum)
  colnames(vab) <- c("cod_provincia","vab2013")
  vab13 <- vab
  #2014
  ann <- 3
  nom <- paste("Cab",yrs[ann],".xlsx",sep = "")
  print(nom)
  vab.aux <- read_excel(nom,range = "VAB TOTAL!A6:G227")
  vab.aux <- as.data.frame(vab.aux)
  vab <- aggregate(vab.aux$`VALOR AGREGADO BRUTO`,list(vab.aux$`CÓDIGO PROVINCIA`),sum)
  colnames(vab) <- c("cod_provincia","vab2014")
  vab14 <- vab
  vab <- cbind(vab12,vab13,vab14)
  provcod <- vab[,1]
  vab <- vab[,c(2,4,6)]
  GDPP <- apply(vab,1,mean)
}



# Cooperacion

inn$ImasD <- NA #I+D
f <- with(inn, viii_2_a_1==1 | viii_2_b_1==1 | viii_2_c_1==1 | viii_2_d_1==1 | viii_2_e_1==1 | viii_2_f_1==1 | viii_2_g_1==1 | viii_2_h_1==1 | viii_2_i_1==1 | viii_2_j_1==1 )
inn$ImasD <- with(inn,ifelse (f,1,ImasD))
inn$ImasD[is.na(inn$ImasD)] = 0
table(inn$ImasD)

ImasD <- tapply(inn$ImasD,inn$cod_provincia,mean,na.rm = TRUE)


inn$IngDis <- NA #Ingernieria y Diseno
f <- with(inn, viii_2_a_2==2 | viii_2_b_2==2 | viii_2_c_2==2 | viii_2_d_2==2 | viii_2_e_2==2 | viii_2_f_2==2 | viii_2_g_2==2 | viii_2_h_2==2 | viii_2_i_2==2 | viii_2_j_2==2 )
inn$IngDis <- with(inn,ifelse (f,1,IngDis))
inn$IngDis[is.na(inn$IngDis)] = 0
table(inn$IngDis)

IngDis <- tapply(inn$IngDis,inn$cod_provincia,mean,na.rm = TRUE)


inn$Capa <- NA #Capacitacion
f <- with(inn, viii_2_a_3==3 | viii_2_b_3==3 | viii_2_c_3==3 | viii_2_d_3==3 | viii_2_e_3==3 | viii_2_f_3==3 | viii_2_g_3==3 | viii_2_h_3==3 | viii_2_i_3==3 | viii_2_j_3==3 )
inn$Capa <- with(inn,ifelse (f,1,Capa))
inn$Capa[is.na(inn$Capa)] = 0
table(inn$Capa)

Capa <- tapply(inn$Capa,inn$cod_provincia,mean,na.rm = TRUE)


inn$AsisTec <- NA #Asistencia tecnica
f <- with(inn, viii_2_a_4==4 | viii_2_b_4==4 | viii_2_c_4==4 | viii_2_d_4==4 | viii_2_e_4==4 | viii_2_f_4==4 | viii_2_g_4==4 | viii_2_h_4==4 | viii_2_i_4==4 | viii_2_j_4==4 )
inn$AsisTec <- with(inn,ifelse (f,1,AsisTec))
inn$AsisTec[is.na(inn$AsisTec)] = 0
table(inn$AsisTec)

AsisTec <- tapply(inn$AsisTec,inn$cod_provincia,mean,na.rm = TRUE)

inn$Info <- NA #Informacion
f <- with(inn, viii_2_a_5==5 | viii_2_b_5==5 | viii_2_c_5==5 | viii_2_d_5==5 | viii_2_e_5==5 | viii_2_f_5==5 | viii_2_g_5==5 | viii_2_h_5==5 | viii_2_i_5==5 | viii_2_j_5==5 )
inn$Info <- with(inn,ifelse (f,1,Info))
inn$Info[is.na(inn$Info)] = 0
table(inn$Info)

Info <- tapply(inn$Info,inn$cod_provincia,mean,na.rm = TRUE)

inn$Pruebas <- NA #Pruebas de productos
f <- with(inn, viii_2_a_6==6 | viii_2_b_6==6 | viii_2_c_6==6 | viii_2_d_6==6 | viii_2_e_6==6 | viii_2_f_6==6 | viii_2_g_6==6 | viii_2_h_6==6 | viii_2_i_6==6 | viii_2_j_6==6 )
inn$Pruebas <- with(inn,ifelse (f,1,Pruebas))
inn$Pruebas[is.na(inn$Pruebas)] = 0
table(inn$Pruebas)

Pruebas <- tapply(inn$Pruebas,inn$cod_provincia,mean,na.rm = TRUE)

inn$Finan <- NA #Financiamiento
f <- with(inn, viii_2_a_7==7 | viii_2_b_7==7 | viii_2_c_7==7 | viii_2_d_7==7 | viii_2_e_7==7 | viii_2_f_7==7 | viii_2_g_7==7 | viii_2_h_7==7 | viii_2_i_7==7 | viii_2_j_7==7 )
inn$Finan <- with(inn,ifelse (f,1,Finan))
inn$Finan[is.na(inn$Finan)] = 0
table(inn$Finan)


#### Cap/Asis/Informacion VS Otro,

inn$CapAsisInf <- NA #Financiamiento
f <- with(inn, Capa==1 | AsisTec==1 | Info ==1)
inn$CapAsisInf <- with(inn,ifelse (f,1,CapAsisInf))
inn$CapAsisInf[is.na(inn$CapAsisInf)] = 0
table(inn$CapAsisInf)

CapAsisInf <- tapply(inn$CapAsisInf,inn$cod_provincia,mean,na.rm = TRUE)

#### Cap/Asis/Informacion VS Otro,

inn$Otro <- NA 
f <- with(inn, ImasD==1 | IngDis==1 | Pruebas ==1)
inn$Otro <- with(inn,ifelse (f,1,Otro))
inn$Otro[is.na(inn$Otro)] = 0
table(inn$Otro)

Otro <- tapply(inn$Otro,inn$cod_provincia,mean,na.rm = TRUE)
# Apoyo no reembolsable del gobierno:
Apoyo <- tapply(inn$vi_2_2,inn$cod_provincia,mean,na.rm = TRUE)
# Porcentaje sistema financiero:
Financiero <- tapply(inn$vi_1_b,inn$cod_provincia,mean,na.rm = TRUE)
Financiero[is.nan(Financiero)] <- 0
Financiero

#### ACTI


setwd(paste(dat.f,acti.f,sep = ""))

actipor <- import("Tabulados CT 2015.xlsx", sheet="ACT_16",range="A6:E30")
actitrab <- import("Tabulados CT 2015.xlsx", sheet="ACT_19",range="A6:E30")

actipor <- actipor[order(actipor$ID),]
actitrab <- actitrab[order(actitrab$ID),]


# DataBase
acti <- import("bdd_CT_2015.sav")
# actiImasD <- acti$IV.1.14
actiImasD <- tapply(acti$IV.1.14,acti$ui.2,sum,na.rm = TRUE)
actiCiencia <- tapply(acti$IV.2.14,acti$ui.2,sum,na.rm = TRUE)
actiPublico <- tapply(acti$IV.3.a.14,acti$ui.2,sum,na.rm = TRUE)

PorImasD <- actiImasD/(actiImasD+actiCiencia)
Publico <- actiPublico/actiImasD

PorImasD <- c(PorImasD[1:7],0,PorImasD[8:12],c(0,0),PorImasD[13:15],0,PorImasD[16:17],0,0,PorImasD[18])
Publico <- c(Publico[1:7],0,Publico[8:12],c(0,0),Publico[13:15],0,Publico[16:17],0,0,Publico[18])

sort(as.numeric(unique(inn$cod_provincia)))



dat <- data.frame(GDPP=as.numeric(GDPP),
                  ImasD = as.numeric(ImasD),
                  IngDis = as.numeric(IngDis),
                  Capa = as.numeric(Capa),
                  AsisTec = as.numeric(AsisTec),
                  Info = as.numeric(Info),
                  Pruebas = as.numeric(Pruebas),
                  prod = as.numeric(prod),
                  exitoso = as.numeric(exitoso),
                  actitrab = as.numeric(actitrab$`2014`),
                  actipor = as.numeric(actipor$`2014`),
                  CapAsisInf = as.numeric(CapAsisInf),
                  Otro = as.numeric(Otro),
                  PorImasD = as.numeric(PorImasD),
                  Publico = as.numeric(Publico),
                  Apoyo = as.numeric(Apoyo),
                  Financiero = as.numeric(Financiero))
# dat <- data.frame(exitoso,interna,externa,otros,i.interna)
str(dat)
dat$DPA_PROVIN <- rownames(dat)

### ST: DESCRIPTIVO

summary(dat$prod)
plot(density(((dat$prod))))
summary(dat$exitoso)
plot(density(((dat$exitoso))))
### END: DESCRIPTIVO


###### MAPA
library(spdep)
library(RColorBrewer)
library(classInt)
library(maptools)

setwd("~/Documents/Consultorias&Cursos/DataLectures/SpatialData")

read_git_shp <- function(uu)
{
  #creamos un par de archivos temporales
  temp <- tempfile()
  temp2 <- tempfile()
  #decargamos el zip folder y lo guardamos en 'temp' 
  
  download.file(uu,temp)
  #descomprimir en 'temp' y guardarlo en 'temp2'
  unzip(zipfile = temp, exdir = temp2)
  #encontramos los archivos SHP
  #el $ al final de ".shp$" asegura que no encontremos archivos del tipo .shp.xml 
  your_SHP_file <- list.files(temp2, pattern = ".shp$",full.names=TRUE)
  
  ff = strsplit(your_SHP_file,"/")
  ff = unlist(ff)
  ff = ff[length(ff)]
  ff = strsplit(ff,".shp")
  ff = unlist(ff)
  
  datos = rgdal::readOGR(your_SHP_file,layer = ff)
  unlink(temp)
  unlink(temp2)
  return(datos)
}

uu <- "https://github.com/vmoprojs/DataLectures/raw/master/SpatialData/ProvEcuador.zip"

pr <- read_git_shp(uu)
pr <- pr[pr$DPA_PROVIN!=90,]
pr <- pr[pr$DPA_PROVIN!=20,]
dat <- dat[-c(20),]


plot(density(dat$exitoso))


setwd("~/Dropbox/FLACSO/Innovacion/Trabajos/Writing")
# ************Mapping Farm Density in 2007
pal.red <- brewer.pal(5,"Reds")
q5.den <- classIntervals(dat$exitoso,5,style="quantile") 
cols.den <- findColours(q5.den, pal.red)
AcrProv <- substr(pr@data$DPA_DESPRO,1,4)
if(pp ==1)
{
  quartz()
  pdf("mapexitoso.pdf")
  plot(pr, col=cols.den)
  text(coordinates(pr),AcrProv,cex=0.7)
  brks.den <- round(q5.den$brks,2)
  leg.txt  <- paste(brks.den[-6], brks.den[-1], sep=" - ")
  legend("bottomright", fill=attr(cols.den,"palette"), legend=leg.txt ,bty="n")
  dev.off()
}

# *******End mapaaaaaa



W_cont_el<-poly2nb(pr, queen=TRUE)
W_cont_el_mat<-nb2listw(W_cont_el, style="W", zero.policy=TRUE)
W_cont_el_mat

#########################################
#### Spatial Regression              ####
#########################################


data <- dat
names(data)

moran.test(dat$exitoso, listw = W_cont_el_mat)

# pdf("moranexitoso.pdf")
moran.plot(dat$exitoso, listw = W_cont_el_mat, xlab = "% éxito", ylab = "Rezago espacial")
# dev.off()


########
## Linear Model
########
summary(dat)


mod.lm <- lm(exitoso ~ (ImasD+IngDis+Capa+AsisTec+Info+Pruebas+Publico+Apoyo+Financiero+log(GDPP)), data=dat)
# mod.lm <- lm(exitoso ~ (ImasD+IngDis+Info+actipor+actitrab+prod), data=data)
# cor(data[,c("ImasD","IngDis","Capa","AsisTec","Info","Pruebas","actipor","actitrab","prod")])
# cor(data[,c("ImasD","IngDis","AsisTec","Info","actipor","actitrab","prod")])
car::vif(mod.lm)
summary(mod.lm)
aux <- step(mod.lm)
mod.lm <- lm(formula(aux), data=dat)
summary(mod.lm)
coef(mod.lm)
shapiro.test(dat$exitoso) # hipótesis nula que una muestra x1, ..., xn proviene de una población normalmente distribuida
library(lmtest)
bptest(mod.lm)# Ho: No Heterocedasticidad


# pdf("desexitoso.pdf")
plot(density(dat$exitoso),main = "")
# dev.off()

## Plot residuals


res <- mod.lm$residuals

res.palette <- colorRampPalette(c("red","orange","white", "lightgreen","green"), space = "rgb")
pal <- res.palette(5)

classes_fx <- classIntervals(res, n=5, style="fixed", fixedBreaks=as.numeric(quantile(res)), rtimes = 1)
cols <- findColours(classes_fx,pal)

par(mar=rep(0,4))
plot(data,col=cols, main="Residuals from OLS Model", pretty=T, border="grey")
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Residuals from OLS Model",ncol=5)

dev.off()

## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

LM<-lm.LMtests(mod.lm, W_cont_el_mat, test="all")
print(LM)

########
## SAR Model (WARNING: This takes a while to run) Simultaneous Autorregresive Lag Model
########

mod.sar <- lagsarlm(formula(aux), data = dat, listw=W_cont_el_mat, zero.policy=T, na.action=na.exclude)
summary(mod.sar)
impacts(mod.sar, listw=W_cont_el_mat)

res <- mod.sar$residuals

classes_fx <- classIntervals(res, n=5, style="fixed", fixedBreaks=as.numeric(quantile(res)), rtimes = 1)
cols <- findColours(classes_fx,pal)

par(mar=rep(0,4))
plot(data,col=cols, border="grey",pretty=T)
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Residuals from SAR Model",ncol=5)

dev.off()

## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

########
## SEM Model (WARNING: This takes a while to run) Spatial Error Model
########


mod.sem <- errorsarlm(formula(aux), data = dat, listw=W_cont_el_mat, zero.policy=T, tol.solve=1e-15)
summary(mod.sem)
# "GDPP"     "ImasD"    "IngDis"   "Capa"     "AsisTec"  "Info"     "Pruebas" 
# [8] "prod"     "exitoso"  "actitrab" "actipor" 

res <- mod.sem$residuals

classes_fx <- classIntervals(res, n=5, style="fixed", fixedBreaks=as.numeric(quantile(res)), rtimes = 1)
cols <- findColours(classes_fx,pal)

## par(mar=rep(0,4))
plot(data,col=cols, border="grey",pretty=T)
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Residuals from SEM Model",ncol=5)


## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)
moran.test(mod.lm$residuals, listw=W_cont_el_mat, zero.policy=T)



########
## SDM Model (WARNING: This takes a while to run)
########


mod.sdm <- lagsarlm(formula(aux), data = dat, listw=W_cont_el_mat, zero.policy=T, type="mixed", tol.solve=1e-12)
summary(mod.sdm)
impacts(mod.sdm, listw=W_cont_el_mat)

res <- mod.sdm$residuals

classes_fx <- classIntervals(res, n=5, style="fixed", fixedBreaks=as.numeric(quantile(res)), rtimes = 1)
cols <- findColours(classes_fx,pal)

## par(mar=rep(0,4))
plot(data,col=cols, border="grey",pretty=T)
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Residuals from SDM Model",ncol=5)
dev.off()

## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)


#************************* TRES Y OTROS ***********************

#########################################
#### Spatial Regression              ####
#########################################

########
## Linear Model
########
summary(dat)


mod.lm <- lm(exitoso ~ (CapAsisInf+Otro+actipor+actitrab), data=data)
# mod.lm <- lm(exitoso ~ (ImasD+IngDis+Info+actipor+actitrab+prod), data=data)
# cor(data[,c("ImasD","IngDis","Capa","AsisTec","Info","Pruebas","actipor","actitrab","prod")])
# cor(data[,c("ImasD","IngDis","AsisTec","Info","actipor","actitrab","prod")])
summary(mod.lm)
aux <- formula(mod.lm)
mod.lm <- lm(formula(aux), data=data)
summary(mod.lm)



res <- mod.lm$residuals

## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

LM<-lm.LMtests(mod.lm, W_cont_el_mat, test="all")
print(LM)

########
## SAR Model (WARNING: This takes a while to run) Simultaneous Autorregresive Lag Model
########

mod.sar <- lagsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, na.action=na.exclude)
summary(mod.sar)
impacts(mod.sar, listw=W_cont_el_mat)

res <- mod.sar$residuals

## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

########
## SEM Model (WARNING: This takes a while to run) Spatial Error Model
########


mod.sem <- errorsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, tol.solve=1e-15)
summary(mod.sem)
# "GDPP"     "ImasD"    "IngDis"   "Capa"     "AsisTec"  "Info"     "Pruebas" 
# [8] "prod"     "exitoso"  "actitrab" "actipor" 

res <- mod.sem$residuals


## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)
moran.test(mod.lm$residuals, listw=W_cont_el_mat, zero.policy=T)



########
## SDM Model (WARNING: This takes a while to run)
########


mod.sdm <- lagsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, type="mixed", tol.solve=1e-12)
summary(mod.sdm)
impacts(mod.sdm, listw=W_cont_el_mat)

res <- mod.sdm$residuals

classes_fx <- classIntervals(res, n=5, style="fixed", fixedBreaks=as.numeric(quantile(res)), rtimes = 1)
cols <- findColours(classes_fx,pal)

## par(mar=rep(0,4))
plot(data,col=cols, border="grey",pretty=T)
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Residuals from SDM Model",ncol=5)
dev.off()

## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)


########################## Models with PCA ##########################

mmm1 <- c("ImasD","IngDis","Pruebas")
mmm2 <- c("Capa","AsisTec","Info")
mmm3 <- c("IngDis","Pruebas","Capa","AsisTec","Info")

pc1 <- princomp(scale(dat[,mmm1]))
pc2 <- princomp(scale(dat[,mmm2]))
pc3 <- princomp(scale(dat[,mmm3]))
summary(pc1)
summary(pc2)
summary(pc3)




#*********************************** MODELO 1 ***********************************

#########################################
#### Spatial Regression              ####
#########################################


data <- cbind(dat$exitoso,pc1$scores[,1],pc2$scores[,1],scale(dat$ImasD),pc3$scores[,1],dat$prod,dat$actitrab,dat$actipor,log(dat$GDPP))
data <- as.data.frame(data)
names(data) <- c("exitoso","pc1","pc2","ImasD","pc3","prod","actitrab","actipor","GDP")


########
## Linear Model
########
summary(dat)


mod.lm <- lm(exitoso ~ (pc1+pc2+actipor+actitrab), data=data)#Mejor1
# mod.lm <- lm(exitoso ~ (pc1+pc2+actitrab), data=data)#Mejor1

summary(mod.lm)# aux <- step(mod.lm)
aux <- formula(mod.lm)
mod.lm <- lm(formula(aux), data=data)
summary(mod.lm)

## Plot residuals


res <- mod.lm$residuals


## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

LM<-lm.LMtests(mod.lm, W_cont_el_mat, test="all")
print(LM)

########
## SAR Model (WARNING: This takes a while to run) Simultaneous Autorregresive Lag Model
########

mod.sar <- lagsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, na.action=na.exclude)
summary(mod.sar)
impacts(mod.sar, listw=W_cont_el_mat)

res <- mod.sar$residuals


## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

########
## SEM Model (WARNING: This takes a while to run) Spatial Error Model
########


mod.sem <- errorsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, tol.solve=1e-15)
summary(mod.sem)
# "GDPP"     "ImasD"    "IngDis"   "Capa"     "AsisTec"  "Info"     "Pruebas" 
# [8] "prod"     "exitoso"  "actitrab" "actipor" 

res <- mod.sem$residuals


## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)
moran.test(mod.lm$residuals, listw=W_cont_el_mat, zero.policy=T)



########
## SDM Model (WARNING: This takes a while to run)
########


mod.sdm <- lagsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, type="mixed", tol.solve=1e-12)
summary(mod.sdm)
impacts(mod.sdm, listw=W_cont_el_mat)

res <- mod.sdm$residuals



## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)





#*********************************** MODELO 2 ***********************************

#########################################
#### Spatial Regression              ####
#########################################



########
## Linear Model
########
summary(dat)

mod.lm <- lm(exitoso ~ (ImasD+pc3+actipor+actitrab), data=data)#Mejor2

summary(mod.lm)# aux <- step(mod.lm)
aux <- formula(mod.lm)
mod.lm <- lm(formula(aux), data=data)
summary(mod.lm)

## Plot residuals


res <- mod.lm$residuals


## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

LM<-lm.LMtests(mod.lm, W_cont_el_mat, test="all")
print(LM)

########
## SAR Model (WARNING: This takes a while to run) Simultaneous Autorregresive Lag Model
########

mod.sar <- lagsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, na.action=na.exclude)
summary(mod.sar)
impacts(mod.sar, listw=W_cont_el_mat)

res <- mod.sar$residuals



## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

########
## SEM Model (WARNING: This takes a while to run) Spatial Error Model
########


mod.sem <- errorsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, tol.solve=1e-15)
summary(mod.sem)
# "GDPP"     "ImasD"    "IngDis"   "Capa"     "AsisTec"  "Info"     "Pruebas" 
# [8] "prod"     "exitoso"  "actitrab" "actipor" 

res <- mod.sem$residuals



## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)
moran.test(mod.lm$residuals, listw=W_cont_el_mat, zero.policy=T)



########
## SDM Model (WARNING: This takes a while to run)
########


mod.sdm <- lagsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, type="mixed", tol.solve=1e-12)
summary(mod.sdm)
impacts(mod.sdm, listw=W_cont_el_mat)

res <- mod.sdm$residuals

## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)


























########################## Models with PCA [2] ##########################

mmm1 <- c("ImasD","IngDis","Pruebas")
mmm2 <- c("Capa","AsisTec","Info")
mmm3 <- c("IngDis","Pruebas","Capa","AsisTec","Info")

pc1 <- princomp(scale(dat[,mmm1]))
pc2 <- princomp(scale(dat[,mmm2]))
pc3 <- princomp(scale(dat[,mmm3]))
summary(pc1)
summary(pc2)
summary(pc3)




#*********************************** MODELO 1 ***********************************

#########################################
#### Spatial Regression              ####
#########################################


data <- cbind(dat$exitoso,pc1$scores[,1],pc2$scores[,1],scale(dat$ImasD),pc3$scores[,1],dat$prod,dat$actitrab,dat$actipor,log(dat$GDPP),dat$PorImasD,dat$Publico,dat$Apoyo,+dat$Financiero)
data <- as.data.frame(data)
names(data) <- c("exitoso","pc1","pc2","ImasD","pc3","prod","actitrab","actipor","GDP","ImasDD","Publico","Apoyo","Financiero")


########
## Linear Model
########
summary(data)
summary(lm(exitoso ~ (pc1+pc2+Publico+GDP), data=data))
summary(lm(exitoso ~ (pc1+pc2+ImasDD+GDP), data=data))
summary(lm(exitoso ~ (pc1+pc2+Apoyo+GDP), data=data))
summary(lm(exitoso ~ (pc1+pc2+Financiero+GDP), data=data))



mod.lm <- lm(exitoso ~ (pc1+pc2+Publico+ImasDD+Apoyo+Financiero+GDP), data=data)#Mejor1
# mod.lm <- lm(exitoso ~ (pc1+pc2+actitrab), data=data)#Mejor1
step(mod.lm)
summary(mod.lm)# aux <- step(mod.lm)
aux <- step(mod.lm)
mod.lm <- lm(formula(aux), data=data)
summary(mod.lm)

## Plot residuals


res <- mod.lm$residuals


## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

LM<-lm.LMtests(mod.lm, W_cont_el_mat, test="all")
print(LM)

########
## SAR Model (WARNING: This takes a while to run) Simultaneous Autorregresive Lag Model
########

mod.sar <- lagsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, na.action=na.exclude)
summary(mod.sar)
impacts(mod.sar, listw=W_cont_el_mat)

res <- mod.sar$residuals


## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

########
## SEM Model (WARNING: This takes a while to run) Spatial Error Model
########


mod.sem <- errorsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, tol.solve=1e-15)
summary(mod.sem)
# "GDPP"     "ImasD"    "IngDis"   "Capa"     "AsisTec"  "Info"     "Pruebas" 
# [8] "prod"     "exitoso"  "actitrab" "actipor" 

res <- mod.sem$residuals


## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)
moran.test(mod.lm$residuals, listw=W_cont_el_mat, zero.policy=T)



########
## SDM Model (WARNING: This takes a while to run)
########


mod.sdm <- lagsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, type="mixed", tol.solve=1e-12)
summary(mod.sdm)
impacts(mod.sdm, listw=W_cont_el_mat)

res <- mod.sdm$residuals



## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)





#*********************************** MODELO 2 ***********************************

#########################################
#### Spatial Regression              ####
#########################################



########
## Linear Model
########
summary(dat)

mod.lm <- lm(exitoso ~ (ImasD+pc3+Publico+Apoyo+Financiero+GDP), data=data)#Mejor2
summary(mod.lm)
step(mod.lm)
car::vif(mod.lm)

summary(mod.lm)# aux <- step(mod.lm)
aux <- step(mod.lm)
mod.lm <- lm(formula(aux), data=data)
summary(mod.lm)

## Plot residuals


res <- mod.lm$residuals


## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

LM<-lm.LMtests(mod.lm, W_cont_el_mat, test="all")
print(LM)

########
## SAR Model (WARNING: This takes a while to run) Simultaneous Autorregresive Lag Model
########

mod.sar <- lagsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, na.action=na.exclude)
summary(mod.sar)
impacts(mod.sar, listw=W_cont_el_mat)

res <- mod.sar$residuals



## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

########
## SEM Model (WARNING: This takes a while to run) Spatial Error Model
########


mod.sem <- errorsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, tol.solve=1e-15)
summary(mod.sem)
# "GDPP"     "ImasD"    "IngDis"   "Capa"     "AsisTec"  "Info"     "Pruebas" 
# [8] "prod"     "exitoso"  "actitrab" "actipor" 

res <- mod.sem$residuals



## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)
moran.test(mod.lm$residuals, listw=W_cont_el_mat, zero.policy=T)



########
## SDM Model (WARNING: This takes a while to run)
########


mod.sdm <- lagsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, type="mixed", tol.solve=1e-12)
summary(mod.sdm)
impacts(mod.sdm, listw=W_cont_el_mat)

res <- mod.sdm$residuals

## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)




#*********************************** MODELO 3 ***********************************

#########################################
#### Spatial Regression              ####
#########################################



########
## Linear Model
########
summary(dat)

mod.lm <- lm(exitoso ~ (pc3+Publico+Apoyo+Financiero+GDP), data=data)#Mejor2
summary(mod.lm)
step(mod.lm)
car::vif(mod.lm)

summary(mod.lm)# aux <- step(mod.lm)
aux <- formula(mod.lm)
mod.lm <- lm(formula(aux), data=data)
summary(mod.lm)

## Plot residuals


res <- mod.lm$residuals


## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

LM<-lm.LMtests(mod.lm, W_cont_el_mat, test="all")
print(LM)

########
## SAR Model (WARNING: This takes a while to run) Simultaneous Autorregresive Lag Model
########

mod.sar <- lagsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, na.action=na.exclude)
summary(mod.sar)
impacts(mod.sar, listw=W_cont_el_mat)

res <- mod.sar$residuals



## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

########
## SEM Model (WARNING: This takes a while to run) Spatial Error Model
########


mod.sem <- errorsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, tol.solve=1e-15)
summary(mod.sem)
# "GDPP"     "ImasD"    "IngDis"   "Capa"     "AsisTec"  "Info"     "Pruebas" 
# [8] "prod"     "exitoso"  "actitrab" "actipor" 

res <- mod.sem$residuals



## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)
moran.test(mod.lm$residuals, listw=W_cont_el_mat, zero.policy=T)



########
## SDM Model (WARNING: This takes a while to run)
########


mod.sdm <- lagsarlm(formula(aux), data = data, listw=W_cont_el_mat, zero.policy=T, type="mixed", tol.solve=1e-12)
summary(mod.sdm)

impacts(mod.sdm, listw=W_cont_el_mat)


(a <- round(median(data$pc3,na.rm=TRUE),10))
(b <- round(median(data$pc3,na.rm=TRUE)*2,10))
round(-6.713712e-02*(b-a),3)


(a <- round(mean(data$Publico,na.rm=TRUE),10))
(b <- round(mean(data$Publico,na.rm=TRUE)*2,10))
round(1.097694e-01*(b-a),2)


(a <- round(mean(data$Financiero,na.rm=TRUE),10))
(b <- round(mean(data$Financiero,na.rm=TRUE)*2,10))
round(2.671109e-03*(b-a),2)

res <- mod.sdm$residuals

## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)
names(dat)
mm <- lm(exitoso~actipor,data = dat)
summary(mm)
moran.test(resid(mm), listw=W_cont_el_mat, zero.policy=T)
