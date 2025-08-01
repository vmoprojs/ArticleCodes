rm( list = ls())
graphics.off()

###mapeo tesis ciudades RM
library(readxl)
library(tidyverse)
library(sp)
library(spdep)
library(spatialreg)
library(spd)
library(lmtest)
library(MTest)
library(tseries)


read_git_shp <- function(uu) {
  # Crear archivos temporales
  temp <- tempfile()
  temp2 <- tempfile()
  
  # Descargar el archivo ZIP
  download.file(uu, temp, mode = "wb")  # "wb" asegura descarga binaria en Windows
  
  # Descomprimir el archivo
  unzip(zipfile = temp, exdir = temp2)
  
  # Listar los archivos SHP en todas las subcarpetas
  your_SHP_file <- list.files(temp2, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  
  # Verificar si se encontró un archivo SHP
  if (length(your_SHP_file) == 0) {
    stop("No se encontró ningún archivo .shp en el ZIP descargado.")
  }
  
  # Obtener el nombre de la capa sin extensión
  layer_name <- tools::file_path_sans_ext(basename(your_SHP_file[1]))  # Usar el primer SHP encontrado
  
  # Leer el archivo SHP
  datos <- sf::st_read(your_SHP_file[1], layer = layer_name)
  
  # Eliminar archivos temporales
  unlink(temp)
  unlink(temp2, recursive = TRUE)
  
  return(datos)
}

uu <- "https://github.com/vmoprojs/DataLectures/raw/master/SpatialData/comunasStgo.zip"

poligonos <- read_git_shp(uu)
poligonos <- st_transform(poligonos,crs=4326)
uu <- "https://raw.githubusercontent.com/vmoprojs/DataLectures/refs/heads/master/SpatialData/famViolence.csv"
datosRM <- read.csv(uu)

rm <- poligonos %>% 
  filter(Region=="Región Metropolitana de Santiago")

AcronimRM <- c("SJQ","SM","SR","IND","LC","PÑL","PROV","LR","CDT","CLN","STG",
               "LMP","PIR","PTEA","HUE","SBER","CUR","MPIN","CRR","CNV",
               "VIT","CLI","EBQ","ECT","LFL","LGJ","LPNA","LCD","LBNA","LESP","LPR","MAC","MAI","ÑUÑ","PAG",
               "PUD","QUIL","QNM","REC","REN","ELMT","PH","PEÑ","TLG","PAI","IDM","BUI","SJMP","TT","MEL","SP","ALH")
centroidespointsrm <- st_centroid(rm)

centroides <- cbind(rm,st_coordinates(st_centroid(rm$geometry)),AcronimRM)

## ST Fig 1: División geográfica comunas
ggplot(data=rm)+
  geom_sf()+
  geom_sf(data=centroides,fill="lightblue")+
  geom_text(data=centroides,aes(x=X,y=Y,label=AcronimRM),size=2.5)+
  labs(title = "Geographic division of the metropolitan region of Chile")
## END Fig 1: División geográfica comunas


distgeodescentr <- st_distance(centroidespointsrm)
rownames(distgeodescentr) <- AcronimRM
colnames(distgeodescentr) <- AcronimRM
W_RM <- poly2nb(rm, queen = T)
W_estandRM <- nb2mat(W_RM, style = "W",zero.policy = NULL)
rownames(W_estandRM) <- AcronimRM
colnames(W_estandRM) <- AcronimRM
W_objetoRM <- mat2listw(W_estandRM)
W_objetoRM <- nb2listw(W_RM, style = "W",zero.policy = NULL)
base1rm <- datosRM %>% left_join(rm,by="Comuna")
# plot(rm[,12],border="black",main="Conexiones generadas con la matriz W\n según criterio de la reina ")
# plot(W_RM,st_coordinates(st_centroid(rm$geometry)),col="red",add=TRUE)
names(base1rm)
base2rm <- centroides %>% left_join(base1rm)

plot(rm[,12],border="black",main="Connections with the Queen’s criterion ")
plot(W_RM,st_coordinates(st_centroid(rm$geometry)),col="red",add=TRUE)
############   Lagrange Multiplier Tests ############
# --- Spatial Weights
coords <- st_coordinates(st_centroid(rm$geometry))

#
#if(knntest)
#{
#  solKNN_AIC <- NULL
# for(m in 2:10)
# {
#    # m = 2
#   Wknn_test <- knearneigh(coords, k = m, longlat = TRUE)
#   cc_test <- nb2listw(knn2nb(Wknn_test))
#    slmKNN_Test <- lagsarlm(tasa_vifm~aep+dens_pob ,
#                            listw=cc_test,
#                           method = "MC", zero.policy=T,data = base2rm)
#   aux <- -2*slmKNN_Test$LL+2*slmKNN_Test$parameters
#   solKNN_AIC <- c(solKNN_AIC,aux)
#   print(m)
# }
# which.min(solKNN_AIC);min(solKNN_AIC)
# plot(solKNN_AIC)
#  summary(solKNN_AIC)
#}


# **** ST: Selección del modelo general ***
val <- min(base2rm$ing_alcohol[base2rm$ing_alcohol>0])
base2rm$tasa_ing_alcohol <- (base2rm$ing_alcohol+val)/base2rm$Total_Poblacional


base2rm$PNB <- max(base2rm$Porc_no_Básica)-base2rm$Porc_no_Básica


acp2 <- princomp(as.data.frame(base2rm)[,c("PNB","aep")])


base2rm$educ <- acp2$scores[,1]
acp2$loadings


mm <- min(base2rm$densidad_2022)
mx <- max(base2rm$densidad_2022)

vals <- seq(100,10000)
sol <- NULL
for(i in vals)
{
  # i = vals[1]
  aux <- (base2rm$densidad_2022>i)*1
  s <- ks.test(log(base2rm$tasa_vif)[aux==1],log(base2rm$tasa_vif)[aux==0])$statistic
  sol <- c(sol,s)
}
plot(vals,sol,t = "l")
sol[which.max(sol)]
max_ks <- vals[which.max(sol)]


auxlm <- lm(log(tasa_vif)~(tasa_ing_alcohol)+educ+I(densidad_2022>3718),data = base2rm)
summary(auxlm)

auxlm <- lm(log(tasa_vif)~(tasa_ing_alcohol)+educ+I(densidad_2022>max_ks),data = base2rm)
summary(auxlm)


ms <- step(auxlm)
summary(ms)

# MTest::MTest(auxlm,nboot = 500) # Ojo cambiar no pesca el LOG



# **** END: Selección del modelo general ***


# **** ST: Estimacion I moran con diferentes W ***

# ST * W

W_objetoRM

#Matriz de vecinos mas cercanos k=5
kk <- 5
kneigh <- knearneigh(coords, k = kk, longlat = TRUE)
knnd <- nb2listw(knn2nb(kneigh))


# Matriz de pesos inversa
dist.mat <- as.matrix(dist(coords, method = "euclidean"))
dist.mat[1:5, 1:5]

dist.mat.inv <- 1 / dist.mat # 1 / d_{ij}
diag(dist.mat.inv) <- 0 # 0 in the diagonal
dist.mat.inv[1:5, 1:5]

# Matriz de pesos inversa estandarizada
id <- mat2listw(dist.mat.inv, style = "W", row.names = AcronimRM)
summary(id)



# * END: W 

y <- with(base2rm,log(tasa_vif))
x1 <- with(base2rm,tasa_ing_alcohol)
x2 <- with(base2rm,educ)
x3 <- with(base2rm,densidad_2022>max_ks)


moW <- moran.test(y, listw = W_objetoRM)
moK <- moran.test(y, listw = knnd)
moI <- moran.test(y, listw = id)

moW_x1 <- moran.test(x1, listw = W_objetoRM)
moK_x1 <- moran.test(x1, listw = knnd)
moI_x1 <- moran.test(x1, listw = id)

moW_x2 <- moran.test(x2, listw = W_objetoRM)
moK_x2 <- moran.test(x2, listw = knnd)
moI_x2 <- moran.test(x2, listw = id)

x3 <- x3*1

moW_x3 <- moran.test(x3, listw = W_objetoRM)
moK_x3 <- moran.test(x3, listw = knnd)
moI_x3 <- moran.test(x3, listw = id)



moW_rf <- moran.test(y, listw = W_objetoRM,randomisation=FALSE)
moK_rf <- moran.test(y, listw = knnd,randomisation=FALSE)
moI_rf <- moran.test(y, listw = id,randomisation=FALSE)

moW_rf_x1 <- moran.test(x1, listw = W_objetoRM,randomisation=FALSE)
moK_rf_x1 <- moran.test(x1, listw = knnd,randomisation=FALSE)
moI_rf_x1 <- moran.test(x1, listw = id,randomisation=FALSE)

moW_rf_x2 <- moran.test(x2, listw = W_objetoRM,randomisation=FALSE)
moK_rf_x2 <- moran.test(x2, listw = knnd,randomisation=FALSE)
moI_rf_x2 <- moran.test(x2, listw = id,randomisation=FALSE)


moW_rf_x3 <- moran.test(x3, listw = W_objetoRM,randomisation=FALSE)
moK_rf_x3 <- moran.test(x3, listw = knnd,randomisation=FALSE)
moI_rf_x3 <- moran.test(x3, listw = id,randomisation=FALSE)



pol <- 999
set.seed(8519)
moWmc <- moran.mc(y, listw = W_objetoRM,pol)
(ZmoWmc <- (moWmc$statistic-mean(moWmc$res))/sd(moWmc$res))
moKmc <- moran.mc(y, listw = knnd,pol)
(ZmoKmc <- (moKmc$statistic-mean(moKmc$res))/sd(moKmc$res))
moImc <- moran.mc(y, listw = id,pol)
(ZmoImc <- (moImc$statistic-mean(moImc$res))/sd(moImc$res))

moWmc_x1 <- moran.mc(x1, listw = W_objetoRM,pol)
(ZmoWmc_x1 <- (moWmc_x1$statistic-mean(moWmc_x1$res))/sd(moWmc_x1$res))
moKmc_x1 <- moran.mc(x1, listw = knnd,pol)
(ZmoKmc_x1 <- (moKmc_x1$statistic-mean(moKmc_x1$res))/sd(moKmc_x1$res))
moImc_x1 <- moran.mc(x1, listw = id,pol)
(ZmoImc_x1 <- (moImc_x1$statistic-mean(moImc_x1$res))/sd(moImc_x1$res))


moWmc_x2 <- moran.mc(x2, listw = W_objetoRM,pol)
(ZmoWmc_x2 <- (moWmc_x2$statistic-mean(moWmc_x2$res))/sd(moWmc_x2$res))
moKmc_x2 <- moran.mc(x2, listw = knnd,pol)
(ZmoKmc_x2 <- (moKmc_x2$statistic-mean(moKmc_x2$res))/sd(moKmc_x2$res))
moImc_x2 <- moran.mc(x2, listw = id,pol)
(ZmoImc_x2 <- (moImc_x2$statistic-mean(moImc_x2$res))/sd(moImc_x2$res))

moWmc_x3 <- moran.mc(x3, listw = W_objetoRM,pol)
(ZmoWmc_x3 <- (moWmc_x3$statistic-mean(moWmc_x3$res))/sd(moWmc_x3$res))
moKmc_x3 <- moran.mc(x3, listw = knnd,pol)
(ZmoKmc_x3 <- (moKmc_x3$statistic-mean(moKmc_x3$res))/sd(moKmc_x3$res))
moImc_x3 <- moran.mc(x3, listw = id,pol)
(ZmoImc_x3 <- (moImc_x3$statistic-mean(moImc_x3$res))/sd(moImc_x3$res))


# Agregado variable Y
Sol1 <- t(rbind(c(moW$estimate,moW$statistic,moW$p.value),
                c(moW_rf$estimate,moW_rf$statistic,moW_rf$p.value),
                c(moWmc$statistic,mean(moWmc$res),var(moWmc$res),ZmoWmc,moWmc$p.value)))
rownames(Sol1) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol1) <- c("Randomization","Normal","Monte Carlo")
Sol1


Sol_x1 <- t(rbind(c(moW_x1$estimate,moW_x1$statistic,moW_x1$p.value),
                  c(moW_rf_x1$estimate,moW_rf_x1$statistic,moW_rf_x1$p.value),
                  c(moWmc_x1$statistic,mean(moWmc_x1$res),var(moWmc_x1$res),ZmoWmc_x1,moWmc_x1$p.value)))
rownames(Sol_x1) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol_x1) <- c("Randomization","Normal","Monte Carlo")
Sol_x1


Sol_x2 <- t(rbind(c(moW_x2$estimate,moW_x2$statistic,moW_x2$p.value),
                  c(moW_rf_x2$estimate,moW_rf_x2$statistic,moW_rf_x2$p.value),
                  c(moWmc_x2$statistic,mean(moWmc_x2$res),var(moWmc_x2$res),ZmoWmc_x2,moWmc_x2$p.value)))
rownames(Sol_x2) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol_x2) <- c("Randomization","Normal","Monte Carlo")
Sol_x2


Sol_x3 <- t(rbind(c(moW_x3$estimate,moW_x3$statistic,moW_x3$p.value),
                  c(moW_rf_x3$estimate,moW_rf_x3$statistic,moW_rf_x3$p.value),
                  c(moWmc_x3$statistic,mean(moWmc_x3$res),var(moWmc_x3$res),
                    ZmoWmc_x3,moWmc_x3$p.value)))
rownames(Sol_x3) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol_x3) <- c("Randomization","Normal","Monte Carlo")
Sol_x3


Sol1_K <- t(rbind(c(moK$estimate,moK$statistic,moK$p.value),
                  c(moK_rf$estimate,moK_rf$statistic,moK_rf$p.value),
                  c(moKmc$statistic,mean(moKmc$res),var(moKmc$res),ZmoKmc,moKmc$p.value)))
rownames(Sol1_K) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol1_K) <- c("Randomization","Normal","Monte Carlo")
Sol1_K


Sol_x1_K <- t(rbind(c(moK_x1$estimate,moK_x1$statistic,moK_x1$p.value),
                    c(moK_rf_x1$estimate,moK_rf_x1$statistic,moK_rf_x1$p.value),
                    c(moKmc_x1$statistic,mean(moKmc_x1$res),var(moKmc_x1$res),
                      ZmoKmc_x1,moKmc_x1$p.value)))
rownames(Sol_x1_K) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol_x1_K) <- c("Randomization","Normal","Monte Carlo")
Sol_x1_K


Sol_x2_K <- t(rbind(c(moK_x2$estimate,moK_x2$statistic,moK_x2$p.value),
                    c(moK_rf_x2$estimate,moK_rf_x2$statistic,moK_rf_x2$p.value),
                    c(moKmc_x2$statistic,mean(moKmc_x2$res),
                      var(moKmc_x2$res),ZmoKmc_x2,moKmc_x2$p.value)))
rownames(Sol_x2_K) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol_x2_K) <- c("Randomization","Normal","Monte Carlo")
Sol_x2_K

Sol_x3_K <- t(rbind(c(moK_x3$estimate,moK_x3$statistic,moK_x3$p.value),
                    c(moK_rf_x3$estimate,moK_rf_x3$statistic,moK_rf_x3$p.value),
                    c(moKmc_x3$statistic,mean(moKmc_x3$res),
                      var(moKmc_x3$res),ZmoKmc_x3,moKmc_x3$p.value)))
rownames(Sol_x3_K) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol_x3_K) <- c("Randomization","Normal","Monte Carlo")
Sol_x3_K


Sol_I <- t(rbind(c(moI$estimate,moI$statistic,moI$p.value),
                 c(moI_rf$estimate,moI_rf$statistic,moI_rf$p.value),
                 c(moImc$statistic,mean(moImc$res),
                   var(moImc$res),ZmoImc,moImc$p.value)))
rownames(Sol_I) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol_I) <- c("Randomization","Normal","Monte Carlo")
Sol_I



Sol_x1_I <- t(rbind(c(moI_x1$estimate,moI_x1$statistic,moI_x1$p.value),
                    c(moI_rf_x1$estimate,moI_rf_x1$statistic,moI_rf_x1$p.value),
                    c(moImc_x1$statistic,mean(moImc_x1$res),
                      var(moImc_x1$res),ZmoImc_x1,moImc_x1$p.value)))
rownames(Sol_x1_I) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol_x1_I) <- c("Randomization","Normal","Monte Carlo")
Sol_x1_I

Sol_x2_I <- t(rbind(c(moI_x2$estimate,moI_x2$statistic,moI_x2$p.value),
                    c(moI_rf_x2$estimate,moI_rf_x2$statistic,moI_rf_x2$p.value),
                    c(moImc_x2$statistic,mean(moImc_x2$res),
                      var(moImc_x2$res),ZmoImc_x2,moImc_x2$p.value)))
rownames(Sol_x2_I) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol_x2_I) <- c("Randomization","Normal","Monte Carlo")
Sol_x2_I

Sol_x3_I <- t(rbind(c(moI_x3$estimate,moI_x3$statistic,moI_x3$p.value),
                    c(moI_rf_x3$estimate,moI_rf_x3$statistic,moI_rf_x3$p.value),
                    c(moImc_x3$statistic,mean(moImc_x3$res),
                      var(moImc_x3$res),ZmoImc_x3,moImc_x3$p.value)))
rownames(Sol_x3_I) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol_x3_I) <- c("Randomization","Normal","Monte Carlo")
Sol_x3_I



kk <- 5
sol_pval_moranI <- NULL
for( kk in 2:50)
{
  # kk= 2
  kneigh_aux <- knearneigh(coords, k = kk, longlat = TRUE)
  knnd_aux <- nb2listw(knn2nb(kneigh))
  
  moWmc_x2_aux <- moran.mc(x1, listw = knnd_aux,pol)
  # (ZmoWmc_x2_aux <- (moWmc_x2_aux$statistic-mean(moWmc_x2_aux$res))/sd(moWmc_x2_aux$res))
  sol_pval_moranI <- c(sol_pval_moranI,moWmc_x2_aux$p.value)
  # print(sol_pval_moranI)
}
sol_pval_moranI

# ST: Graficos de Moran de las variables:
vl <- "Domestic Violence"
moran.plot(y,W_objetoRM,xlab = vl,ylab = paste("Spatially lagged",vl))
# vl <- "Alcoholic consumption"
# moran.plot(x1,W_objetoRM,xlab = vl,ylab = paste("Spatially lagged",vl))
vl <- "Education"
moran.plot(x2,W_objetoRM,xlab = vl,ylab = paste("Spatially lagged",vl))
vl <- "Population density"
moran.plot(x3,W_objetoRM,xlab = vl,ylab = paste("Spatially lagged",vl))
# END: Graficos de Moran de las variables

# ST: Graficos de matriz de contiguidad:


# Create a contiguity-based neighborhood structure (Queen adjacency)
W_RM
# Plot the base map of the counties
plot(st_geometry(base2rm), border = "gray", main = "Contiguity Matrix")
# Add the contiguity links (Queen's adjacency) in red
plot(W_RM, st_coordinates(st_centroid(base2rm)), col = "red", lwd = 2, add = TRUE)



# END: Graficos de matriz de contiguidad


ggplot()+
  geom_sf(data=base2rm,aes(fill=(tasa_vifm)))+
  scale_fill_distiller(palette =3,direction = 1)+
  #scale_fill_viridis_c(option="viridis",direction=1)+
  geom_text(data=centroides,aes(x=X,y=Y,label=AcronimRM),size=2)+
  labs(title="Domestic violence M", subtitle = "",
       fill =expression( habs/km^2))+
  theme_gray()

# ST: Graficos de variables
ggplot()+
  geom_sf(data=base2rm,aes(fill=log(tasa_vif)))+
  scale_fill_distiller(palette =3,direction = 1)+
  #scale_fill_viridis_c(option="viridis",direction=1)+
  geom_text(data=centroides,aes(x=X,y=Y,label=AcronimRM),size=2)+
  labs(title="Domestic violence", subtitle = "",
       fill =expression( habs/km^2))+
  theme_gray()

ggplot()+
  geom_sf(data=base2rm,aes(fill=tasa_ing_alcohol))+
  scale_fill_distiller(palette =3,direction = 1)+
  #scale_fill_viridis_c(option="viridis",direction=1)+
  geom_text(data=centroides,aes(x=X,y=Y,label=AcronimRM),size=2)+
  labs(title="Alcohol", subtitle = "",
       fill =expression( habs/km^2))+
  theme_gray()

ggplot()+
  geom_sf(data=base2rm,aes(fill=educ))+
  scale_fill_distiller(palette =3,direction = 1)+
  #scale_fill_viridis_c(option="viridis",direction=1)+
  geom_text(data=centroides,aes(x=X,y=Y,label=AcronimRM),size=2)+
  labs(title="Education", subtitle = "",
       fill ="")+
  theme_gray()

ggplot()+
  geom_sf(data=base2rm,aes(fill=densidad_2022))+
  scale_fill_distiller(palette =3,direction = 1)+
  #scale_fill_viridis_c(option="viridis",direction=1)+
  geom_text(data=centroides,aes(x=X,y=Y,label=AcronimRM),size=2)+
  labs(title="Density", subtitle = "",
       fill ="")+
  theme_gray()

ggplot() +
  geom_sf(data = base2rm, aes(fill = densidad_2022 > 966)) +
  scale_fill_brewer(palette = "Set1") +  # Usa una paleta adecuada para variables categóricas
  # scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +  # Otra opción manual
  geom_text(data = centroides, aes(x = X, y = Y, label = AcronimRM), size = 2) +
  labs(title = "Density", fill = "Above 966") +
  theme_gray()



# ********************** ST: Test de Lagrange. **********************
hist(y)
jarque.bera.test(y)
qqnorm(y);qqline(y)
mod.lm <- lm(y ~ x1+x2+x3,data = base2rm)


RS <- lm.RStests(mod.lm, W_objetoRM, test="all")

SD.RStests(mod.lm,W_objetoRM)

RS$RSerr
RS$RSlag

RSknn5 <- lm.RStests(mod.lm, knnd, test="all")
RSknn5$RSerr
RSknn5$RSlag

RSid <- lm.RStests(mod.lm, id, test="all")
RSid$RSerr
RSid$RSlag



W_Contiguity <- c(c(RS$RSlag$statistic,RS$RSlag$p.value),
                  c(RS$adjRSlag$statistic,RS$adjRSlag$p.value),
                  c(RS$RSerr$statistic,RS$RSerr$p.value),
                  c(RS$adjRSerr$statistic,RS$adjRSerr$p.value))

W_K5 <- c(c(RSknn5$RSlag$statistic,RSknn5$RSlag$p.value),
          c(RSknn5$adjRSlag$statistic,RSknn5$adjRSlag$p.value),
          c(RSknn5$RSerr$statistic,RSknn5$RSerr$p.value),
          c(RSknn5$adjRSerr$statistic,RSknn5$adjRSerr$p.value))

W_ID <- c(c(RSid$RSlag$statistic,RSid$RSlag$p.value),
          c(RSid$adjRSlag$statistic,RSid$adjRSlag$p.value),
          c(RSid$RSerr$statistic,RSid$RSerr$p.value),
          c(RSid$adjRSerr$statistic,RSid$adjRSerr$p.value))



rbind(W_Contiguity,W_K5,W_ID)
# rbind(W_Contiguity1,W_Distance1,W_K51,W_K101,W_ID1)



# Wald Test 
summary(lm(y~x1+x2+x3))
sac <- sacsarlm(y~x1+x2+x3, listw = W_objetoRM,method = "eigen",type="sacmixed")
sdm <- lagsarlm(y~x1+x2+x3, listw = W_objetoRM,method = "eigen",type = "mixed")
slm <- lagsarlm(y~x1+x2+x3, listw = W_objetoRM,method = "eigen")
sem <- errorsarlm(y~x1+x2+x3, listw = W_objetoRM,method = "eigen")


sac <- sacsarlm(y~x1+x2+x3, listw = W_objetoRM,method = "MC",type="sacmixed")
sdm <- lagsarlm(y~x1+x2+x3, listw = W_objetoRM,method = "MC",type = "mixed")
slm <- lagsarlm(y~x1+x2+x3, listw = W_objetoRM,method = "MC")
sem <- errorsarlm(y~x1+x2+x3, listw = W_objetoRM,method = "MC")

summary(sac)

# mod.lm <- lm(y~x2)
# summary(mod.lm)
pl <- 9999
moran.mc(resid(mod.lm),listw = W_objetoRM,nsim = pl)
moran.mc(resid(sac),listw = W_objetoRM,nsim = pl)
moran.mc(resid(sdm),listw = W_objetoRM,nsim = pl)
moran.mc(resid(slm),listw = W_objetoRM,nsim = pl)
moran.mc(resid(sem),listw = W_objetoRM,nsim = pl)


# Ho:absence of spatial dependence
Wald1.Sarlm(sac)
Wald1.Sarlm(sdm)
Wald1.Sarlm(slm)
Wald1.Sarlm(sem)

LR1.Sarlm(sac)
LR1.Sarlm(sdm)
LR1.Sarlm(slm)
LR1.Sarlm(sem)



# sac vs otros, no hay diferencias, se prefiere el otro:
LR.Sarlm(sac,sdm)
LR.Sarlm(sac,slm)
LR.Sarlm(sac,sem)

# sdm vs otros, no hay diferencias, se prefiere el otro:
LR.Sarlm(sdm,slm)
LR.Sarlm(sdm,sem)
# LR.Sarlm(slm,sem)
LR.Sarlm(sem,sdm)

summary(sac)
summary(sdm)
summary(slm)
summary(sem)

summary(mod.lm)

######### Residuals Test   ###############

###OLS

res<-residuals(mod.lm)
qqnorm(res);qqline(res)
hist(res,freq=F,breaks=6)
jarque.bera.test(res)
bptest(mod.lm)
Box.test(res, lag = 1, type = "Ljung-Box")

##SLM

resslm<-residuals(slm)
qqnorm(resslm);qqline(resslm)
hist(resslm,freq=F,breaks=6)
jarque.bera.test(resslm)
bptest.Sarlm(slm)
Box.test(resslm, lag = 1, type = "Ljung-Box")



##### TABLA 7 Y 8 #####


### ***** ST AIC and KNN method

LL <- as.numeric(slm$LLNullLlm)
pp <- slm$parameters
which.min(-2*LL+2*pp)
-2*LL+2*pp #AIC

-2*as.numeric(logLik(mod.lm))+2*5
AIC(mod.lm)

summary(slm)
# ********************** END: Test de Lagrange. **********************


# ********************** ST: Local Moran. **********************

varo <- base2rm$tasa_vif
bjo <- base2rm$AcronimRM
locM <- localmoran(varo, W_objetoRM,alternative = "greater")
oid1 <- order(bjo)
locMorMat <- printCoefmat(data.frame(locM[oid1,], row.names=bjo[oid1]), check.names=FALSE)
# Renombrar la columna de probabilidad
names(locMorMat)[5] <- "Prob"

# Veamos los valores significativos <0.05
la <- locMorMat[which(locMorMat$Prob<0.05),]
la

# Veamos los valores significativos > 0.95
lb <- na.omit(locMorMat[locMorMat$Prob>0.95,])
lb

rbind(la,lb)


# Definir los puntos de corte corregidos
brks <- c(-Inf, 0.05, 0.95, Inf)

# Asignar etiquetas a las categorías
locMorMat$Category <- cut(locMorMat$Prob, breaks = brks, 
                          labels = c("High-High, locM > 0", "Non significant", "Low-Low, locM < 0"),
                          include.lowest = TRUE)

locMorMat$AcronimRM <- rownames(locMorMat)
base2rm <- (merge(base2rm,locMorMat[,c("Category","AcronimRM")],by= "AcronimRM"))



# Definir los puntos de corte corregidos
brks <- c(-Inf, 0.05, 0.95, Inf)
# Definir colores correspondientes a las categorías
cols <- c("royalblue", "springgreen", "yellow")

# Crear el gráfico con ggplot2
ggplot() +
  geom_sf(data = base2rm, aes(fill = Category)) +  # Mapa con colores según categorías de Local Moran
  scale_fill_manual(values = cols, name = "Local Moran") +  # Escala de colores
  geom_text(data = centroides, aes(x = X, y = Y, label = AcronimRM), size = 2) +  # Etiquetas en centroides
  labs(title = "Local Moran", fill = "Significance") +  
  theme_minimal()  # Tema más limpio para mapas


# ********************** END: Local Moran. **********************

# ********************** ST: efectos directos indirectos. *********************

library(coda)

#*** Continuity
W <- as(W_estandRM, "CsparseMatrix")
trMatc <- trW(W, type="mult")
trMC <- trW(W, type="MC")
# impactos_slm <- spatialreg::impacts(slm, tr=trMatc, R=1000, Q=5)
impactos_slm <- spatialreg::impacts(slm, tr=trMC, R=5000, Q=5)
imp_slm <- summary(impactos_slm, zstats=TRUE,short = TRUE,reportQ = TRUE)
imp_slm$res
imp_slm$pzmat

# x1
o <- 1
slm_cont_imp_x1 <- cbind(c(imp_slm$res$total[o],
                           imp_slm$res$direct[o],
                           imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                      imp_slm$pzmat[o,1],
                                                      imp_slm$pzmat[o,2]))

# x2
o <- 2
slm_cont_imp_x2 <- cbind(c(imp_slm$res$total[o],
                           imp_slm$res$direct[o],
                           imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                      imp_slm$pzmat[o,1],
                                                      imp_slm$pzmat[o,2]))

# x3
o <- 3
slm_cont_imp_x3 <- cbind(c(imp_slm$res$total[o],
                           imp_slm$res$direct[o],
                           imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                      imp_slm$pzmat[o,1],
                                                      imp_slm$pzmat[o,2]))

imp_cont <- rbind(slm_cont_imp_x1,slm_cont_imp_x2,slm_cont_imp_x3)
imp_cont


# ********************** END: efectos directos indirectos. *********************










# ********************** ST: efectos fila y columna. *********************

# ********************** x2: Educ *********************
#Función para determinar efectos marginales de los modelos

W <- listw2mat(W_objetoRM)                  # Matriz de distancias por contiguidad en formato matriz
colnames(W) <- rownames(W) <- AcronimRM             # Se asignan nombres de filas a columna




margin.effect <- function(modelo, W, b, first, Durbin="False") 
  
  #W es matriz de pesos en formato matriz, b es el número del coeficiente que se desdea estima
  # first es el tamaño del top de cantones, Durbin especifica si el modelo es DURBIN o Lag
{
  
  if(Durbin=="False"){                             #Si es un modelo lag se extraen coeficiente de X
    coef <- data.frame(modelo$coefficients)         #Se estraen coeficientes beta del modelo lag
    beta  <-coef[b+1,1]                             #Se toma el coeficiente número b sin contar el intercepto
  } else {                                        #Si es un modelo Durbin se toma coeficiente de X y lag de X                   
    coef  <- data.frame(modelo$coefficients)        #Se estraen coeficientes beta del modelo Durbin
    beta  <-coef[b+1,1]                             #Coeficiente de X
    beta2  <-coef[b+4,1]                            #Coeficiente de lag de X
  }
  
  if(Durbin=="False"){                             #Condicional para modelo Lag y Durbin
    A <- solve(diag(nrow(W)) - modelo$rho*W)      #Se estima matriz A de modelo Lag
    S <- A%*%diag(nrow(W))*beta                 #Se estima matriz S de modelo Lag
    
  } else {
    A <- solve(diag(nrow(W)) - modelo$rho*W)    #Se estima matriz A de modelo Durbin
    S <- A%*%(diag(nrow(W))*beta + W*beta2)     #Se estima matriz S de modelo Durbin
  }
  
  colnames(S) <- rownames(S)                      #Se asigna código de cantón a las columnas
  
  I <- matrix(rep(1, nrow(W)), nrow = 1)          #Se estiman los efectos totales, indirectos y directos
  
  e.total  <- I%*%S%*%t(I)/nrow(S)
  e.dirct  <- sum(diag(S))/nrow(S)
  e.indi   <- e.total - e.dirct
  
  impa <- data.frame(c(e.dirct,e.indi,e.total))   #Se crea base de efectos
  rownames(impa) <- c("efecto directo","efecto indirecto","efecto total")
  colnames(impa) <- c("valor")
  
  #Efecto Columna por Cantón
  
  E.ind.col <- matrix(NA,nrow = ncol(S))         #Se crea contenedor de efectos columna
  
  for (i in 1:ncol(S)) 
  {
    E.ind.col[i] <- sum(S[,i])-S[i,i]           #Se estiman efectos columna
  }
  # head(colSums(S)-diag(S))
  # head(as.numeric(E.ind.col))
  
  rownames(E.ind.col) <- rownames(S)            #Se asigna el codigo de cantón a los efectos columna
  
  datos_ordenados.col <- data.frame(E.ind.col[order(E.ind.col),]) #Se ordena por magnitud del efecto
  datos_ordenados.col <- E.ind.col #Se ordena por magnitud del efecto
  colnames(datos_ordenados.col)[1] <- "Efectos"                      
  principales.col <- tail(datos_ordenados.col, first)             #Se toman los primero "first" cantones con mayor efecto
  bar.col<- barplot(as.numeric(principales.col), names.arg = rownames(principales.col), ylab = "Valores", xlab = "Categorías", main = "Efecto indirecto sobre otros cantones")
  
  #Efecto Fila por Cantón
  
  E.ind.row <-matrix(NA,nrow = ncol(S))         #Se crea contenedor de efectos columna
  
  for (i in 1:nrow(S)) 
  {
    E.ind.row[i] <- sum(S[i,])-S[i,i]           #Se estiman efectos columna
  }
  
  rownames(E.ind.row) <- rownames(S)            #Se asigna el codigo de cantón a los efectos columna
  
  datos_ordenados.row <- data.frame(E.ind.row[order(E.ind.row),]) #Se ordena por magnitud del efecto
  datos_ordenados.row <- E.ind.row #Se ordena por magnitud del efecto
  colnames(datos_ordenados.row)[1] <- "Efectos"                      
  principales.row <- tail(datos_ordenados.row, first)             #Se toman los primero "first" cantones con mayor efecto
  bar.row<- barplot(as.numeric(principales.row), names.arg = rownames(principales.row), ylab = "Valores", xlab = "Categorías", main = "Efecto indirecto de otros cantones sobre el canton")
  
  return(list(A=A, S=S, efectos = impa,
              plot.col = bar.col, efectos.margin.col = datos_ordenados.col, efectos.margin.col.first=principales.col,
              plot.row = bar.row, efectos.margin.row = datos_ordenados.row, efectos.margin.row.first=principales.row))
}

#Efectos en el modelo Lag
Lag.effect <- margin.effect(slm, W, 2, 20, Durbin="False")   #Se obtienen los efectos marginales del modelo lag                

#Para comprobar comparamos los efectos directos, indirectos y totales con la funcion impacts

Lag.effect$efectos                     #Estimación con la funcion creada
impacts(slm, listw = W_objetoRM)    #Estimación con la función impats

#Efectos Columna#EfecW_estandRMtos Columna
Lag.effect.col <- Lag.effect$efectos.margin.col
columna <- data.frame(Lag.effect.col)
columna$Codigo <- rownames(Lag.effect.col)

#Efectos Fila
Lag.effect.row <- Lag.effect$efectos.margin.row
fila <- data.frame(Lag.effect.row)
fila$Codigo <- rownames(Lag.effect.row)

proof <- sp::merge(base2rm, columna, by.x = "AcronimRM", by.y = "Codigo", all.x = T)  #Se unifica en una sola base con datos de geo referenciación
colnames(proof)[34] <- "Efecto Columna"
proof <- sp::merge(proof, fila, by.x = "AcronimRM", by.y = "Codigo", all.x = T)
colnames(proof)[35] <- "Efectos Fila"

Efectos <- proof[, c(16,34:35)] #Se mantienen variables de efectos, tamano poblacional y economico
Efectos <- data.frame(Efectos)


cor.test(log(Efectos$tasa_vif),log(Efectos$Efecto.Columna),method = "pearson")
plot(log(Efectos$tasa_vif),log(Efectos$Efecto.Columna))


# numero de vecinos
prop.table(table(apply(W>0,1,sum)))*100
cumsum(prop.table(table(apply(W>0,1,sum)))*100)
barplot(table(apply(W>0,2,sum)))


#Se exporta resultados de Efectos
#****write.csv(Efectos, "efectos.csv")


ggplot()+
  geom_sf(data=proof,aes(fill=`Efecto Columna`))+
  scale_fill_distiller(palette =3,direction = 1)+
  #scale_fill_viridis_c(option="viridis",direction=1)+
  geom_text(data=centroides,aes(x=X,y=Y,label=AcronimRM),size=2)+
  labs(title="Efecto Columna (hacia cantones externos)", subtitle = "",
       fill ="")+
  theme_gray()

ggplot()+
  geom_sf(data=proof,aes(fill=`Efectos Fila`))+
  scale_fill_distiller(palette =3,direction = 1)+
  #scale_fill_viridis_c(option="viridis",direction=1)+
  geom_text(data=centroides,aes(x=X,y=Y,label=AcronimRM),size=2)+
  labs(title="Efecto Fila (desde cantones externos)", subtitle = "",
       fill ="")+
  theme_gray()

summary(proof["Efecto Columna"])
summary(proof["Efectos Fila"])

aux <- data.frame(proof[order(proof$`Efecto Columna`,decreasing = TRUE),c("tasa_vif","AcronimRM","Efecto Columna")])
aux$geometry <- NULL
head(aux,10)

aux <- data.frame(proof[order(proof$`Efectos Fila`,decreasing = TRUE),c("tasa_vif","AcronimRM","Efectos Fila")])
aux$geometry <- NULL
head(aux,10)

















# ********************** x3: densidad *********************
#Función para determinar efectos marginales de los modelos

W <- listw2mat(W_objetoRM)                  # Matriz de distancias por contiguidad en formato matriz
colnames(W) <- rownames(W) <- AcronimRM             # Se asignan nombres de filas a columna


#Efectos en el modelo Lag
Lag.effect <- margin.effect(slm, W, 3, 20, Durbin="False")   #Se obtienen los efectos marginales del modelo lag                
# Sden <- Lag.effect$S
# Lag.effect <- margin.effect(slm, W, 2, 20, Durbin="False")   #Se obtienen los efectos marginales del modelo lag                
# Seduc <- Lag.effect$S
#Para comprobar comparamos los efectos directos, indirectos y totales con la funcion impacts

Lag.effect$efectos                     #Estimación con la funcion creada
impacts(slm, listw = W_objetoRM)    #Estimación con la función impats

#Efectos Columna#EfecW_estandRMtos Columna
Lag.effect.col <- Lag.effect$efectos.margin.col
columna <- data.frame(Lag.effect.col)
columna$Codigo <- rownames(Lag.effect.col)

#Efectos Fila
Lag.effect.row <- Lag.effect$efectos.margin.row
fila <- data.frame(Lag.effect.row)
fila$Codigo <- rownames(Lag.effect.row)

proof <- sp::merge(base2rm, columna, by.x = "AcronimRM", by.y = "Codigo", all.x = T)  #Se unifica en una sola base con datos de geo referenciación
colnames(proof)[34] <- "Efecto Columna"
proof <- sp::merge(proof, fila, by.x = "AcronimRM", by.y = "Codigo", all.x = T)
colnames(proof)[35] <- "Efectos Fila"

Efectos <- proof[, c(16,34:35)] #Se mantienen variables de efectos, tamano poblacional y economico
Efectos <- data.frame(Efectos)


cor.test(log(Efectos$tasa_vif),log(Efectos$Efecto.Columna),method = "pearson")
plot(log(Efectos$tasa_vif),log(Efectos$Efecto.Columna))


# numero de vecinos
prop.table(table(apply(W>0,1,sum)))*100
cumsum(prop.table(table(apply(W>0,1,sum)))*100)
barplot(table(apply(W>0,2,sum)))


#Se exporta resultados de Efectos
#****write.csv(Efectos, "efectos.csv")


ggplot()+
  geom_sf(data=proof,aes(fill=`Efecto Columna`))+
  scale_fill_distiller(palette =3,direction = 1)+
  #scale_fill_viridis_c(option="viridis",direction=1)+
  geom_text(data=centroides,aes(x=X,y=Y,label=AcronimRM),size=2)+
  labs(title="Efecto Columna (hacia cantones externos)", subtitle = "",
       fill ="")+
  theme_gray()

ggplot()+
  geom_sf(data=proof,aes(fill=`Efectos Fila`))+
  scale_fill_distiller(palette =3,direction = 1)+
  #scale_fill_viridis_c(option="viridis",direction=1)+
  geom_text(data=centroides,aes(x=X,y=Y,label=AcronimRM),size=2)+
  labs(title="Efecto Fila (desde cantones externos)", subtitle = "",
       fill ="")+
  theme_gray()

summary(proof["Efecto Columna"])
summary(proof["Efectos Fila"])



aux <- data.frame(proof[order(proof$`Efecto Columna`,decreasing = TRUE),c("tasa_vif","AcronimRM","Efecto Columna")])
aux$geometry <- NULL
head(aux,10)

aux <- data.frame(proof[order(proof$`Efectos Fila`,decreasing = TRUE),c("tasa_vif","AcronimRM","Efectos Fila")])
aux$geometry <- NULL
head(aux,10)

# ********************** END: efectos fila y columna. *********************