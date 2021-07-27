rm(list = ls())
graphics.off()
cat("\014")
library(GeoModels)
library(STBEU)
library(scatterplot3d)
# devtools::install_github("andrewzm/STRbook")
data("Medwind_data",package = "STRbook")

coords = Edat$ECMWFxylocs

datos = matrix(unlist(Edat$EUdat),ncol = ncol(Edat$EUdat),nrow = nrow(Edat$EUdat))
datos = t(datos)


coords_ll = coords #lon-lat coords

prj = mapproj::mapproject(coords[ ,1],coords[ ,2],projection ="sinusoidal")
coords = cbind(prj$x,prj$y) # Projected coords
coords = coords*6371

time <- 1:nrow(datos)

#*********************************** sep = 0.5 *****************************#

#### **************************    Estimation

################################################
#   parameters for the subsampling ####
###############################################
coordx=coords[,1]
coordy=coords[,2]
LX=abs(range(coordx)[1]-range(coordx)[2])
LY=abs(range(coordy)[1]-range(coordy)[2])
lato_fin=400  #changing window size
lx=lato_fin          #lunghezza lato x quadrato subfinestra
ly=lato_fin          #lunghezza lato y quadrato subfinestra
winc=c(lx/sqrt(LX),ly/sqrt(LY))
winstp= 1###   1/lato_fin complete overlapping in space  1 "no" overlapping in space
###############################################
winc_t=6   ###  length of temporal window
winstp_t=1 ###   0.5 half overlapping  1 "no" overlapping

(maxdist <-  40)
(maxtime <- 1*winc_t)
###############################################
weighted=0
type_dist=1 ### type of distance 1:euclidean
type_subs=1 ### type of subsampling  1=in space    2= in time


smooth_t=0
scale_t=20
scale_s=350
power2_t=3.5+smooth_t  +1
power_s=2
power2_s=2.5+2*smooth_t
sep=0.5

sill=var(c(datos),na.rm = TRUE)
nugget=0.01
mean=mean(datos,na.rm=TRUE)

start=list(scale_s=scale_s,scale_t=scale_t,sill=sill,nugget=nugget)
fix=c( power_s=power_s,mean = mean,
       power2_s=power2_s,
       power2_t=power2_t,smooth_t=smooth_t,
       sep=sep)
param <- c(start,fix)

summary(dist(coords))
max(dist(coords))/maxdist
max(dist(time))/maxtime
summary(dist(time))
cc = 3


#2 : STBEU in OpenCL framework with CPU
res1_0=STBEUFit(start,fix,coords,time,cc,datos,
                type_dist,maxdist ,maxtime,
                winc,winstp,NULL,NULL,type_subs,weighted
                ,varest = TRUE
)


###### pairwise likelihood ########
fixed=as.list(fix)
res2_0=GeoFit(data=datos,coordx=coords,coordt=time,
              corrmodel="Wen_time",
              start=start,fixed=fixed,
              maxdist=maxdist ,maxtime=maxtime,varest = TRUE)


res1_0$par
res2_0$par