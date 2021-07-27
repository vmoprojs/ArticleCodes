# Separability parameter
rm(list = ls())
graphics.off()
cat("\014")
library(GeoModels)
library(STBEU)

# ST: Creating grid & Data:
tt = 10
N = 20

times <- 1:tt
x <- seq(0,1,length.out = N)
y <- x
coords <- expand.grid(x,y)
(NT = length(times)*nrow(coords))



smooth_t=0 # k or kappa
scale_t=time_comp_supp=3  # compact supportt scale_t
scale_s=.053
power2_t=3.5+smooth_t #nu
power_s=2
power2_s=2.5+2*smooth_t# tau

sep=0.5  ## 0 0.5  1
sill=1
mean=0
nugget=0



param = c(scale_s = scale_s,scale_t=scale_t,sill = sill,
          nugget = nugget, power_s=power_s, mean =mean,
          power2_s = power2_s,
          power2_t =power2_t,  smooth_t  =smooth_t, sep =sep)



set.seed(2)
datos <- GeoSim(coordx=coords,coordt=times,sparse=TRUE,
                corrmodel="Wen_time", param=as.list(param))$data
# END: Creating grid & Data


# ST: Starting values & estimation:
start <- NULL
start$scale_s <- as.numeric(param[1])
start$scale_t <-  as.numeric(param[2])
start$sill <-  as.numeric(param[3])
# start$mean <-  as.numeric(param[6])
start$sep <-  as.numeric(param[10])

fix <- as.list(param[c(4,5,6,7,8,9)])

l <- .2
winc=l
winstp=1

(maxdist1=(1*winc))

maxdist1=0.06
maxtime1=3

type_subs=1
type_dist=1
weighted=0

#### Simluation:

SolPar <- NULL
SolSD <- NULL

semilla = 1537
set.seed(semilla)
i = 1
nsim = 1000
while(i <=nsim){
  dd <- GeoSim(coordx=coords,coordt=times,
               corrmodel="Wen_time", param=as.list(param),
               model = "Gaussian",sparse = TRUE)$data
  
  tryCatch({aux=STBEUFit(theta =start,fixed = unlist(fix),
               coords = coords,times=times,cc=3,datos=dd,
               type_dist=type_dist,
               maxdist=maxdist1 ,maxtime=maxtime1,
               winc_s=winc,winstp_s=winstp,
               winc_t=NULL,
               winstp_t=NULL,subs=type_subs,weighted=weighted,varest = TRUE)},
           error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  SolPar <- rbind(SolPar,aux$par)
  SolSD <- rbind(SolSD,aux$stderr)
  cat("Iter: ",i,"de: ",nsim,"\n")
  i = i+1
}
solSTBEU <- SolPar
apply(solSTBEU,2,mean)
par(mfrow = c(2,2))
boxplot(solSTBEU[,1], main = "scale_s");
abline(h = scale_s, col = "blue")
boxplot(solSTBEU[,2], main = "scale_t");
abline(h = scale_t, col = "blue")
boxplot(solSTBEU[,3], main = "sill");
abline(h = sill, col = "blue")
boxplot(solSTBEU[,4], main = "sep");
abline(h = sep, col = "blue")
par(mfrow = c(1,1))




mm <- SolPar 
se <- SolSD

qq <- qnorm(0.975)

# ******** SCALE S
lo.conf.scale_s <- mm[,1] - qq*se[,1]
up.conf.scale_s <- mm[,1] + qq*se[,1]

bl.scale_s <- sum(start$scale_s<lo.conf.scale_s) # bad lower
bu.scale_s <- sum(up.conf.scale_s<start$scale_s) # bad upper

1-(bl.scale_s+bu.scale_s)/nsim # should be close to 1



# ******** SCALE T
lo.conf.scale_t <- mm[,2] - qq*se[,2]
up.conf.scale_t <- mm[,2] + qq*se[,2]

bl.scale_t <- sum(start$scale_t<lo.conf.scale_t) # bad lower
bu.scale_t <- sum(up.conf.scale_t<start$scale_t) # bad upper
1-(bl.scale_t+bu.scale_t)/nsim # should be close to 1



# ******** SILL
lo.conf.sill <- mm[,3] - qq*se[,3]
up.conf.sill <- mm[,3] + qq*se[,3]

bl.sill <- sum(start$sill<lo.conf.sill) # bad lower
bu.sill <- sum(up.conf.sill<start$sill) # bad upper
1-(bl.sill+bu.sill)/nsim # should be close to 1




# ******** Sep
lo.conf.sep <- mm[,4] - qq*se[,4]
up.conf.sep <- mm[,4] + qq*se[,4]

bl.sep <- sum(start$sep<lo.conf.sep) # bad lower
bu.sep <- sum(up.conf.sep<start$sep) # bad upper
1-(bl.sep+bu.sep)/nsim # should be close to 1
