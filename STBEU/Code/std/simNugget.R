rm(list = ls())
graphics.off()
library(GeoModels)
library(STBEU)

#### Simluation:

SolPar <- NULL

# semilla = 1537
# set.seed(semilla)
i = 1
nsim = 1000
while(i <=nsim){
  # if(i ==7) i = i+1
  
  # ST: Creating grid & Data:
  tt = 10
  N = 20
  
  times <- 1:tt
  x <- seq(0,1,length.out = N)
  y <- x
  coords <- expand.grid(x,y)
  (NT = length(times)*nrow(coords))
  
  
  
  smooth_t=0 # k or kappa
  scale_t=time_comp_supp=4  # compact supportt scale_t
  scale_s=.056
  power2_t=3.5+smooth_t #nu
  power_s=2
  power2_s=2.5+2*smooth_t# tau
  
  sep=1  ## 0 0.5  1
  sill=1
  mean=0
  nugget=0.1
  
  
  
  param = c(scale_s = scale_s,scale_t=scale_t,sill = sill,
            nugget = nugget, power_s=power_s, mean =mean,
            power2_s = power2_s,
            power2_t =power2_t,  smooth_t  =smooth_t, sep =sep)
  
  # END: Creating grid & Data
  
  
  # ST: Starting values & estimation:
  start <- NULL
  start$scale_s <- as.numeric(param[1])
  start$scale_t <-  as.numeric(param[2])
  start$sill <-  as.numeric(param[3])
  # start$mean <-  as.numeric(param[6])
  start$nugget <-  as.numeric(param[4])
  
  fix <- as.list(param[c(5,6,7,8,9,10)])
  
  l <- .2
  winc=l
  winstp=1
  
  (maxdist1=(1*winc))
  
  maxdist1=0.1
  maxtime1=3
  
  
  
  type_subs=1
  type_dist=1
  weighted=0
  
  set.seed(i)
  dd <- GeoSim(coordx=coords,coordt=times,
               corrmodel="Wen_time", param=as.list(param),
               model = "Gaussian",sparse = TRUE)$data
  
  
  tryCatch({aux=STBEUFit(theta =start,fixed = unlist(fix),
                         coords = coords,times=times,cc=3,datos=dd,
                         type_dist=type_dist,
                         maxdist=maxdist1 ,maxtime=maxtime1,winc_s=winc,winstp_s=winstp,
                         winc_t=NULL,
                         winstp_t=NULL,subs=type_subs,weighted=weighted)},
           error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
  
  SolPar <- rbind(SolPar,c(aux$par,iter = i))
  solDF <- data.frame(SolPar)
  save(solDF, file = "solDF_st4.RData")
  cat("Iter: ",i,"de: ",nsim,"\n")
  i = i+1
  gc()
}
solSTBEU <- SolPar
apply(solSTBEU,2,mean)
par(mfrow = c(2,2))
boxplot(solSTBEU[,1], main = "scale_s");abline(h = scale_s, col = "blue")
boxplot(solSTBEU[,2], main = "scale_t");abline(h = scale_t, col = "blue")
boxplot(solSTBEU[,3], main = "sill");abline(h = sill, col = "blue")
boxplot(solSTBEU[,4], main = "nugget");abline(h = nugget, col = "blue")
par(mfrow = c(1,1))

