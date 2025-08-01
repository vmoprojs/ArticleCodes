# ST: Creating grid & Data:
rm(list = ls())
graphics.off()
library(GeoModels)
library(STBEU)

#################################################################
type_dist=1 ### type of distance 1:euclidean
type_subs=1 ### type of subsampling  1=in space    2= in time

scale_t=0.6
scale_s=0.6

sill=1
nugget=0
mean=0.05

start=list(scale_s=scale_s,scale_t=scale_t,sill=sill)
fix=c( nugget=nugget,mean = mean)

####location sites ########################################
lambda=6
xx=seq(-lambda,lambda);
coords=as.matrix(expand.grid(xx,xx))   ###regular

####temporal instants ########################################
nt = 4 
times=seq(1,nt,1)
(NT <- nrow(coords)*nt)

param=list(scale_s=scale_s,scale_t=scale_t,
           sill=sill,nugget=nugget,mean = mean)
maxdist1=max(dist(coords))*.25
maxtime1=ceiling(max(dist(times))*.25)

winc=c(0,0)  ###  length of temporal window
winstp=1 ###   0.5 half overlapping  1 "no" overlapping
cc = 1 #Exp_Exp

datos <- GeoSim(coordx=coords,coordt=times,
                corrmodel="Exp_Exp", param=param)$data

weighted = 0
# END: Creating grid & Data



# ST: Starting values & estimation:
res1=STBEUFit(start,fix,coords,times,cc,datos,
              type_dist,maxdist1 ,maxtime1,
              winc,winstp,0,0,type_subs,weighted,
              varest = TRUE)

NT = nrow(coords)*length(times)
qq <- qnorm(0.975)

# ******** SCALE S
c(res1$par[1] - qq*res1$stderr[1],res1$par[1] + qq*res1$stderr[1])
# ******** SCALE T
c(res1$par[2] - qq*res1$stderr[2],res1$par[2] + qq*res1$stderr[2])
# ******** SILL
c(res1$par[3] - qq*res1$stderr[3],res1$par[3] + qq*res1$stderr[3])
# END: Starting values & estimation:









#### ST: Simulation:

SolPar <- NULL
SolSD <- NULL

semilla = 1537 
set.seed(semilla)
i = 1
nsim = 1000
while(i <=nsim)
{
  dd <- GeoSim(coordx=coords,coordt=times,
               corrmodel="Exp_Exp", param=param)$data
  
  tryCatch({aux=STBEUFit(start,fix,coords,times,cc,dd,
                         type_dist,maxdist1 ,maxtime1,
                         winc,winstp,0,0,type_subs,weighted,
                         varest = TRUE)},
           error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  SolPar <- rbind(SolPar,aux$par)
  SolSD <- rbind(SolSD,aux$stderr)
  cat("Iter: ",i,"de: ",nsim,"\n")
  i = i+1
}
#### END Simulation:







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