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

theta <- start

dd <- GeoSim(coordx=coords,coordt=times,
             corrmodel="Wen_time", param=as.list(param),
             model = "Gaussian",sparse = TRUE)$data

res=STBEUFit(theta =start,fixed = unlist(fix),
             coords = coords,times=times,cc=3,datos=dd,
             type_dist=type_dist,
             maxdist=maxdist1 ,maxtime=maxtime1,
             winc_s=winc,winstp_s=winstp,
             winc_t=NULL,
             winstp_t=NULL,subs=type_subs,weighted=weighted)


coordx=coords[,1];coordy=coords[,2];ncoords=nrow(coords);ntime=length(times)
npar=length(theta)

subs <- type_subs
if(subs==1) type_sub="SubSamp_space"
if(subs==2) type_sub="SubSamp_time"
if(subs==3) type_sub="SubSamp_spacetime"

sol <- eucl_st_ocl(theta=unlist(theta),fixed = unlist(fix),coordx = coordx,coordy = coordy,
                   ncoords = ncoords,times = times, ntime = ntime,
                   cc = 3, datos = datos, type_dist = type_dist,
                   maxdist=maxdist1, maxtime=maxtime1, winc_s=winc, winstp_s=winstp, 
                   winc_t=NULL, winstp_t=NULL, weighted=weighted, 
                   type_sub=type_sub, local=c(1,1), GPU=NULL, kernel= "DouExp.cl")


sol
res$par

#********************** SCALE S **********************#
t_aux <- theta
vals <- seq(0.01,1,0.01)

sol_scale_s <- NULL

for(i in 1:length(vals))
{
  t_aux$scale_s <- vals[i]
  
  sol_aux <- eucl_st_ocl(theta=unlist(t_aux),fixed = unlist(fix),coordx = coordx,coordy = coordy,
                         ncoords = ncoords,times = times, ntime = ntime,
                         cc = 3, datos = datos, type_dist = type_dist,
                         maxdist=maxdist1, maxtime=maxtime1, winc_s=winc, winstp_s=winstp, 
                         winc_t=NULL, winstp_t=NULL, weighted=weighted, 
                         type_sub=type_sub, local=c(1,1), GPU=NULL, kernel= "DouExp.cl")
  sol_scale_s <- c(sol_scale_s,sol_aux)
}

plot(vals,sol_scale_s,t = "l", main = "Objetive function for scale s")
abline(v = c(res$par["scale_s"],scale_s), col =c("red","blue"))
legend("bottomright",c("Estimated","Theoretical"), col = c("red","blue"),lty = 1)


#********************** SCALE t **********************#
t_aux <- theta
vals <- seq(1.5,10,0.1)

sol_scale_t <- NULL

for(i in 1:length(vals))
{
  t_aux$scale_t <- vals[i]
  
  sol_aux <- eucl_st_ocl(theta=unlist(t_aux),fixed = unlist(fix),coordx = coordx,coordy = coordy,
                         ncoords = ncoords,times = times, ntime = ntime,
                         cc = 3, datos = datos, type_dist = type_dist,
                         maxdist=maxdist1, maxtime=maxtime1, winc_s=winc, winstp_s=winstp, 
                         winc_t=NULL, winstp_t=NULL, weighted=weighted, 
                         type_sub=type_sub, local=c(1,1), GPU=NULL, kernel= "DouExp.cl")
  sol_scale_t <- c(sol_scale_t,sol_aux)
}

plot(vals,sol_scale_t,t = "l", main = "Objetive function for scale t")
abline(v = c(res$par["scale_t"],scale_t), col =c("red","blue"))
legend("bottomright",c("Estimated","Theoretical"), col = c("red","blue"),lty = 1)




#********************** sep **********************#
t_aux <- theta
vals <- seq(0.1,0.9,0.01)

sol_sep <- NULL

for(i in 1:length(vals))
{
  t_aux$sep <- vals[i]
  
  sol_aux <- eucl_st_ocl(theta=unlist(t_aux),fixed = unlist(fix),coordx = coordx,coordy = coordy,
                         ncoords = ncoords,times = times, ntime = ntime,
                         cc = 3, datos = datos, type_dist = type_dist,
                         maxdist=maxdist1, maxtime=maxtime1, winc_s=winc, winstp_s=winstp, 
                         winc_t=NULL, winstp_t=NULL, weighted=weighted, 
                         type_sub=type_sub, local=c(1,1), GPU=NULL, kernel= "DouExp.cl")
  sol_sep <- c(sol_sep,sol_aux)
}

plot(vals,sol_sep,t = "l", main = "Objetive function for sep")
abline(v = c(res$par["sep"],sep), col =c("red","blue"))
legend("bottomright",c("Estimated","Theoretical"), col = c("red","blue"),lty = 1)



#********************** sill **********************#
t_aux <- theta
vals <- seq(0.01,2,0.01)

sol_sill <- NULL

for(i in 1:length(vals))
{
  t_aux$sill <- vals[i]
  
  sol_aux <- eucl_st_ocl(theta=unlist(t_aux),fixed = unlist(fix),coordx = coordx,coordy = coordy,
                         ncoords = ncoords,times = times, ntime = ntime,
                         cc = 3, datos = datos, type_dist = type_dist,
                         maxdist=maxdist1, maxtime=maxtime1, winc_s=winc, winstp_s=winstp, 
                         winc_t=NULL, winstp_t=NULL, weighted=weighted, 
                         type_sub=type_sub, local=c(1,1), GPU=NULL, kernel= "DouExp.cl")
  sol_sill <- c(sol_sill,sol_aux)
}

plot(vals,sol_sill,t = "l", main = "Objective function for sill")
abline(v = c(res$par["sill"],sill), col =c("red","blue"))
legend("bottomright",c("Estimated","Theoretical"), col = c("red","blue"),lty = 1)
