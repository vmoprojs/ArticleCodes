#________________________________________________________________#
# Project: EAD (Exposure at default - TDC)
# Enterprise: Banco Solidario
# Author: Carlos Cambino Contreras, Victor Morales-Onate
# Personal site: https://sites.google.com/site/moralesonatevictor/
# Detail: TFM
# Last dat modified: 12/07/2021
# Area: Data Analitics
#________________________________________________________________#

memp <- function(x, order) mean(x^order)

mae <- function(y,yhat)
{
  return(mean(abs(y-yhat),na.rm = TRUE))
}

rmse <- function(y,yhat)
{
  return(sqrt(mean((y-yhat)^2,na.rm = TRUE)))
}


ajuste <- function(x,grafico=FALSE,discreta = FALSE,criterio="Chi",nsim = 1000,
                   empirica = TRUE,inf = NULL, sup = NULL)
{
  # x = as.numeric(na.omit(dd$E_td))
  # x = xx
  if(criterio=="AD" & discreta == TRUE) 
  {
    warning("Con distribuciones discretas no se puede evaluar los criterios KS ni AD")
    return(-1)
  }
  if(criterio=="KS" & discreta == TRUE) 
  {
    warning("Con distribuciones discretas no se puede evaluar los criterios KS ni AD")
    return(-1)
  }
  
  #Choose a distribution
  # @RISK (monte carlo apriori): beta general, binomial, cumul, discrete, expon, gamma, general, histogram,
  # lognorm, normal, pert, poisson, triang, trigen, uniform, vary, weibull
  
  # @RISK (Dist Fitting): Logistic, lognorm, Normal, InvGauss,Webull,ExtValue,Triag,
  #Uniform, pareto2, Expon, Erf, Pareto, Student, Beta general, chisq,Erlang,
  # gamma, person5, pearson6, Rayleigh
  
  #@RISK (rank fits) Chisq statistic, Andreson-darling stat, Komogorov
  
  # models <- c("beta", "cauchy", "chi-squared", "exponential", "f", "gamma", "geometric", "log-normal", "lognormal", "logistic", "negative binomial", "normal", "Poisson", "t", "weibull")
  models <- c("gamma", "pois", "nbinom", "weibull", "pareto", "unif", "triang","beta",
              "t","cauchy","chisq","exp","f","lnorm","logis","norm","invgauss",
              "genpareto","llogis","trgamma")  
  # gamma, poisson, negative binomial, weibull,pareto,uniforme,triangular
  rango <- range(x)
  # ******************************  [1]Gamma:
  if(all(x>0) & discreta == FALSE)
  {
    # sol.gamma <- fitdist(x,distr = models[1])
    # sol.gamma <- fitdist(x,distr = models[1],method = "mme")
    
    cat("Fitting---: " ,models[1],"\n")
    
    t <- try(sol.gamma <- fitdist(x,distr = models[1]),silent = TRUE)
    if("try-error" %in% class(t)) sol.gamma <- fitdist(x,distr = models[1],method = "mme")
    
  }else
  {
    sol.gamma <- 0
  }
  # ******************************  [2]Poisson:
  if(discreta==TRUE)
  {
    cat("Fitting---: " ,models[2],"\n")
    sol.pois <- fitdist(x,distr = models[2])
  }else
  {
    sol.pois <- 0
  }
  # ******************************  [3]negative binomial
  if(discreta==TRUE)
  {
    cat("Fitting---: " ,models[3],"\n")
    sol.nbinom <- fitdist(x,distr = models[3])
  }else
  {
    sol.nbinom <- 0
  }
  # ******************************  [4]Weibull:
  if(all(x>=0) & discreta == FALSE)
  {
    cat("Fitting---: " ,models[4],"\n")
    sol.wei <- fitdist(x,distr = models[4],lower = c(0,0),start = list(shape=1, scale=1000),method = "mle")
  }else
  {
    sol.wei <- 0
  }
  
  # ******************************  [5]Pareto:
  xm <- min(x,na.rm = TRUE)
  
  if(all(x>=xm) & discreta == FALSE)
  {
    cat("Fitting---: " ,models[5],"\n")
    sol.pareto <- fitdist(x, models[5], method="mme", order=c(1, 2), memp="memp", 
                          start=list(shape=10, scale=1000), lower=1, upper=Inf)
  }else
  {
    sol.pareto <- 0
  }
  
  # ******************************  [6]Uniform:
  if(discreta == FALSE){
    cat("Fitting---: " ,models[6],"\n")
    sol.unif <- fitdist(x,distr = models[6])
  }else
  {
    sol.unif <- 0
  }
  
  
  # ******************************  [7]Triangular:
  if(discreta == FALSE){
    cat("Fitting---: " ,models[7],"\n")
    sol.triang <- fitdist(x, models[7], method="mge", start = list(min=min(x), mode=mean(x),max=max(x)), gof="CvM")
    # sol.triang <- fitdist(x, models[7], method="mge", start = list(min=mean(x), mode=mean(x),max=mean(x)), gof="KS")
  }else
  {
    sol.triang <- 0
  }
  
  # ******************************  [8]Beta:
  if(rango[2]<=1 & rango[1]>=0 & discreta==FALSE)
  {
    cat("Fitting---: " ,models[8],"\n")
    sol.beta <- fitdist(x,distr = models[8])
  }else
  {
    sol.beta <- 0
  }
  
  # ******************************  [9]t:
  if(discreta == FALSE){
    cat("Fitting---: " ,models[9],"\n")
    dfval <- as.numeric(quantile(x,probs = c(.999)))
    if(dfval<1) dfval <- 1
    sol.tdist <- fitdist(x,distr = models[9],start = list(df = dfval))
    # sol.tdist <- fitdist(x,distr = models[9],start = list(df = dfval),method = "mme")
  }else
  {
    sol.tdist <- 0
  }
  
  # ******************************  [10]Cauchy:
  if(discreta == FALSE){
    cat("Fitting---: " ,models[10],"\n")
    sol.cauchy <- fitdist(x,distr = models[10])
  }else
  {
    sol.cauchy <- 0
  }
  
  # ******************************  [11]Chi-Square:
  if(all(x>=0) & discreta == FALSE)
  {
    cat("Fitting---: " ,models[11],"\n")
    dfval <- round(as.numeric(quantile(x,probs = c(.9))))
    t <- try(sol.chisq <- fitdist(x,distr = models[11],start = list(df = dfval)))
    if("try-error" %in% class(t)) sol.chisq <-0
    
  }else{
    sol.chisq <- 0
  }
  
  
  # ******************************  [12]Exponential:
  if(all(x>=0) & discreta == FALSE){
    # sol.exp <- fitdist(x,distr = models[12])
    cat("Fitting---: " ,models[12],"\n")
    t <- try(sol.exp <- fitdist(x,distr = models[12]),silent = TRUE)
    if("try-error" %in% class(t)) sol.exp <- fitdist(x,distr = models[12],method = "mme")
  }else{
    sol.exp <- 0
  }
  
  
  # ******************************  [13]f:
  if(all(x>=0) & discreta == FALSE){
    cat("Fitting---: " ,models[13],"\n")
    dfval <- as.numeric(quantile(x,probs = c(.9)))
    sol.f <- try(fitdist(x,distr = models[13],start = list(df1 = dfval,df2 = dfval)))
    if("try-error" %in% class(sol.f)) sol.f <- 0#fitdist(x,distr = models[12],method = "mme")
  }else
  {
    sol.f <- 0
  }
  
  
  # ******************************  [14]Log-normal:
  if(all(x>0) & discreta == FALSE){
    cat("Fitting---: " ,models[14],"\n")
    sol.lnorm <- fitdist(x,distr = models[14])
  }else
  {
    sol.lnorm <- 0
  }
  
  # ******************************  [15]Logistic:
  if(discreta == FALSE){
    cat("Fitting---: " ,models[15],"\n")
    sol.logis <- fitdist(x,distr = models[15])
  }else
  {
    sol.logis <- 0
  }
  
  # ******************************  [16]Normal:
  if(discreta == FALSE){
    cat("Fitting---: " ,models[16],"\n")
    sol.norm <- fitdist(x,distr = models[16])
  }else
  {
    sol.norm <- 0
  }
  
  # ******************************  [17]Inverse Gaussian:
  
  # library(goft)
  # ss <- ig_fit(x)
  
  if(all(x>0) & discreta == FALSE){
    
    # hist(rinvgauss(1000,mean = 1003, shape =0.001))
    
    cat("Fitting---: " ,models[17],"\n")
    # media = as.numeric(quantile(x,probs = c(.9)))
    media = mean(x)
    t1 <- try(sol.invgauss <- fitdist(x,distr = models[17],start = list(mean =mean(x), shape = 1),lower = c(0,0)),silent = TRUE)
    if("try-error" %in% class(t1))
    {
      sol.invgauss <- 0
    }
  }else
  {
    sol.invgauss <- 0
  }
  
  
  # ******************************  [18]Generalize Pareto:
  if(all(x>=xm) & discreta == FALSE & all(x>0)){
    cat("Fitting---: " ,models[18],"\n")
    sol.genpareto <- fitdist(x,distr = models[18],start = list(shape1 = min(x), shape2 = max(x)))
  }else
  {
    sol.genpareto <- 0
  }
  
  # ******************************  [19]LogLogistic:
  if(all(x>=0) & discreta == FALSE){
    cat("Fitting---: " ,models[19],"\n")
    shape = as.numeric(quantile(x,probs = c(.9)))
    t1 <- try(sol.llogis <- fitdist(x,distr = models[19],start = list(shape = shape)))
    
    if("try-error" %in% class(t1))
    {
      sol.llogis <- 0
    }
    
  }else
  {
    sol.llogis <- 0
  }
  
  # ******************************  [20]R Gamma:
  if(all(x>=0) & discreta == FALSE){
    cat("Fitting---: " ,models[20],"\n")
    t1 <- try(sol.trgamma <- fitdist(x,distr = models[20],start = list(shape1 = mean(x),shape2 = mean(x)+1000),lower = c(0,0)))
    if("try-error" %in% class(t1))
    {
      sol.trgamma <- 0
    }
  }else
  {
    sol.trgamma <- 0
  }
  
  
  # ********************* Recovergin Solutions ********************* #
  list.mod <- list(sol.gamma,sol.pois,sol.nbinom,sol.wei,sol.pareto,sol.unif,sol.triang,
                   sol.beta, sol.tdist,sol.cauchy,sol.chisq,sol.exp,sol.f,sol.lnorm,
                   sol.logis,sol.norm,sol.invgauss,sol.genpareto,sol.llogis,sol.trgamma)
  pos <- sapply(list.mod,test)
  res <- gofstat(list.mod[pos*1:length(models)])
  # str(res)
  # res$chisqpvalue
  # res$adtest
  # res$kstest
  # res$aic
  if(criterio=="Chi")
  {
    sol <- as.numeric(res$chisqpvalue)
    names(sol) <- models[pos*1:length(models)]
    sol <- sol[order(sol,decreasing = TRUE)]
    index <- order(sol,decreasing = TRUE)
  }
  if(criterio=="KS"& discreta == FALSE)
  {
    sol <- as.numeric(res$ks)
    names(sol) <- models[pos*1:length(models)]
    sol <- sol[order(sol,decreasing = TRUE)]
    index <- order(sol,decreasing = TRUE)
  }
  if(criterio=="AD"& discreta == FALSE)
  {
    sol <- as.numeric(res$ad)
    names(sol) <- models[pos*1:length(models)]
    sol <- sol[order(sol,decreasing = TRUE)]
    index <- order(sol,decreasing = TRUE)
  }
  
  
  if(grafico==TRUE)
  {
    # list.mod[pos*1:length(models)][order(sol,decreasing = TRUE)][[1]]$estimate
    # names(sol)
    AA = names(sol)[1]
    if(AA =="gamma")
    {#[1] Gamma
      
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rgamma(nsim,shape = sol.gamma$estimate[1],rate = sol.gamma$estimate[2]))
        # Histogram overlaid with kernel density curve
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  # Overlay with transparent density plot
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dgamma(x,shape = sol.gamma$estimate[1],rate = sol.gamma$estimate[2]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
      }
      
    }
    if(AA == "pois")
    {#[2] Poison
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rpois(nsim,sol.pois$estimate))
        
        pp = ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),  
                         binwidth=.5,
                         colour="black", fill="white") 
        print(pp)
      }
      if(empirica==FALSE)
      {
        # curve(dpois(x,sol.pois$estimate),xlim = rango,ylab = "")
        plot(x,dpois(x,sol.pois$estimate),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
        
      }
      
    }
    if(AA == "nbinom")
    {#[3] Negative Binomial
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rnbinom(nsim,size = sol.nbinom$estimate[1],mu = sol.nbinom$estimate[2]))
        
        pp = ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),  
                         binwidth=.5,
                         colour="black", fill="white") 
        print(pp)
      }
      if(empirica==FALSE)
      {
        # curve(dnbinom(x,size = sol.nbinom$estimate[1],mu = sol.nbinom$estimate[2]),xlim = rango,ylab = "")
        plot(x,dnbinom(x,size = sol.nbinom$estimate[1],mu = sol.nbinom$estimate[2]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
        
      }
      
    }
    if(AA =="weibull")
    {#[4] weibull
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rweibull(nsim,shape = sol.wei$estimate[1],scale = sol.wei$estimate[2]))
        # Histogram overlaid with kernel density curve
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  # Overlay with transparent density plot
        print(pp)
      }
      
      
      if(empirica==FALSE)
      {
        curve(dweibull(x,shape = sol.wei$estimate[1],scale = sol.wei$estimate[2]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
        
      }
    }
    if(AA =="pareto")
    {#[5] pareto
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rpareto(nsim,shape = sol.pareto$estimate[1],scale =sol.pareto$estimate[2] ))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp) 
      }
      if(empirica==FALSE)
      {
        curve(dpareto(x,shape = sol.pareto$estimate[1],scale =sol.pareto$estimate[2] ),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
      }
      
    }
    if(AA =="unif")
    {#[6] Uniforme
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = runif(nsim,min = sol.unif$estimate[1],max = sol.unif$estimate[2]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dunif(x,min = sol.unif$estimate[1],max = sol.unif$estimate[2]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
      }
      
    }
    if(AA =="triang")
    {#[7] Triang
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rtriang(nsim,min = sol.triang$estimate[1],
                                         mode = sol.triang$estimate[2],
                                         max = sol.triang$estimate[3]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dtriang(x,min = sol.triang$estimate[1],
                      mode = sol.triang$estimate[2],
                      max = sol.triang$estimate[3]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
        
      }
      
    }
    if(AA =="beta")
    {#[8] Beta
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rbeta(nsim,shape1 = sol.beta$estimate[1],
                                       shape2 = sol.beta$estimate[2]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dbeta(x,shape1 = sol.beta$estimate[1],
                    shape2 = sol.beta$estimate[2]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
      }
      
    }
    if(AA =="t")
    {#[9] t
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rt(nsim,df = sol.tdist$estimate[1]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dt(x,df = sol.tdist$estimate[1]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
      }
    }
    if(AA =="cauchy")
    {#[10] cauchy
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rcauchy(nsim,location = sol.cauchy$estimate[1],
                                         scale = sol.cauchy$estimate[2]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dcauchy(x,location = sol.cauchy$estimate[1],
                      scale = sol.cauchy$estimate[2]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
      }
      
    }
    if(AA =="chisq")
    {#[11] chisq
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rchisq(nsim,df = sol.chisq$estimate[1]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dchisq(x,df = sol.chisq$estimate[1]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
      }
      
    }
    if(AA =="exp")
    {#[12] exp
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rexp(nsim,rate = sol.exp$estimate[1]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dexp(x,rate = sol.exp$estimate[1]),xlim = rango,ylab="")
        hist(x,freq = FALSE,main = "",add =TRUE)
      }
    }
    if(AA =="f")
    {#[13] f
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rf(nsim,df1 = sol.f$estimate[1],
                                    df2 = sol.f$estimate[2]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(df(x,df1 = sol.f$estimate[1],
                 df2 = sol.f$estimate[2]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
        
      }
    }
    if(AA =="lnorm")
    {#[14] lnorm
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rlnorm(nsim,meanlog = sol.lnorm$estimate[1],
                                        sdlog = sol.f$estimate[2]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dlnorm(x,meanlog = sol.lnorm$estimate[1],
                     sdlog = sol.f$estimate[2]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add = TRUE)
        
      }
    }
    if(AA =="logis")
    {#[15] logis
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rlogis(nsim,location = sol.logis$estimate[1],
                                        scale = sol.logis$estimate[2]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dlogis(x,location = sol.logis$estimate[1],
                     scale = sol.logis$estimate[2]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
      }
    }
    if(AA =="norm")
    {#[16] norm
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rnorm(nsim,mean = sol.norm$estimate[1],
                                       sd = sol.norm$estimate[2]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dnorm(x,mean = sol.norm$estimate[1],
                    sd = sol.norm$estimate[2]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
      }
    }
    if(AA =="invgauss")
    {#[17] invgauss
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rinvgauss(nsim,mean = sol.invgauss$estimate[1],
                                           shape = sol.f$estimate[2]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dinvgauss(x,mean = sol.invgauss$estimate[1],
                        shape = sol.f$estimate[2]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
        
      }
      
    }
    if(AA =="genpareto")
    {#[18] genpareto
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rgenpareto(nsim,shape1 = sol.genpareto$estimate[1],
                                            shape2 = sol.genpareto$estimate[2]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dgenpareto(x,shape1 = sol.genpareto$estimate[1],
                         shape2 = sol.genpareto$estimate[2]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add =TRUE)
      }
      
    }
    if(AA =="llogis")
    {#[19] llogis
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rllogis(nsim,shape = sol.llogis$estimate[1]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        curve(dllogis(x,shape = sol.llogis$estimate[1]),xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add = TRUE)
        
      }
      
    }
    if(AA =="trgamma")
    {#[20] trgamma
      if(empirica==TRUE)
      {
        dat <- data.frame(vals = rtrgamma(nsim,shape1 = sol.trgamma$estimate[1],shape2 = sol.trgamma$estimate[2]))
        
        pp =  ggplot(dat, aes(x=vals)) + 
          geom_histogram(aes(y=..density..),      
                         binwidth=.5,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#8cd4ff")  
        print(pp)
      }
      if(empirica==FALSE)
      {
        
        curve(dtrgamma(x,shape1 = sol.trgamma$estimate[1],shape2 = sol.trgamma$estimate[2])
              ,xlim = rango,ylab = "")
        hist(x,freq = FALSE,main = "",add = TRUE)
      }
    }
  }
  
  
  return(round(sol,5))
}
