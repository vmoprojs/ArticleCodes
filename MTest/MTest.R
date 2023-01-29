#*********************** MTEST *****************+
# This code is for: MTest: a bootstrap test for multicollinearity
# Main functions are: MTest and pairwise.ks.test.
# Simulation procedures and application are detailed in the article.
# Victor Morales Onate: victor.morales@uv.cl



# ***** Data Generation
library(MASS)
rm(list = ls())
graphics.off()

# Sample size:
n <- 10000
# vector means
medias <- c(0,0,0) 
# Correlation structure:
rho_12 <- -.945 # -.94
rho_13 <- .3
rho_23 <- -.5
# Coefficients of the regression:
betas <- c(10,-5,3,9) 
s.d <- 3 # deviation of the residual



(Sigma <- matrix(c(1,rho_12,rho_13,rho_12,1,rho_23,rho_13,rho_23,1),3,3))
set.seed(247)
# Predictors simulation:
X <- mvrnorm(n = n, medias, Sigma)
M <- cbind(1,X)
# Output simulation
y <- M %*% betas + rnorm(n,0,s.d)

datos <- data.frame((y),(X))
names(datos) <- c("y","X1","X2","X3")

standar <- function(X)
{
  # X <- M
  mm <- colMeans(X)
  n <- dim(X)[1]
  vv <- apply(X,2,var)
  X_sol <- NULL
  for(i in 1:dim(X)[2])
    {
    # i = 1
    X_sol <- cbind(X_sol,(X[,i]-mm[i])/sqrt(n*vv[i])) 
  }
  return(X_sol)
}


Z <- standar(X) # X standardized
Z_s <- t(Z)%*%Z # Z squared
evals_Z <- eigen(Z_s)
svd_vals <- svd(Z)$d
diag(solve(Z_s))


cor(datos)

head(datos)
m1 <- lm(y~.,data = (datos))
coef(m1)
betas
m1Sum <- summary(m1)

library(car)
(vif.vals <- vif(m1))

(inferior <- 1/max(evals_Z$values)) #minimo
(superior <- 1/min(evals_Z$values)) #maximo

sqrt(max(evals_Z$values)/min(evals_Z$values))# K(X): CN
max(svd_vals)/min(svd_vals)# K(X): CN


vif.vals<superior
inferior<vif.vals

Raux <- (vif.vals-1)/vif.vals
Rglobal <- m1Sum$r.squared
1/(1-Raux)


############## 


pairwise.ks.test <- function(X,alternative="two.sided")
{
  #Returns the p value of the pairwise KS test of X columns
  n <- ncol(X)
  sol <- matrix(NA, ncol = n, nrow = n)
  for(i in 1:(n))
  {
    for(j in (1):n)
    {
      # print(c(i,j))
      a <- suppressWarnings(ks.test(X[,i],X[,j],alternative = alternative))
      # print(a$p.value)
      sol[i,j] <- a$p.value
    }
  }
  if(alternative=="less") print("alternative hypothesis: the CDF of x lies below that of y. Rows are `x` and Columns are `y`")
  if(alternative=="greater") print("alternative hypothesis: the CDF of x lies above that of y. Rows are `x` and Columns are `y`")
  if(alternative=="two.sided") print("alternative hypothesis: two-sided")
  colnames(sol) <- colnames(X)
  rownames(sol) <- colnames(X)
  return(sol)
}

Mtest <- function(datos, nboot = 500,
                  nsam = NULL,trace = TRUE,seed = NULL,
                  valor_vif = 0.9)
{
  if(is.null(nsam)){nsam = nrow(datos)*3}
  
  vals <- 1:nrow(datos)
  
  if(!is.null(seed)) {set.seed(seed)}
  
  sol.rsq <- NULL
  sol.vif <- NULL
  i = 1
  while(i <=nboot)
  {
    sam <- sample(vals,nsam,replace = TRUE)
    aux <- datos[sam,]
    maux <- lm(y~.,data = aux)
    sm  <- summary(maux)
    vif.vals <- vif(maux)
    Raux <- (vif.vals-1)/vif.vals
    
    s1 <- c(sm$r.squared,Raux)
    sol.rsq <- rbind(sol.rsq,s1)
    
    sol.vif <- rbind(sol.vif,vif.vals)
    
    if(trace)
    {
      cat("Iteration",i,"out of ",nboot,"\n")
    }
    i = i+1
  }
  
  pval_vif <- NULL
  for(j in 2:ncol(sol.rsq))
  {
    pval_vif <- c(pval_vif,sum(sol.rsq[,j]>valor_vif)/nboot)
  }
  names(pval_vif) <- colnames(sol.rsq)[2:ncol(sol.rsq)]
  pval_klein <- NULL
  for(z in 2:ncol(sol.rsq))
  {
    pval_klein <- c(pval_klein,sum(sol.rsq[,1]<sol.rsq[,z])/nboot)
  }
  names(pval_klein) <- colnames(sol.rsq)[2:ncol(sol.rsq)]
  
  colnames(sol.rsq) <- c("global",paste(names(datos)[-1],sep =""))
  rownames(sol.rsq) <- 1:nrow(sol.rsq)
  return(list(Bvals= sol.rsq,pval_vif = pval_vif,pval_klein=pval_klein))
}

# colMeans(Mtest(datos,trace=FALSE,seed = 1))

boot.sol <- Mtest(datos,trace=FALSE,seed = 1,nboot = 1000)
boot.sol$pval_vif
boot.sol$pval_klein

setwd("~/Documents/Articles/EPN/MTest/Figures")
pdf("fig1.pdf")
par(mfrow = c(2,2))
# aux <- reshape2::melt(boot.sol$Bvals[,2:ncol(boot.sol$Bvals)])
# library(ggplot2)
# p <- ggplot(aux, aes(x=Var2, y=value, color=Var2)) +
#   geom_boxplot()
# p
boxplot(boot.sol$Bvals[,1],ylab = expression(paste("y")))
boxplot(boot.sol$Bvals[,2],ylab = expression(paste("X")[1]))
boxplot(boot.sol$Bvals[,3],ylab = expression(paste("X")[2]))
boxplot(boot.sol$Bvals[,4],ylab = expression(paste("X")[3]))
par(mfrow = c(1,1))
dev.off()

summary(boot.sol$Bvals)

round(pairwise.ks.test(X = boot.sol$Bvals),5)
# round(pairwise.ks.test(X = boot.sol$Bvals, alternative = "less"),5)
round(pairwise.ks.test(X = boot.sol$Bvals, alternative = "greater"),5)


round(c(Rglobal,Raux),5)
Raux>Rglobal
round(vif.vals,5)


summary(boot.sol$Bvals[,4])
summary(boot.sol$Bvals[,2])

ks.test(boot.sol$Bvals[,4],boot.sol$Bvals[,2],
        alternative ="less")

wilcox.test(x = boot.sol$Bvals[,4],y = boot.sol$Bvals[,2],
            alternative ="greater")

wilcox.test(x = boot.sol$Bvals[,4],y = boot.sol$Bvals[,2])


















# ***** Simulation 2
library(MASS)
rm(list = ls())
graphics.off()
library(car)

############## 


pairwise.ks.test <- function(X,alternative="two.sided")
{
  #Returns the p value of the pairwise KS test of X columns
  n <- ncol(X)
  sol <- matrix(NA, ncol = n, nrow = n)
  for(i in 1:(n))
  {
    for(j in (1):n)
    {
      # print(c(i,j))
      a <- suppressWarnings(ks.test(X[,i],X[,j],alternative = alternative))
      # print(a$p.value)
      sol[i,j] <- a$p.value
    }
  }
  if(alternative=="less") print("alternative hypothesis: the CDF of x lies below that of y. Rows are `x` and Columns are `y`")
  if(alternative=="greater") print("alternative hypothesis: the CDF of x lies above that of y. Rows are `x` and Columns are `y`")
  if(alternative=="two.sided") print("alternative hypothesis: two-sided")
  colnames(sol) <- colnames(X)
  rownames(sol) <- colnames(X)
  return(sol)
}

Mtest <- function(datos, nboot = 500,
                  nsam = NULL,trace = TRUE,seed = NULL,
                  valor_vif = 0.9)
{
  if(is.null(nsam)){nsam = nrow(datos)*3}
  
  vals <- 1:nrow(datos)
  
  if(!is.null(seed)) {set.seed(seed)}
  
  sol.rsq <- NULL
  sol.vif <- NULL
  i = 1
  while(i <=nboot)
  {
    sam <- sample(vals,nsam,replace = TRUE)
    aux <- datos[sam,]
    maux <- lm(y~.,data = aux)
    sm  <- summary(maux)
    vif.vals <- vif(maux)
    Raux <- (vif.vals-1)/vif.vals
    
    s1 <- c(sm$r.squared,Raux)
    sol.rsq <- rbind(sol.rsq,s1)
    
    sol.vif <- rbind(sol.vif,vif.vals)
    
    if(trace)
    {
      cat("Iteration",i,"out of ",nboot,"\n")
    }
    i = i+1
  }
  
  pval_vif <- NULL
  for(j in 2:ncol(sol.rsq))
  {
    pval_vif <- c(pval_vif,sum(sol.rsq[,j]>valor_vif)/nboot)
  }
  names(pval_vif) <- colnames(sol.rsq)[2:ncol(sol.rsq)]
  pval_klein <- NULL
  for(z in 2:ncol(sol.rsq))
  {
    pval_klein <- c(pval_klein,sum(sol.rsq[,1]<sol.rsq[,z])/nboot)
  }
  names(pval_klein) <- colnames(sol.rsq)[2:ncol(sol.rsq)]
  
  colnames(sol.rsq) <- c("global",paste(names(datos)[-1],sep =""))
  rownames(sol.rsq) <- 1:nrow(sol.rsq)
  return(list(Bvals= sol.rsq,pval_vif = pval_vif,pval_klein=pval_klein))
}

#####


# Sample size:
n <- round(seq(20,200, by = 60))
n <- c(20,100,200)
# Covariates
p <- seq(3,5,by =1) 
# Correlation structure:
theta <- seq(0.8,0.99,0.02)
# Coefficients of the regression:
mm <- 10#c(0,10,2)
ss <- 10#sqrt(seq(0.1,2,0.1)) # sd simulation
s.d <- 3 # deviation of the residual

i = 1 #1:length(n)
j = 1 #1:length(p)
k = 1 #1:length(theta)

### ****************************** SET UP: p = 3, n = 20
vif_a <- NULL
klein_a <- NULL
vif_vals <- NULL

contador <- 1

j = 1#p
i = 3#n
k = 1#theta
set.seed(8519)
for(k in 1:length(theta))
{
  X_sol <- NULL
  W <- matrix(rnorm(n[i]*p[j],mean = mm,sd = ss),ncol = p[j])
  
  for(l in 1:p[j])
  {
    X_sol <- cbind(X_sol,W[,l]*sqrt(1-theta[k]^2)+W[,p[j]]*theta[k])
  }
  cat("\n","dim(X_sol): ",dim(X_sol),"dim(W)",dim(W),"\n")
  print(cor(X_sol))
  
  betas <- runif(p[j],-5,5)
  M <- cbind(1,X_sol)
  # Output simulation
  y <- M %*% c(1,betas) + rnorm(nrow(X_sol),0,s.d)
  datos <- data.frame(y,X_sol)
  
  boot.sol <- Mtest(datos,trace=FALSE,seed = 1,nboot = 500,valor_vif = 0.9)
  vif_a <- rbind(vif_a,boot.sol$pval_vif)
  klein_a <- rbind(klein_a,boot.sol$pval_klein)
  m1 <- lm(y~.,data = (datos))
  (vif.vals <- car::vif(m1))
  vif_vals <- rbind(vif_vals,vif.vals)
}
row.names(vif_vals) <- 1:nrow(vif_vals)
vif_a
vif_vals


### ****************************** SET UP: p = 4, n = 20
vif_a <- NULL
klein_a <- NULL
vif_vals <- NULL

contador <- 1

j = 2#p
i = 3#n
k = 1#theta
set.seed(8519)
for(k in 1:length(theta))
{
  X_sol <- NULL
  W <- matrix(rnorm(n[i]*p[j],mean = mm,sd = ss),ncol = p[j])
  
  for(l in 1:p[j])
  {
    X_sol <- cbind(X_sol,W[,l]*sqrt(1-theta[k]^2)+W[,p[j]]*theta[k])
  }
  cat("\n","dim(X_sol): ",dim(X_sol),"dim(W)",dim(W),"\n")
  print(cor(X_sol))
  
  betas <- runif(p[j],-5,5)
  M <- cbind(1,X_sol)
  # Output simulation
  y <- M %*% c(1,betas) + rnorm(nrow(X_sol),0,s.d)
  datos <- data.frame(y,X_sol)
  
  boot.sol <- Mtest(datos,trace=FALSE,seed = 1,nboot = 500,valor_vif = 0.9)
  vif_a <- rbind(vif_a,boot.sol$pval_vif)
  klein_a <- rbind(klein_a,boot.sol$pval_klein)
  m1 <- lm(y~.,data = (datos))
  (vif.vals <- car::vif(m1))
  vif_vals <- rbind(vif_vals,vif.vals)
}
row.names(vif_vals) <- 1:nrow(vif_vals)
vif_a
vif_vals



### ****************************** SET UP: p = 5, n = 20
vif_a <- NULL
klein_a <- NULL
vif_vals <- NULL

contador <- 1

j = 3#p
i = 3#n
k = 1#theta
set.seed(8519)
for(k in 1:length(theta))
{
  X_sol <- NULL
  W <- matrix(rnorm(n[i]*p[j],mean = mm,sd = ss),ncol = p[j])
  
  for(l in 1:p[j])
  {
    X_sol <- cbind(X_sol,W[,l]*sqrt(1-theta[k]^2)+W[,p[j]]*theta[k])
  }
  cat("\n","dim(X_sol): ",dim(X_sol),"dim(W)",dim(W),"\n")
  print(cor(X_sol))
  
  betas <- runif(p[j],-5,5)
  M <- cbind(1,X_sol)
  # Output simulation
  y <- M %*% c(1,betas) + rnorm(nrow(X_sol),0,s.d)
  datos <- data.frame(y,X_sol)
  
  boot.sol <- Mtest(datos,trace=FALSE,seed = 1,nboot = 500,valor_vif = 0.9)
  vif_a <- rbind(vif_a,boot.sol$pval_vif)
  klein_a <- rbind(klein_a,boot.sol$pval_klein)
  m1 <- lm(y~.,data = (datos))
  (vif.vals <- car::vif(m1))
  vif_vals <- rbind(vif_vals,vif.vals)
}
row.names(vif_vals) <- 1:nrow(vif_vals)
vif_a
vif_vals

















# ******* Application


uu <- "https://raw.githubusercontent.com/vmoprojs/DataLectures/master/GA/tabla10_8.csv"
datos<- read.csv(url(uu),sep=";",header=TRUE)

datos <- datos[,c("Y","X1","X2","X3","X4","X5","TIME")]
names(datos) <- tolower(names(datos))

ajuste.2 <- lm(y~., data = datos)
saj <- summary(ajuste.2)

library("texreg")
texreg(list(saj),dcolumn = TRUE, booktabs = TRUE,
        use.packages = FALSE, label = "tab:3", caption = "Two linear models.",
        float.pos = "h")

(vif.vals <- car::vif(ajuste.2))

Raux <- (vif.vals-1)/vif.vals
Rglobal <- saj$r.squared
1/(1-Raux)

round(c(Rglobal,Raux),3)
round(vif.vals,0)

Raux>Rglobal
round(0.9955122,3)

boot.sol <- Mtest(datos,trace=FALSE,seed = 1,nboot = 1000)
boot.sol$pval_vif
boot.sol$pval_klein
round(pairwise.ks.test(X = boot.sol$Bvals),5)
# round(pairwise.ks.test(X = boot.sol, alternative = "less"),5)
round(pairwise.ks.test(X = boot.sol$Bvals, alternative = "greater"),4)


plot(density(boot.sol$Bvals[,2]))
summary(boot.sol$Bvals)
aux <- reshape2::melt(boot.sol$Bvals[,c(1,2,3,4,5,6,7)])
head(aux)
library(ggplot2)
p <- ggplot(aux, aes(y=value, color=factor(Var2))) +
  geom_boxplot()+ylim(0.9,1)

# setwd("~/Documents/Articles/EPN/MTest/Figures")
# pdf("fig2.pdf")
# p+theme(legend.title=element_blank())
# dev.off()


boxplot(aux$value~aux$Var2,ylim = c(0.9,1))




# remuevo x2
names(datos)
df1 <- datos[,-3]
names(df1)


boot.sol <- Mtest(df1,trace=TRUE,seed = 1,nboot = 1000)
boot.sol$pval_vif
boot.sol$pval_klein
# round(pairwise.ks.test(X = boot.sol$Bvals),5)
# round(pairwise.ks.test(X = boot.sol, alternative = "less"),5)
round(pairwise.ks.test(X = boot.sol$Bvals, alternative = "greater"),4)



# remuevo x2 y time
names(datos)
df1 <- datos[,-c(3,7)]
names(df1)


boot.sol <- Mtest(df1,trace=TRUE,seed = 1,nboot = 1000)
boot.sol$pval_vif
boot.sol$pval_klein
# round(pairwise.ks.test(X = boot.sol$Bvals),5)
# round(pairwise.ks.test(X = boot.sol, alternative = "less"),5)
round(pairwise.ks.test(X = boot.sol$Bvals, alternative = "greater"),4)




# remove x2 and time
names(datos)
df1 <- datos[,-c(3,6,7)]
names(df1)


boot.sol <- Mtest(df1,trace=TRUE,seed = 1,nboot = 1000)
boot.sol$pval_vif
boot.sol$pval_klein
# round(pairwise.ks.test(X = boot.sol$Bvals),5)
# round(pairwise.ks.test(X = boot.sol, alternative = "less"),5)
round(pairwise.ks.test(X = boot.sol$Bvals, alternative = "greater"),4)




# remove x2 and time
names(datos)
df1 <- datos
df1$gni <- df1$x2/df1$x1
df1 <- df1[,-c(2,3,6,7)]
names(df1)


boot.sol <- Mtest(df1,trace=TRUE,seed = 1,nboot = 1000)
boot.sol$pval_vif
boot.sol$pval_klein
# round(pairwise.ks.test(X = boot.sol$Bvals),5)
# round(pairwise.ks.test(X = boot.sol, alternative = "less"),5)
round(pairwise.ks.test(X = boot.sol$Bvals, alternative = "greater"),4)

mf <- lm(y~.,data = df1)
summary(mf)




texreg(list(mf),dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "tab:3", caption = "Two linear models.",
       float.pos = "h")
