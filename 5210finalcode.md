```{r}
setwd('D:/secondterm/5210/final')

# library the packages
library(nloptr)
library(data.table)
library(PerformanceAnalytics)
library(CVXR)
library(ggfortify)
Rt=read.table('return_13.csv', header=T, sep=',')
colnames(Rt)[1]<-'Date'
Assets = Rt
Assets
```
```{r}
# creat weight matrix of each portfolio
w_MVO<-matrix(data=NA, ncol=60, nrow=13)
w_RP<-matrix(data=NA, ncol=60, nrow=13)
w_CVaR<-matrix(data=NA, ncol=60, nrow=13)
w_SR<-matrix(data=NA, ncol=60, nrow=13)
w_max<-matrix(data=NA, ncol=60, nrow=13)
w_GMVP<-matrix(data=NA, ncol=60, nrow=13)

# create rolling window
loop_date = as.numeric(as.vector(unique(Rt$Date)))
loop_date = loop_date[1:60]

# length of time series using to calculate the factor exposure 
i=1
win = 60

for(i in 1:60){
  
  Assets = Rt
  Assets = Assets[i:(i+win-1),-1]
  
  # compute covariance matrix and choose the specific return
  AssetCov = cov (Assets)
  AssetRtn = apply(Assets,2,mean)
  
  ######################################## MVO Portfolio ###################################################
  Vol_tgt = 0.05  #constrain
  ObjFun = function (w)
  {
    -sum (AssetRtn*w)
  }  #Objective function
  
  ObjFunGradient = function (w)
  {
    -AssetRtn
  }  #Gradient of objective function
  
  Equal = function (w)
  {
    sum(w)-1
  }  #Equality function
  
  EqualGradient = function (w)
  {
    rep(1, length (w) )
  }  #Gradient of Equality function
  
  Inequal = function (w)
  {
    t(w) %*% AssetCov %*% w-Vol_tgt^2
  }  #Inequality function
  
  InequalGradient = function (w)
  {
    t(w) %*% (AssetCov + t(AssetCov) )
  }  #Gradient of Inequality function
  
  #w0 of 13 assets
  w0 = rep(1/13, 13)
  opt = nloptr(x0=w0, eval_f = ObjFun, eval_grad_f = ObjFunGradient,
               eval_g_eq= Equal, eval_jac_g_eq =EqualGradient,
               eval_g_ineq = Inequal, eval_jac_g_ineq = InequalGradient,
               lb = rep(-10, 13), ub = rep(10, 13),  
               opts = list('algorithm' = 'NLOPT_LD_SLSQP', 'xtol_abs' = 1.0e-8, 'maxeval' = 100000000))
  w1= opt$solution
  sum(w1*AssetRtn)
  MCR1=AssetCov%*% w1 / sqrt(t(w1) %*% AssetCov %*% w1)[1,1]
  data.frame (w1, w1*MCR1)
  w_MVO[,i]=as.vector(w1)
  ######################################## MVO Portfolio ###################################################
 
  
  #################################        Risk Parity ############################################
  Assets1 = Assets[,1:12]
  AssetCov1 = cov(Assets1)
  
  #Objective function
  ObjFun = function(w)
  {
    -sum(log(abs(w)))
  }  
  #Gradient of objective function
  ObjFunGradient = function(w)
  {
    -(1/w)
  } 
  #Inequality function
  Inequal = function(w)
  {
    t(w) %*% AssetCov1 %*% w-Vol_tgt^2
  }  
  #Gradient of Inequality function
  InequalGradient = function(w)
  {
    t(w) %*% (AssetCov1 + t(AssetCov1))
  }  
  
  w0 = rep(1/12, 12)
  opt = nloptr(x0=w0, eval_f = ObjFun, eval_grad_f = ObjFunGradient,
               eval_g_ineq = Inequal, eval_jac_g_ineq = InequalGradient,
               lb = rep(-10, 12), ub = rep(10, 12),
               opts = list('algorithm' = 'NLOPT_LD_SLSQP', 'xtol_abs' = 1.0e-8, 'maxeval' = 100000000) )
  w2 = opt$solution
  w_RP[,i]=as.vector(c(w2, 1-sum(w2)))
  sum (w2)
  sum (AssetRtn*c(w2, 1-sum(w2)))
  sqrt(t(w2) %*% AssetCov1 %*% w2)
  
  MCR2 = AssetCov1 %*% w2 / sqrt(t(w2) %*% AssetCov1 %*% w2)[1,1]
  data.frame (w2, w2*MCR2)
 
  
  
  ######################################   CVaR    ############################
  portolioCVaR <- function(X, lmd = 0.5, alpha = 0.95) {
    T <- nrow(X)
    N <- ncol(X)
    X <- as.matrix(X)
    mu <- colMeans(X)
    # variables
    w <- Variable(N)
    z <- Variable(T)
    zeta <- Variable(1)
    # problem
    prob <- Problem(Maximize(t(w) %*% mu - lmd*zeta - (lmd/(T*(1-alpha))) * sum(z)),
                    constraints = list(z >= 0, z >= -X %*% w - zeta,
                                       w >= 0, sum(w) == 1))
    result <- solve(prob)
    return(as.vector(result$getValue(w)))
  }
  X=Assets
  w_CVaR[,i] <- portolioCVaR(Assets, alpha = 0.95)
  

  ##################################  Max  Sharpe Ratio portfolio #########################
  portolioMaxSharpeRatio <- function(mu, Sigma) {
    w <- Variable(nrow(Sigma))
    prob <- Problem(Minimize(quad_form(w, Sigma)),
                    constraints = list(w >= 0, t(mu) %*% w == 1))
    result <- solve(prob)
    return(as.vector(result$getValue(w)/sum(result$getValue(w))))
  }
  
  Sigma=cov(Assets)
  mu=AssetRtn
  w_SR[,i]<- portolioMaxSharpeRatio(mu,Sigma)
  
  ################################### Max Return Portfolio###########################
  
  portfolioMaxReturn <- function(mu) {
    w <- Variable(length(mu))
    prob <- Problem(Maximize(t(w) %*% mu), 
                    constraints = list(w >= 0, sum(w) == 1))
    result <- solve(prob)
    return(as.vector(result$getValue(w)))
  }
  
  w_max[,i] <- portfolioMaxReturn(AssetRtn) 
  
  #################################### GMVP Portfolio #########################
  
  portolioGMVP <- function(Sigma) {
    w <- Variable(nrow(Sigma))
    prob <- Problem(Minimize(quad_form(w, Sigma)), 
                    constraints = list(w >= 0, sum(w) == 1))
    result <- solve(prob)
    return(as.vector(result$getValue(w)))
  }
  
  Sigma=cov(Assets)
  w_GMVP[,i]<- portolioGMVP(Sigma)
  
  
}

#see the weight
data.frame (w1, w1*MCR1)
data.frame (w2, w2*MCR2)
a<-matrix(data=c(w_CVaR[,48],w_SR[,48],w_max[,48],w_GMVP[,48]),nrow=13,ncol=4)
# see the total return about MVO and Risk Parity
sum(w1*AssetRtn)
sum (AssetRtn*c(w2, 1-sum(w2)))
```

```{r}
###################################  Back-testing  #################################

Rt_MVO=vector()
Rt_RP=vector()
Rt_SR=vector()
Rt_max=vector()
Rt_GMVP=vector()

#Use the latest 6 years as testing data for rolling window backtest.
ReturnFacor=as.matrix(Rt[61:120,2:14])

for(i in 1:60){
  
  Rt_MVO[i]= ReturnFacor[i,] %*% as.matrix(w_MVO)[,i]
  Rt_RP[i]=ReturnFacor[i,] %*% as.matrix(w_RP)[,i]
  Rt_GMVP[i]=ReturnFacor[i,] %*% as.matrix(w_GMVP)[,i]
  Rt_max[i]=ReturnFacor[i,] %*% as.matrix(w_CVaR)[,i]
  Rt_SR[i]=ReturnFacor[i,] %*% as.matrix(w_SR)[,i]
  
}


Rt_MVO <- ts(Rt_MVO, start=c(2014,2), frequency=12)
Rt_RP <- ts(Rt_RP, start=c(2014,2), frequency=12)
Rt_SR<-ts(Rt_SR, start=c(2014,2), frequency=12)
Rt_max<-ts(Rt_max, start=c(2014,2), frequency=12)
Rt_GMVP<-ts(Rt_GMVP, start=c(2014,2), frequency=12)
Rt_BM<- ts(Rt[61:120,2], start=c(2014,2), frequency=12)

ret_all<-cbind(Rt_MVO,Rt_RP,Rt_SR,Rt_max,Rt_GMVP,Rt_BM)

#Table analyze
table.AnnualizedReturns(ret_all,Rf=0.0175/12)

ret_1=ts(ret_all[1:13,],start=c(2014,2), frequency=12)
#Plot back-testing of returns
chart.CumReturns(ret_1, main = "Cumulative Return for first testing year",begin="axis",
                   wealth.index = FALSE, legend.loc = "topleft",colorset = rich6equal)

chart.Histogram(Rt_MVO ,methods = "add.normal",p=0.95, breaks = 30)
autoplot(Rt_MVO, ts.geom = 'bar', facets = FALSE, main = "MVO-Monthly Return")

#Performance Summary
{ charts.PerformanceSummary(Rt_MVO,main = "Cumulative Return", geometric=TRUE,
                            legend.loc = "topleft", colorset = rich8equal)}
```

```{r}
###################################Factor based asset allocation #############################################

# factor exposure for each asset 
Factors = fread('factor_new.csv' )
Factors = Factors[(nrow (Factors) -win+1) :nrow(Factors)]

AssetExpo = matrix(0, ncol (Assets), ncol (Factors) )
rownames (AssetExpo) = colnames (Assets)
colnames (AssetExpo) = colnames (Factors)
Mapping = function (y)
{
  dat = data.table(Y = y, Factors)
  full = lm(Y~., data=dat)
  null = lm(Y~1, data=dat)
  best = step(null, scope=list(upper = full, lower = null), direction = 'both')
  list (coeff = best$coefficients[-1], res = summary (best) $residuals)
}
Spe=matrix(NA,nrow = 60,ncol=ncol(Assets))


# Stepwise regression: step
i=1
for (i in 1:ncol(Assets)) {
  a = Mapping(data.frame(Assets[, i]) [, 1])
  AssetExpo[i,names(a$coeff)] = a$coeff
  Spe[,i]=a$res
} 

# Risk exposure
autoplot(AssetExpo,colour = 'Species')



FacCov=cov(Factors)
SpeCov=cov(Spe)
TargetExpo=c(0.40,0.12,0.08,0.1,0.2,0.25)
#TargetExpo=c(0.45,0.3,0.18,0.1,0.2,0.15)
names(TargetExpo)=colnames(Factors)
lambda=0.99
ObjFun=function(w){
  M = w %*% AssetExpo - TargetExpo
  ((1-lambda) * M %*% t(M) + lambda * M %*% FacCov %*% t(M) + lambda*w %*% SpeCov %*% w)[1,1]
}

ObjFunGradient=function(w){
  M=w %*% AssetExpo-TargetExpo
  2*(1-lambda)*M %*% t(AssetExpo)+2*lambda*M %*% FacCov %*% t(AssetExpo) + 2*lambda * w %*% SpeCov
}
fullyInvested=function(w)
{
  sum(w)-1
}
fullyInvestedGradient=function(w){
  rep(1,length(w))
}

library(ggplot2) 
library(ggthemes) 

#Case1: Constrains asset weights range(-1,1)

w0=rep(1/13,13)
opt = nloptr(x0=w0, eval_f = ObjFun, eval_grad_f = ObjFunGradient,
             eval_g_ineq = fullyInvested, eval_jac_g_ineq = fullyInvestedGradient,
             lb = rep(-1, 13), ub = rep(1, 13),
             opts = list('algorithm' = 'NLOPT_LD_SLSQP', 'xtol_abs' = 1.0e-8, 'maxeval' = 100000000) )
w3 = opt$solution
w3 %*% AssetExpo

#ggplot(c, aes(x=factor, y=value)) + geom_bar(aes(fill=factor),stat="identity")  #c is the file of w3 %*% AssetExpo
#ggplot(z, aes(x=assets, y=value)) + geom_bar(aes(fill=assets),stat="identity")+coord_flip()  #z is w3



#Case2: Constrains asset weights range (0.02,1)

w0=rep(1/13,13)
opt = nloptr(x0=w0, eval_f = ObjFun, eval_grad_f = ObjFunGradient,
             eval_g_ineq = fullyInvested, eval_jac_g_ineq = fullyInvestedGradient,
             lb = rep(0.02, 13), ub = rep(1, 13),
             opts = list('algorithm' = 'NLOPT_LD_SLSQP', 'xtol_abs' = 1.0e-8, 'maxeval' = 100000000) )
w4 = opt$solution

barplot(w4 %*% AssetExpo)

#Case3
lb=c(0,0.02,0,0,0.02,0,0,0.02,0.02,0,0.02,0.02,0.02)
ub=c(0,1,0,0,1,0,0,1,1,0,1,1,1)
w0=c(0,abs(rnorm(1)),0,0,abs(rnorm(1)),0,0,abs(rnorm(1)),abs(rnorm(1)),0,abs(rnorm(1)),abs(rnorm(1)),abs(rnorm(1)))
w0=lb+w0/sum(w0)*(1-sum(lb))

opt = nloptr(x0=w0, eval_f = ObjFun, eval_grad_f = ObjFunGradient,
             eval_g_ineq = fullyInvested, eval_jac_g_ineq = fullyInvestedGradient,
             lb = lb, ub = ub,
             opts = list('algorithm' = 'NLOPT_LD_SLSQP', 'xtol_abs' = 1.0e-8, 'maxeval' = 100000000) )
w = opt$solution
w %*% AssetExpo

# Factor Risk Parity
Equal=function(w)
{
  return(rbind(sum(w)-1, w%*%(AssetExpo[,1]-AssetExpo[,2]), w%*%(AssetExpo[,1]-AssetExpo[,3]), w%*%(AssetExpo[,1]-AssetExpo[,4]), w%*%(AssetExpo[,1]-AssetExpo[,5]), w%*%(AssetExpo[,1]-AssetExpo[,6])))
  
}

EqualGradient=function(w)
{
  return(rbind(rep(1,length(w)), AssetExpo[,1]-AssetExpo[,2], AssetExpo[,1]-AssetExpo[,3], AssetExpo[,1]-AssetExpo[,4],AssetExpo[,1]-AssetExpo[,5],AssetExpo[,1]-AssetExpo[,6]))
  
}

w0=rep(1/13,13)
opt = nloptr(x0=w0, eval_f = ObjFun, eval_grad_f = ObjFunGradient,
             eval_g_ineq = Equal, eval_jac_g_ineq = EqualGradient,
             lb = rep(-1, 13), ub = rep(1, 13),
             opts = list('algorithm' = 'NLOPT_LD_SLSQP', 'xtol_abs' = 1.0e-8, 'maxeval' = 100000000) )
w5= opt$solution
barplot(w5 %*% AssetExpo)
```
