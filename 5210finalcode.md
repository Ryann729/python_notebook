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
```
