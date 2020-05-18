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
