### test dimensionality by running DETECT test
library(sirt)
load('fittedModels/stanMod.RData')
score <- colMeans(rstan::extract(main,'studEff')[[1]])

dat <- matrix(NA,nrow=sdat$nstud,ncol=sdat$nsec)
for(i in 1:sdat$nsecWorked) dat[sdat$studentM[i],sdat$section[i]] <- sdat$grad[i]

rowObs <- apply(dat,1,function(x) sum(!is.na(x)))

dat <- cbind(dat,score)

dat <- dat[rowObs>30,]
colObs <- apply(dat,2,function(x) sum(!is.na(x)))


detect <- conf.detect(dat[,-ncol(dat)],dat[,ncol(dat)],factor(rep(1,ncol(dat)-1)))


### results:
##           unweighted weighted
## DETECT         0.058    0.011
## ASSI           0.006   -0.015
## RATIO          0.042    0.010
## MADCOV100      1.392    1.107
## MCOV100        0.058    0.011
