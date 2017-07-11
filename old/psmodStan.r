library(rstan)
library(lme4)
library(splines)

source('src/jagsSims.r')
source('src/makeData.r')
source('src/dataPrep.r')

#######################
#### knots for spline
#######################
makeKnots <- function(nknots)
    qnorm(seq(1/(nknots+1),nknots/(nknots+1),length=nknots),mean=0,sd=1.5)


X <- cbind(ns(dat$xirt,3),X)
X <- scale(X,scale=FALSE)
ncovar <- ncol(X)

stanDat <- c('nsecWorked','grad','studentM','section','X','teacher','Z','Xm','school','nstud','Y','ntch','nscl','nsec','unit','nunit','ncovar','npair','pair','knots','num_knots','nclass','cid','Zcls','Zscl','spline_degree','nschool')

num_knots <- 5
knots <- makeKnots(num_knots)
spline_degree <- 1

stanDat <- sapply(stanDat,get,simplify=FALSE,USE.NAMES=TRUE)
stanDat$X <- X


mod <- stan('src/psmodLinSpline.stan',data=stanDat,iter=100,chains=1)
