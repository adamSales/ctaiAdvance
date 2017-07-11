library(R2jags)
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

makeKnotsX <- function(nknots)
    quantile(xirt,seq(1/(nknots+1),nknots/(nknots+1),length=nknots))

X <- cbind(ns(dat$xirt,3),X)
X <- scale(X,scale=FALSE)
ncovar <- ncol(X)

jagsDat <- c('nsecWorked','grad','studentM','section','X','teacher','Z','Xm','school','nstud','Y','ntch','nscl','nsec','unit','nunit','ncovar','npair','pair','knot','nknots','nclass','cid','Zcls','Zscl')


params <- c('studEff','secEff','alphaU','betaU','classEffU','schoolEffU','Ynew','tauY','useEff','trtEff','betaY','classEffY','classEffX','schoolEffY','Ustd','a0','a1','a2','b0','b1','b2','tauClsU','tauClsY','tauSclU','tauSclY','tauSec','tauUn','tauU','sigU','sigY','sigClsU','sigClsY','sigClsX','sigSclU','sigSclY','siguu','bu','bt','uu','ut','u','betaX','sdstu','uxu','uxy','siguuxy','siguuxu','secEff','pairEffect','tau','alphaY','sigUn','sigSec','unitEff','studEffStd')

### for(kkk in 1:5){
###     print(kkk)
###     nknots <- kkk
###     knot <- makeKnots(kkk)
###     print(Sys.time())
###     mod <- jags(jagsDat,parameters=params,model.file='src/psmodLinSpline.bug',n.chains=4,n.iter=4000,n.thin=8)
###     save(mod,file=paste0('splineMod',kkk,'.RData'))
###     rm(mod); gc()
### }


nknots <- 5
knot <- makeKnots(nknots)


print(Sys.time())
jagsDat <- sapply(jagsDat,get,simplify=FALSE,USE.NAMES=TRUE)
jagsDat$X <- X
mod <- jags(jagsDat,parameters=params,model.file='src/psmodLinSpline.bug',n.chains=4,n.iter=8000,n.thin=8)
                                        #update(mod,n.iter=4000)
simList <- goodList(mod$BUGSoutput$sims.array,mod$parameters.to.save)
summ <- mod$BUGSoutput$summary
save(simList,summ,jagsDat,file=paste0('~/Google Drive/CTmodels/splineMod',nknots,Sys.Date(),'.RData'))
print(Sys.time())

rm(mod); gc()

############## no effect
source('src/makeData.r')

################################################################
### Here's where I make things artifical
##################################################

### first delete control schools
dat <- subset(dat,treatment==1)

### now double the dataset
dat$schoolid2 <- as.character(dat$schoolid2)
dat$teachid2 <- as.character(dat$teachid2)
dat$classid2 <- as.character(dat$classid2)
dat2 <- dat
dat2$schoolid2 <- paste0(dat2$schoolid2,'Fake')
dat2$teachid2 <- paste0(dat2$teachid2,'Fake')
dat2$field_id <- dat2$field_id*100+99
dat2$treatment <- 0
dat2$classid2 <- paste0(dat2$classid2,'Fake')


dat <- rbind(dat,dat2)

### now delete usage data for "control" group
# advance <- advance[advance$field_id%in%dat$field_id,]
advance <- advance[advance$field_id%in%dat$field_id[dat$treatment==1],]

#########################################################
### The rest is the same
#####################################################

source('src/dataPrep.r')

X <- cbind(ns(dat$xirt,3),X)
X <- scale(X,scale=FALSE)
ncovar <- ncol(X)

jagsDat <- c('nsecWorked','grad','studentM','section','X','teacher','Z','Xm','school','nstud','Y','ntch','nscl','nsec','unit','nunit','ncovar','npair','pair','knot','nknots','nclass','cid','Zcls','Zscl')

nknots <- 5
knot <- makeKnots(nknots)


print(Sys.time())
jagsDat <- sapply(jagsDat,get,simplify=FALSE,USE.NAMES=TRUE)
jagsDat$X <- X
mod <- jags(jagsDat,parameters=params,model.file='src/psmodLinSpline.bug',n.chains=4,n.iter=8000,n.thin=8)
                                        #update(mod,n.iter=4000)
simList <- goodList(mod$BUGSoutput$sims.array,mod$parameters.to.save)
summ <- mod$BUGSoutput$summary
save(simList,summ,jagsDat,file=paste0('~/Google Drive/CTmodels/splineModNoEffect',nknots,Sys.Date(),'.RData'))
print(Sys.time())

rm(mod); gc()


##################################################
#### constant effect
####################################################
source('src/makeData.r')

### first delete control schools
dat <- subset(dat,treatment==1)

### now double the dataset
dat$schoolid2 <- as.character(dat$schoolid2)
dat$teachid2 <- as.character(dat$teachid2)
dat$classid2 <- as.character(dat$classid2)
dat2 <- dat
dat2$schoolid2 <- paste0(dat2$schoolid2,'Fake')
dat2$teachid2 <- paste0(dat2$teachid2,'Fake')
dat2$field_id <- dat2$field_id*100+99
dat2$treatment <- 0
dat2$classid2 <- paste0(dat2$classid2,'Fake')
dat <- rbind(dat,dat2)


### now delete usage data for "control" group
# advance <- advance[advance$field_id%in%dat$field_id,]
advance <- advance[advance$field_id%in%dat$field_id[dat$treatment==1],]

### Add generic treatment effect
## numbers came from mod2:
## > te <- jagsresults(mod2,'trtEff')[,1]
## > mean(te)
## [1] 0.1783
## > sd(te)
## [1] 0.09942
dat$Y[dat$treatment==1] <- dat$Y[dat$treatment==1]+rnorm(sum(dat$treatment==1),0.18,0.1)

source('src/dataPrep.r')

X <- cbind(ns(dat$xirt,3),X)
X <- scale(X,scale=FALSE)
ncovar <- ncol(X)

jagsDat <- c('nsecWorked','grad','studentM','section','X','teacher','Z','Xm','school','nstud','Y','ntch','nscl','nsec','unit','nunit','ncovar','npair','pair','knot','nknots','nclass','cid','Zcls','Zscl')

nknots <- 5
knot <- makeKnots(nknots)


print(Sys.time())
jagsDat <- sapply(jagsDat,get,simplify=FALSE,USE.NAMES=TRUE)
jagsDat$X <- X
mod <- jags(jagsDat,parameters=params,model.file='src/psmodLinSpline.bug',n.chains=4,n.iter=8000,n.thin=8)
simList <- goodList(mod$BUGSoutput$sims.array,mod$parameters.to.save)
summ <- mod$BUGSoutput$summary
save(simList,summ,jagsDat,file=paste0('~/Google Drive/CTmodels/splineModSameEffect',nknots,Sys.Date(),'.RData'))
print(Sys.time())


rm(mod); gc()



###########################################################################################################
############## linear effect
###########################################################################################################
source('src/makeData.r')


#######################################
### first delete control schools
dat <- subset(dat,treatment==1)

### now double the dataset
dat$schoolid2 <- as.character(dat$schoolid2)
dat$teachid2 <- as.character(dat$teachid2)
dat$classid2 <- as.character(dat$classid2)
dat2 <- dat
dat2$schoolid2 <- paste0(dat2$schoolid2,'Fake')
dat2$teachid2 <- paste0(dat2$teachid2,'Fake')
dat2$field_id <- dat2$field_id*100+99
dat2$treatment <- 0
dat2$classid2 <- paste0(dat2$classid2,'Fake')
dat <- rbind(dat,dat2)

### now delete usage data for "control" group
# advance <- advance[advance$field_id%in%dat$field_id,]
advance <- advance[advance$field_id%in%dat$field_id[dat$treatment==1],]

### Add linear treatment effect
load('~/Google Drive/CTmodels/advance/logitUsage/mod1.RData')
Xm <- model.matrix( ~ (grade + race + sex + frl + xirt + state + esl + spec) * year ,data=dat)
U <- Xm%*%fixef(mod1)[colnames(Xm)]
ref <- ranef(mod1)
U <- U+ref$teachid2[dat$teachid2,1]
U <- U+ref$schoolid2[dat$schoolid2,1]
U[dat$field_id%in%rownames(ref$field_id)] <- U[dat$field_id%in%rownames(ref$field_id)]+
    ref$field_id[as.character(dat[dat$field_id%in%rownames(ref$field_id),'field_id']),1]

## same mean and var of treatment effect as in mod2
te <- 0.1/sd(U,na.rm=TRUE)*U[dat$treatment==1]
te <- te-mean(te)+0.18
dat$Y[dat$treatment==1] <- dat$Y[dat$treatment==1]+te
####################################################################################


source('src/dataPrep.r')

X <- cbind(ns(dat$xirt,3),X)
X <- scale(X,scale=FALSE)
ncovar <- ncol(X)

jagsDat <- c('nsecWorked','grad','studentM','section','X','teacher','Z','Xm','school','nstud','Y','ntch','nscl','nsec','unit','nunit','ncovar','npair','pair','knot','nknots','nclass','cid','Zcls','Zscl')

nknots <- 5
knot <- makeKnots(nknots)


print(Sys.time())
jagsDat <- sapply(jagsDat,get,simplify=FALSE,USE.NAMES=TRUE)
jagsDat$X <- X
mod <- jags(jagsDat,parameters=params,model.file='src/psmodLinSpline.bug',n.chains=4,n.iter=8000,n.thin=8)
                            #update(mod,n.iter=4000)
    simList <- goodList(mod$BUGSoutput$sims.array,mod$parameters.to.save)
summ <- mod$BUGSoutput$summary
save(simList,summ,jagsDat,file=paste0('~/Google Drive/CTmodels/splineModLinEffect',nknots,Sys.Date(),'.RData'))
print(Sys.time())

rm(mod); gc()
#####################################################################################################


###########################################################################################################
############## quadratic effect
###########################################################################################################
source('src/makeData.r')


### first delete control schools
dat <- subset(dat,treatment==1)

### now double the dataset
dat$schoolid2 <- as.character(dat$schoolid2)
dat$teachid2 <- as.character(dat$teachid2)
dat$classid2 <- as.character(dat$classid2)
dat2 <- dat
dat2$schoolid2 <- paste0(dat2$schoolid2,'Fake')
dat2$teachid2 <- paste0(dat2$teachid2,'Fake')
dat2$field_id <- dat2$field_id*100+99
dat2$treatment <- 0
dat2$classid2 <- paste0(dat2$classid2,'Fake')
dat <- rbind(dat,dat2)

### now delete usage data for "control" group
# advance <- advance[advance$field_id%in%dat$field_id,]
advance <- advance[advance$field_id%in%dat$field_id[dat$treatment==1],]

### Add linear treatment effect
#load('logitUsage/mod1.RData')
Xm <- model.matrix( ~ (grade + race + sex + frl + xirt + state + esl + spec) * year ,data=dat)
U <- Xm%*%fixef(mod1)[colnames(Xm)]
ref <- ranef(mod1)
U <- U+ref$teachid2[dat$teachid2,1]
U <- U+ref$schoolid2[dat$schoolid2,1]
U[dat$field_id%in%rownames(ref$field_id)] <- U[dat$field_id%in%rownames(ref$field_id)]+
    ref$field_id[as.character(dat[dat$field_id%in%rownames(ref$field_id),'field_id']),1]

## same mean and var of treatment effect as in mod2
te <- -(U[dat$treatment==1]-mean(U[dat$treatment==1]))^2
te <- te/sd(te)*0.1
te <- te-mean(te)+0.18
dat$Y[dat$treatment==1] <- dat$Y[dat$treatment==1]+te


source('src/dataPrep.r')

X <- cbind(ns(dat$xirt,3),X)
X <- scale(X,scale=FALSE)
ncovar <- ncol(X)

jagsDat <- c('nsecWorked','grad','studentM','section','X','teacher','Z','Xm','school','nstud','Y','ntch','nscl','nsec','unit','nunit','ncovar','npair','pair','knot','nknots','nclass','cid','Zcls','Zscl')

nknots <- 5
knot <- makeKnots(nknots)


print(Sys.time())
jagsDat <- sapply(jagsDat,get,simplify=FALSE,USE.NAMES=TRUE)
jagsDat$X <- X
mod <- jags(jagsDat,parameters=params,model.file='src/psmodLinSpline.bug',n.chains=4,n.iter=8000,n.thin=8)
                                        #update(mod,n.iter=4000)
simList <- goodList(mod$BUGSoutput$sims.array,mod$parameters.to.save)
summ <- mod$BUGSoutput$summary
save(simList,summ,jagsDat,file=paste0('~/Google Drive/CTmodels/splineModQuadEffect',nknots,Sys.Date(),'.RData'))
print(Sys.time())

rm(mod); gc()
