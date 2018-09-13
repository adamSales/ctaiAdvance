### test dimensionality by running DETECT test
library(sirt)
load('fittedModels/stanMod.RData')
studEff <- rstan::extract(main,'studEff')[[1]]
secEff <- rstan::extract(main,'secEff')[[1]]
studEff <- studEff[,sdat$Z==1]
score <- colMeans(studEff)
sdScore <- apply(studEff,2,sd)

dat <- matrix(NA,nrow=sdat$nstud,ncol=sdat$nsec)
for(i in 1:sdat$nsecWorked) dat[sdat$studentM[i],sdat$section[i]] <- sdat$grad[i]
colnames(dat) <- 1:sdat$nsec

dat <- dat[sdat$Z==1,]
#score <- score[sdat$Z==1]

ccc1 <- ccov.np(dat,score)


detect <- conf.detect(dat,score,factor(rep(1,ncol(dat))))
detectMbar <- conf.detect(dat,rowMeans(dat,na.rm=TRUE),factor(rep(1,ncol(dat))))

rowObs <- apply(dat,1,function(x) sum(!is.na(x)))

score <- score[rowObs>0]
sdScore <- sdScore[rowObs>0]
dat <- dat[rowObs>0,]

ccc2 <- ccov.np(dat,score)

ccc3 <- ccov.np(dat[rowObs>30,],score[rowObs>30])

detect <- conf.detect(dat,score,factor(rep(1,ncol(dat))))
detectMbar <- conf.detect(dat,rowMeans(dat,na.rm=TRUE),factor(rep(1,ncol(dat))))

detSD <- NULL
for(ss in seq(.5,1,.1)){
    detSD <- rbind(detSD,c(ss,
                           conf.detect(dat[sdScore<ss,],score[sdScore<ss],factor(rep(1,ncol(dat))))$detect[1,]))
}





dat <- dat[rowObs>30,]
score <- score[rowObs>30]

colObs <- apply(dat,2,function(x) sum(!is.na(x)))


detect <- conf.detect(dat,score,factor(rep(1,ncol(dat))))
detectMbar <- conf.detect(dat,rowMeans(dat,na.rm=TRUE),factor(rep(1,ncol(dat))))

### results:
##           unweighted weighted
## DETECT         0.058    0.011
## ASSI           0.006   -0.015
## RATIO          0.042    0.010
## MADCOV100      1.392    1.107
## MCOV100        0.058    0.011


exdetect <- expl.detect(dat[,-ncol(dat)],dat[,ncol(dat)],5,seed=1)


##### other approaches

## residuals:
draws <- extract(mod)

### mean eta_ip
meanVarP <- matrix(nrow=nsecWorked,ncol=2)
for(wp in 1:nsecWorked){
    lp <- with(draws,studEff[,sdat$studentM[wp]]+secEff[,sdat$section[wp]])
    prob <- plogis(lp)
    meanVarP[wp,1] <- mean(prob)
    ## var(resp) = E[var(resp|prob)]+var(E[resp|prob])
    meanVarP[wp,2] <- mean(prob*(1-prob))+var(prob)
}

res <- sdat$grad-meanVarP[,1]
standRes <- res/sqrt(meanVarP[,2])

outfit <- vapply(1:sdat$nsec,function(i) mean(standRes[sdat$section==i]^2),1)
infit <- vapply(1:sdat$nsec,function(i) sum(res[sdat$section==i]^2)/sum(meanVarP[sdat$section==i,2]),1)







#### PPMC
### use Q3 (levy et al 2009)

stud2 <- sort(unique(sdat$studentM))[table(sdat$studentM)>1]
studM2 <- sdat$studentM[sdat$studentM%in%stud2]
sec2 <- sdat$section[sdat$studentM%in%stud2]
grad2 <- sdat$grad[sdat$studentM%in%stud2]

studEffObs <- studEff[,stud2]

studEffObs <- studEffObs[seq(1,nrow(studEffObs),length.out=1000),]
secEffThin <- secEff[seq(1,nrow(secEff),length.out=1000),]

nstud <- ncol(studEffObs)
nsec <- ncol(secEffThin)

studObs <- as.numeric(as.factor(studM2))
Xobs <- matrix(NA,nrow=nstud,ncol=nsec)
for(i in 1:length(sec2)) Xobs[studObs[i],sec2[i]] <- grad2[i]





ex <- function(iter){
    linPred <- outer(studEffObs[iter,],secEffThin[iter,],'+')
    linPred[is.na(Xobs)] <- NA
    plogis(linPred)
}

xrep <- function(iter){
    Xrep <- matrix(nrow=nstud,ncol=nprob)
    prob <- ex(iter)
    Xrep[
