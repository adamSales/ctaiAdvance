library(R2jags)
library(jagstools)
## regression slopes:
## convergence?
#trptVars <- mod$parameters.to.save
trptVars <- names(mod$BUGSoutput$sims.list)[sapply(mod$BUGSoutput$sims.list,ncol)<20]
#trtpVars <- setdiff(trptVars,highD)
R2jags::traceplot(mod,var=trptVars,ask=TRUE)

jagsresults(mod,trptVars)

par(mfrow=c(3,4))
for(varb in setdiff(names(mod$BUGSoutput$sims.list),trptVars)){
    print(varb)
    x <- jagsresults(mod,varb)
    hist(x[,'Rhat'],main=varb)
    }



trtEff <- mod$BUGSoutput$sims.list$trtEff
for(i in 1:ncol(trtEff)) trtEff[,i] <- trtEff[,i]+mod$BUGSoutput$sims.list$tau
studEff <- mod$BUGSoutput$sims.list$studEff

## avg trt Effect
plot(density(apply(trtEff,1,mean)))




betaU <- jagsresults(mod,'betaU')
rownames(betaU) <- colnames(X)
#betaU <- rbind(Intercept=jagsresults(mod,'alphaU'),betaU)
print(betaU)
## plot spline results:
plot(dat$xirt,X[,1:3]%*%betaU[1:3,1],xlab='Pretest',ylab='Log-Odds Grad (other covs 0)')
## (basically linear)

## variance components:
## convergence:
print(jagsresults(mod,c('tauTchU','tauSclU','tauSec')))
traceplot(mod,var=c('tauTchU','tauSclU','tauSec','tauUn'),ask=TRUE)
## since I didn't translate taus to sigmas in model, gotta do it here
##vcompU <- mod$BUGSoutput$sims.matrix[,c('tauU','tauTchU','tauSclU','tauSec','tauUn')]
## dangit forgot to track 'tauU' (fixed above)
vcompU <- mod$BUGSoutput$sims.matrix[,c('tauTchU','tauSclU','tauSec','tauUn')]
vcompU <- 1/vcompU
vcompUsumm <- apply(vcompU,2,function(x) c(mean(x),sd(x)))
## rough est of tauU:
studEff <- jagsresults(mod,'studEff')
totVarU <- var(studEff[,1])
explVarU <- var(X%*%betaU[-1,1])
cat('total variation in student effects: ',totVarU,'\n')
cat('explained variation in student effects: ',explVarU,'\n')
cat('other variance components:\n',vcompUsumm[1,],'\n')
## student effects
studEffbar <- apply(studEff,2,mean)#jagsresults(mod,'studEff')
## compare to % grad
gradStud <- aggregate(grad,by=list(studentM),mean)
gradStud$studEff <- studEffbar[sort(unique(studentM))]
gradStud$nsec <- as.vector(table(studentM))
gradStud$nsec[gradStud$nsec>100] <- 100 ## helps calibrate point sizes better
## estimated student effect vs % grad
library(ggplot2)
qplot(studEffbar,x, size=nsec,data=gradStud,ylab='% Grad',
      main=paste0('spearman rho=',with(gradStud,round(cor(studEff,x,method='spearman'),3))))+geom_hline(yintercept=c(1/6,1/5,1/4,1/3,1/2))

library(bayesplot)
prob <- mod$BUGSoutput$sims.list$prob
ppc_error_binned(as.numeric(grad),prob[seq(sample(1:nrow(prob)/10,1),to=nrow(prob),length=nrow(prob)),])


## compare to glmer fit (different model: both years & more covariates. but still.)
load('logitUsage/mod1.RData')
X1 <- model.matrix(mod1)
ids <- mod1@frame$field_id
Xstud <- aggregate(X1,by=list(id=ids),FUN=function(x) x[1])
Xstud$reff <- ranef(mod1)$field_id[as.character(Xstud$id),1]
Xstud$sid <- sid[as.character(Xstud$id)]
Xstud <- Xstud[!is.na(Xstud$sid),]
Xstud <- Xstud[order(Xstud$sid),]
Xstud <- as.matrix(Xstud[,-1])
mod1Pred <- Xstud[,names(fixef(mod1))]%*%fixef(mod1)
plot(mod1Pred,X[Xstud[,'sid'],]%*%betaU[-1,1]+betaU[1,1],xlab='glmer',ylab='PS via JAGS',main='Stud. Predictions')

mod1Ranef <- mod1Pred+Xstud[,'reff']
plot(mod1Ranef,studEffbar[Xstud[,'sid']],xlab='glmer',ylab='PS via JAGS',main='Stud. Ran. Eff.')
plot(Xstud[,'reff'],studEffbar[Xstud[,'sid']]-X[Xstud[,'sid'],]%*%betaU[-1,1]+betaU[1,1],xlab='glmer',ylab='JAGS',main='Random Part')

### do the variances make sense?
obs <- seq(nstud)%in%unique(studentM)
studEffsd <- apply(studEff,2,sd)
boxplot(studEffsd~obs,ylab='SD of $\\eta$')
plot(as.vector(table(studentM)),studEffsd[sort(unique(studentM))])
## checks out

## outcome
## regression slopes:
## convergence?
traceplot(mod,var=c('betaY'),ask=FALSE)

betaY <- jagsresults(mod,'betaY')
rownames(betaY) <- colnames(X)
cat('coefficients for outcome regression:\n')
print(betaY)
## plot spline results:
plot(dat$xirt,X[,1:3]%*%betaY[1:3,1],xlab='Pretest',ylab='Pred. Y (all other covs 0)')
## (basically linear)

## variance components:
## convergence:
print(jagsresults(mod,c('tauTchY','tauSclY','tauY')))
traceplot(mod,var=c('tauTchY','tauSclY','tauY'),ask=FALSE)
## since I didn't translate taus to sigmas in model, gotta do it here
vcompY <- mod$BUGSoutput$sims.matrix[,c('tauTchY','tauSclY','tauY[1]','tauY[2]')]
vcompY <- 1/vcompY
vcompYsumm <- print(apply(vcompY,2,function(x) c(mean(x),sd(x))))

### model predictions vs truth
Ynew <- jagsresults(mod,'Ynew')
hist(Ynew[,'Rhat'])
plot(Ynew[,1],Y)
abline(0,1)
library(lattice)
zz <- factor(ifelse(Z==1,'trt','ctl'))
xyplot(Y-Ynew[,1]~Ynew[,1],groups=zz,auto.key=TRUE)
mean(Y-Ynew[,1])
res <- Y-Ynew[,1]
print(summary(lm(res~Ynew[,1]+Z)))

plotYdat <- data.frame(Y=Y,Yhat=Ynew[,1],res=res,Z=as.factor(Z))
ggplot(plotYdat,aes(Yhat,res,color=Z))+geom_point()+geom_smooth()

library(bayesplot)
Ynew <- mod$BUGSoutput$sims.matrix[,paste0('Ynew[',1:length(Y),']')]
ppc_dens_overlay(Y,Ynew[sample(1:nrow(Ynew),50),])
ppc_dens_overlay(Y[Z==1],Ynew[sample(1:nrow(Ynew),50),Z==1])
ppc_dens_overlay(Y[Z==0],Ynew[sample(1:nrow(Ynew),50),Z==0])


############ treatment effects
studEff <- mod$BUGSoutput$sims.list$studEff
useEff <- mod$BUGSoutput$sims.list$useEff
#trtEff <- mod$BUGSoutput$sims.list$trtEff
Ustd <- mod$BUGSoutput$sims.matrix[,paste0('Ustd[',1:nstud,']')]
set.seed(613)
runs <- sample(1:nrow(studEff),200)

plot(studEff[1,],useEff[1,]+trtEff[1,],col='white',ylim=range(useEff+trtEff))
for(rr in runs){
    lines(Ustd[rr,],useEff[rr,],col='red')
    lines(Ustd[rr,],useEff[rr,]+trtEff[rr,],col='blue')
}

plotEffect <- function(simList,summ){
    trtEff <- simList$trtEff
    studEff <- simList$studEff

    studEff95 <- quantile(studEff,c(0.025,0.975))
    plot(studEff[1,],trtEff[1,],col='white',ylim=range(trtEff[studEff<studEff95[2] & studEff>studEff95[1]]),
         xlim=studEff95)
    sapply(1:nrow(studEff), function(rr) lines(sort(studEff[rr,]),trtEff[rr,order(studEff[rr,])],
                                               col=adjustcolor('red',.3)))

    bt1 <- summ['tau']
    bt2 <- summ['bt']
    ut <- summ[grep('ut',rownames(summ)),1]
    knots <- makeKnots(length(ut))
    splineFun <- function(x){
        splinePart <- 0
        for(i in 1:length(knots))
            splinePart <- splinePart +ifelse(x>knots[i],(x-knots[i])*ut[i],0)
        splinePart
    }
    curve(bt1+bt2*x+splineFun(x),add=TRUE,lwd=2)
}
makeKnots <- function(nknots)
    qnorm(seq(1/(nknots+1),nknots/(nknots+1),length=nknots),mean=0,sd=1.5)

plotUse <- function(useEff,studEff,bu,uu){
    studEff95 <- quantile(studEff,c(0.025,0.975))
    plot(studEff[1,],useEff[1,],col='white',ylim=range(useEff[studEff<studEff95[2] & studEff>studEff95[1]]),
         xlim=studEff95)
    sapply(1:nrow(studEff), function(rr) lines(sort(studEff[rr,]),useEff[rr,order(studEff[rr,])],
                                               col=adjustcolor('red',.2)))

    bu1 <- mean(bu[,1])
    bu2 <- mean(bu[,2])

    uu <- colMeans(uu)
    knots <- makeKnots(length(uu))
    splineFun <- function(x){
        splinePart <- 0
        for(i in 1:length(knots))
            splinePart <- splinePart +ifelse(x>knots[i],(x-knots[i])*uu[i],0)
        splinePart
    }
    curve(bu1+bu2*x+splineFun(x),add=TRUE,lwd=2)
}

plot(studEff[1,],useEff[1,],col='white',ylim=range(useEff),xlim=range(studEff))
for(rr in runs){
    lines(sort(studEff[rr,]),useEff[rr,order(studEff[rr,])],col='red')
}

### plotting estimated treatment effect function
aEst <- mod$BUGSoutput$sims.matrix[,c('b0','b1','b2')]
curve(0.24+ifelse(x>0,-0.145*x,0.069*x),-2,2,ylim=range(aEst[,'b0']),lwd=2)
for(rr in runs){
    curve(aEst[rr,'b0']+ifelse(x>0,aEst[rr,'b1']*x,aEst[rr,'b2']*x),-4,4,add=TRUE,col='pink')
    }

cat('selection and treatment effects:\n')
print(jagsresults(mod,c('a0','a1','a2','b0','b1','b2')))

### ATE
ateMod <- lmer(Y~Z+X+(1|teachid2)+(1|schoolid2),data=dat)

ateDist <- rowMeans(trtEff)

plot(density(ateDist))
abline(v=fixef(ateMod)['Z'])






