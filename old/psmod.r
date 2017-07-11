### only look at year 2
library(splines)
library(R2jags)

load('../../data/RANDstudyData/HSdata.RData')
pload('../../data/sectionLevelUsageData/advanceData.RData')

dat <- dat[dat$year==2,]
dat <- dat[!duplicated(dat$field_id),] # removes second mention
dat$sid <- 1:nrow(dat)

dat <- droplevels(dat)
### look at promotion
### delete "CP" sections. Logic: these are missing data, since we don't get to see if student
### would have graduated. Student random effects calculated using observed sections. FIML?
advance <- droplevels(subset(advance, status%in%c('graduated','promoted')))

### just look at algebra I sections--- likely different advance patterns in other curricula
### make sure to keep algebra i units that are also part of other curricula
algUnit <- unique(advance$unit[advance$curriculum=='algebra i'])
advance <- subset(advance,unit%in%algUnit)

advance$grad <- advance$status=='graduated'

advance <- advance[advance$field_id%in%dat$field_id,]

advance <- droplevels(advance)

nsecWorked <- nrow(advance)
grad <- advance$grad
sid <- dat$sid
names(sid) <- dat$field_id
studentM <- sid[as.character(advance$field_id)]
nstud <- max(sid)
stopifnot(nstud==length(unique(dat$field_id)))
section <- as.numeric(as.factor(advance$section))
nsec <- max(section)
stopifnot(nsec==length(unique(advance$section)))

X <- model.matrix(~ns(xirt,3)+race+sex+spec,data=dat)[,-1]
X <- scale(X)
ncovar <- ncol(X)

teacher <- as.numeric(as.factor(dat$teachid2))
ntch <- max(teacher)
stopifnot(ntch==length(unique(dat$teachid2)))

Z <- as.numeric(dat$treatment)

Xm <- colMeans(X)

school <- as.numeric(as.factor(dat$schoolid2))
school <- vapply(1:ntch,function(i) school[teacher==i][1],1)
nscl <- max(school)
stopifnot(nscl==length(unique(dat$schoolid2)))

Y <- dat$Y

unit <- as.numeric(as.factor(advance$unit))
unit <- vapply(1:nsec,function(i) unit[section==i][1],1)
nunit <- max(unit)
stopifnot(nunit==length(unique(advance$unit)))


jagsDat <- list('nsecWorked','grad','studentM','section','X','teacher','Z','Xm','school','nstud','Y','ntch','nscl','nsec','unit','nunit','ncovar')

params <- c('prob','studEff','secEff','alphaGrad','betaU','teacherEffU','Ynew','tauY','useEff','trtEff','betaY','teacherEffY','Ustd','a0','a1','a2','b0','b1','b2','tauTchU','tauTchY','tauSclU','tauSclY','tauSec','tauUn','tauU')

print(Sys.time())
mod <- jags(jagsDat,parameters=params,model.file='src/psmod.bug',n.chains=4,n.iter=4000,n.thin=4)
print(Sys.time())
save(mod,file='psMod1.RData')


### results, interpretation
library(jagstools)
pdf('psMod1plots.pdf')
sink('psdMod1results.txt')
## usage:

## regression slopes:
## convergence?
traceplot(mod,var=c('alphaGrad','betaU'),ask=FALSE)
# intercept & spline results bad; race not so good

betaU <- jagsresults(mod,'betaU')
rownames(betaU) <- colnames(X)
betaU <- rbind(Intercept=jagsresults(mod,'alphaGrad'),betaU)
print(betaU)
## plot spline results:
plot(dat$xirt,X[,1:3]%*%betaU[2:4,1]+betaU[1,1],xlab='Pretest',ylab='Log-Odds Grad (other covs 0)')
## (basically linear)

## variance components:
## convergence:
print(jagsresults(mod,c('tauTchU','tauSclU','tauSec','tauUn')))
traceplot(mod,var=c('tauTchU','tauSclU','tauSec','tauUn'),ask=FALSE)
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
studEff <- jagsresults(mod,'studEff')
## compare to % grad
gradStud <- aggregate(grad,by=list(studentM),mean)
gradStud$studEff <- studEff[sort(unique(studentM)),1]
gradStud$nsec <- as.vector(table(studentM))
gradStud$nsec[gradStud$nsec>100] <- 100 ## helps calibrate point sizes better
## estimated student effect vs % grad
library(ggplot2)
qplot(studEff,x, size=nsec,data=gradStud,ylab='% Grad',
      main=paste0('spearman rho=',with(gradStud,cor(studEff,x,method='spearman'))))

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
plot(mod1Ranef,studEff[Xstud[,'sid'],1],xlab='glmer',ylab='PS via JAGS',main='Stud. Ran. Eff.')
plot(Xstud[,'reff'],studEff[Xstud[,'sid'],1]-X[Xstud[,'sid'],]%*%betaU[-1,1]+betaU[1,1],xlab='glmer',ylab='JAGS',main='Random Part')

### do the variances make sense?
obs <- seq(nstud)%in%unique(studentM)
boxplot(studEff[,2]~obs)
plot(as.vector(table(studentM)),studEff[sort(unique(studentM)),2])
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
useEff <- mod$BUGSoutput$sims.matrix[,paste0('useEff[',1:nstud,']')]
trtEff <- mod$BUGSoutput$sims.matrix[,paste0('trtEff[',1:nstud,']')]
Ustd <- mod$BUGSoutput$sims.matrix[,paste0('Ustd[',1:nstud,']')]
runs <- sample(1:nrow(Ustd),200)

plot(Ustd[1,],useEff[1,]+trtEff[1,],col='white',ylim=range(useEff+trtEff))
for(rr in runs){
    lines(Ustd[rr,],useEff[rr,],col='red')
    lines(Ustd[rr,],useEff[rr,]+trtEff[rr,],col='blue')
}

plot(Ustd[1,],trtEff[1,],col='white',ylim=range(trtEff),xlim=range(Ustd))
for(rr in runs){
    lines(Ustd[rr,],trtEff[rr,],col='red')
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

dev.off()
sink()


