### only look at year 2
### and only at students who weren't in  year 1
library(splines)
library(R2jags)
memory.limit(50000)

load('../../data/RANDstudyData/HSdata.RData')
load('../../data/sectionLevelUsageData/advanceData.RData')

dat <- dat[!dat$field_id%in%dat$field_id[dat$year==1],]
table(dat$year)

dat <- droplevels(dat)
### look at promotion
### delete "CP" sections. Logic: these are missing data, since we don't get to see if student
### would have graduated. Student random effects calculated using observed sections. FIML?
advance <- droplevels(subset(advance, status%in%c('graduated','promoted')))

advance <- advance[advance$field_id%in%dat$field_id[dat$treatment==1],]

### just look at algebra I sections--- likely different advance patterns in other curricula
### make sure to keep algebra i units that are also part of other curricula
algUnit <- unique(advance$unit[advance$curriculum=='algebra i'])
advance <- subset(advance,unit%in%algUnit)

advance$grad <- advance$status=='graduated'

advance <- droplevels(advance)


### discard some pairs
## discard treatment schools with no usage data
## (do a robustness check with everything left in afterwards)
percUse <- function(scl)
    length(intersect(unique(dat$field_id[dat$schoolid2==scl]),unique(advance$field_id)))/
        length(unique(dat$field_id[dat$schoolid2==scl]))

obsUse <- vapply(unique(dat$schoolid2[dat$treatment==1]),percUse,1)

obsUse <- unique(dat$schoolid2[dat$treatment==1])[obsUse>0.1]
obsUse <- c(as.character(obsUse),as.character(unique(dat$schoolid2[dat$treatment==0])))

dat <- dat[dat$schoolid2%in%obsUse,]

## discard pairs with only a treatment or a control school
pairTrtTab <- with(dat,table(pair,treatment))
trtVar <- apply(pairTrtTab,1,prod)
dat <- dat[trtVar[dat$pair]>0,]

advance <- advance[advance$field_id%in%dat$field_id,]

aaa <- aggregate(advance$grad,by=list(section=advance$section),FUN=mean)
aaa$n <- as.vector(table(advance$section))

advance <- subset(advance,section%in%aaa$section[aaa$n>100 & aaa$x<1] & year==2)

advance <- droplevels(advance)
dat <- droplevels(dat)

dat$sid <- 1:nrow(dat)

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

## just linear xirt effects this time
X <- model.matrix(~xirt+race+sex+spec,data=dat)[,-1]
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

##### trying a second time


## for calculating the variance of studEff
covX <- cov(X)

### forgot pair fixed effects
pair <- as.numeric(dat$pair)
npair <- max(pair)
stopifnot(npair==length(unique(dat$pair)))

jagsDat <- list('nsecWorked','grad','studentM','section','X','teacher','Z','Xm','school','nstud','Y','ntch','nscl','nsec','unit','nunit','ncovar','covX','pair','npair')

params <- c('studEff','secEff','alphaGrad','betaU','teacherEffU','Ynew','sig2Y','useEff','trtEff','betaY','teacherEffY','Ustd','a0','a1','a2','b0','b1','b2','sig2TchU','sig2TchY','sig2SclU','sig2SclY','sig2Sec','sig2Un','sig2U','pairEffect','ab','sdSeff','avgSeff')



print(Sys.time())
mod3 <- jags(jagsDat,parameters=params,model.file='src/psmod.bug',n.chains=4,n.iter=4000,n.thin=4)
print(Sys.time())
save(mod3,file='psMod3.RData')

print(Sys.time())
mod4 <- jags(jagsDat,parameters=params,model.file='src/psmod2int.bug',n.chains=4,n.iter=4000,n.thin=4)
print(Sys.time())
save(mod4,file='psMod4.RData')

### results, interpretation
library(jagstools)
pdf('psMod2plots.pdf')
sink('psdMod2results.txt')
## usage:

## regression slopes:
## convergence?
traceplot(mod2,var=c('alphaGrad','betaU'),ask=TRUE)
# intercept & spline results bad; race not so good

betaU <- jagsresults(mod2,'betaU')
rownames(betaU) <- colnames(X)
#betaU <- rbind(Intercept=jagsresults(mod2,'alphaGrad'),betaU)
print(betaU)

## variance components:
## convergence:
print(jagsresults(mod2,c('sig2U','sig2TchU','sig2SclU','sig2Sec','sig2Un')))
traceplot(mod2,var=c('sig2U','sig2TchU','sig2SclU','sig2Sec','sig2Un'),ask=TRUE)

## student effects
studEff <- jagsresults(mod2,'studEff')
hist(studEff[,'Rhat'])
boxplot(studEff[,'Rhat']~Z)

## compare to % grad
gradStud <- aggregate(grad,by=list(studentM),mean)
gradStud$studEff <- studEff[sort(unique(studentM)),1]
gradStud$nsec <- as.vector(table(studentM))
gradStud$nsec[gradStud$nsec>100] <- 100 ## helps calibrate point sizes better
## estimated student effect vs % grad
library(ggplot2)
qplot(studEff,x, size=nsec,data=gradStud,ylab='% Grad',
      main=paste0('spearman rho=',round(with(gradStud,cor(studEff,x,method='spearman')),3)))

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
traceplot(mod2,var=c('betaY'),ask=TRUE)

betaY <- jagsresults(mod2,'betaY')
rownames(betaY) <- colnames(X)
cat('coefficients for outcome regression:\n')
print(betaY)

## variance components:
## convergence:
print(jagsresults(mod2,c('sig2TchY','sig2SclY','sig2Y')))
traceplot(mod2,var=c('sig2TchY','sig2SclY','sig2Y'),ask=TRUE)

### model predictions vs truth
Ynew <- jagsresults(mod2,'Ynew')
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
Ynew <- mod2$BUGSoutput$sims.matrix[,paste0('Ynew[',1:length(Y),']')]
ppc_dens_overlay(Y,Ynew[sample(1:nrow(Ynew),50),])
ppc_dens_overlay(Y[Z==1],Ynew[sample(1:nrow(Ynew),50),Z==1])
ppc_dens_overlay(Y[Z==0],Ynew[sample(1:nrow(Ynew),50),Z==0])


############ treatment effects
useEff <- mod2$BUGSoutput$sims.matrix[,paste0('useEff[',1:nstud,']')]
trtEff <- mod2$BUGSoutput$sims.matrix[,paste0('trtEff[',1:nstud,']')]
Ustd <- mod2$BUGSoutput$sims.matrix[,paste0('Ustd[',1:nstud,']')]
runs <- sample(1:nrow(Ustd),200)

## plot(Ustd[1,],useEff[1,]+trtEff[1,],col='white',ylim=range(useEff+trtEff))
## for(rr in runs){
##     lines(Ustd[rr,],useEff[rr,],col='red')
##     lines(Ustd[rr,],useEff[rr,]+trtEff[rr,],col='blue')
## }

## plot(Ustd[1,],trtEff[1,],col='white',ylim=range(trtEff),xlim=range(Ustd))
## for(rr in runs[1:50]){
##     lines(Ustd[rr,],trtEff[rr,],col='red')
## }


### plotting estimated treatment effect function
aEst <- mod2$BUGSoutput$sims.matrix[,c('b0','b1','b2')]
aMean <- colMeans(aEst)
ustdM <- colMeans(Ustd)
curve(aMean['b0']+ifelse(x>0,aMean['b1']*x,aMean['b2']*x),from=min(ustdM),to=max(ustdM),ylim=range(trtEff),lwd=2)
for(rr in runs){
    curve(aEst[rr,'b0']+ifelse(x>0,aEst[rr,'b1']*x,aEst[rr,'b2']*x),add=TRUE,col='pink')
    }
curve(aMean['b0']+ifelse(x>0,aMean['b1']*x,aMean['b2']*x),ylim=range(aEst[,'b0']),lwd=2,add=T)
rug(colMeans(Ustd))

cat('selection and treatment effects:\n')
print(jagsresults(mod2,c('a0','a1','a2','b0','b1','b2')))

traceplot(mod2,var=c('a0','a1','a2','b0','b1','b2'),ask=TRUE)
### ATE
ateMod <- lmer(Y~Z+X+(1|teachid2)+(1|schoolid2),data=dat)

ateDist <- rowMeans(trtEff)

plot(density(ateDist))
abline(v=fixef(ateMod)['Z'])

dev.off()
sink()


