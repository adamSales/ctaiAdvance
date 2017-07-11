### only look at year 2
### and only at students who weren't in  year 1
library(splines)
library(R2jags)
memory.limit(50000)

#save(list=ls(),file=paste0('prev',Sys.time(),'.RData'))
rm(list=ls())

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


################################################################
### Here's where I make things artifical
###
### No relationship between effect and mastery
##################################################

### first delete control schools
dat <- subset(dat,treatment==1)

### now double the dataset
dat$schoolid2 <- as.character(dat$schoolid2)
dat$teachid2 <- as.character(dat$teachid2)
dat2 <- dat
dat2$schoolid2 <- paste0(dat2$schoolid2,'Fake')
dat2$teachid2 <- paste0(dat2$teachid2,'Fake')
dat2$field_id <- dat2$field_id*100+99
dat2$treatment <- 0
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

#########################################################
### The rest is the same
#####################################################

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

params <- c('studEff','secEff','alphaGrad','betaU','teacherEffU','Ynew','sig2Y','useEff','trtEff','betaY','teacherEffY','Ustd','a0','a1','a2','b0','b1','b2','sig2TchU','sig2TchY','sig2SclU','sig2SclY','sig2Sec','sig2Un','sig2U','pairEffect')



print(Sys.time())
modSameEff <- jags(jagsDat,parameters=params,model.file='src/psmod.bug',n.chains=4,n.iter=4000,n.thin=4)
print(Sys.time())
save(modSameEff,file='psModSameEff.RData')


### results:
pdf('constSimResults.pdf')
te <- dat$Y[dat$treatment==1]-dat$Y[dat$treatment==0]

load('logitUsage/mod1.RData')
Xm <- model.matrix( ~ (grade + race + sex + frl + xirt + state + esl + spec) * year ,data=dat)
U <- Xm%*%fixef(mod1)[colnames(Xm)]
ref <- ranef(mod1)
U <- U+ref$teachid2[dat$teachid2,1]
U <- U+ref$schoolid2[dat$schoolid2,1]
U[dat$field_id%in%rownames(ref$field_id)] <- U[dat$field_id%in%rownames(ref$field_id)]+
    ref$field_id[as.character(dat[dat$field_id%in%rownames(ref$field_id),'field_id']),1]


attach.jags(modSameEff)
plot(U[Z==1],te,xlab='Propensity to Graduate Section',ylab='Treatment Effect (Simulated)',
     main='Simulated Data',ylim=c(-0.5,0.5))
for(i in seq(1,2000,by=100))
    lines(U[Z==1],trtEff[i,Z==1],col=adjustcolor('pink',0.3))
#lines(sort(U[Z==1]),te[order(U[Z==1])],lwd=2)
dev.off()
