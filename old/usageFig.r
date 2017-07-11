### only look at year 2
### and only at students who weren't in  year 1
library(splines)
library(R2jags)
memory.limit(50000)

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

################################################################
#### Code differs from main code here
dat <- subset(dat,treatment==1)
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

school <- as.numeric(as.factor(dat$schoolid2))
school <- vapply(1:ntch,function(i) school[teacher==i][1],1)
nscl <- max(school)
stopifnot(nscl==length(unique(dat$schoolid2)))

unit <- as.numeric(as.factor(advance$unit))
unit <- vapply(1:nsec,function(i) unit[section==i][1],1)
nunit <- max(unit)
stopifnot(nunit==length(unique(advance$unit)))


load('~/Google Drive/CTmodels/usageModels/fullUsage.RData')

fullEta <- mod$BUGSoutput$sims.list$studEff

impEta <- matrix(NA,nrow=nrow(fullEta),ncol=ncol(fullEta))
colnames(impEta) <- colnames(fullEta)

schoolBig <- as.numeric(as.factor(dat$schoolid2))
betas <- list()
for(i in 1:20){
    print(i)
    testStud <- seq(nstud)[schoolBig==i]
    load(paste0('~/Google Drive/CTmodels/usageModels/usage',i,'.RData'))
    impEta[,testStud] <- mod$BUGSoutput$sims.list$studEff[,testStud]
    betas[[i]] <- mod$BUGSoutput$sims.list$betaU
}

save(fullEta,impEta,file=paste0('output/usageCheck',Sys.Date(),'.RData'))

#### compare 90% intervals
library(Hmisc)
w90 <- function(x) quantile(x,.95)-quantile(x,.05)
ebDat <- NULL
samp <- floor(seq(1,2390,length=100))
rimp <- range(impEta[,samp])
rfull <- range(fullEta[,samp])
ran <- range(c(rimp,rfull))

impInts <- apply(impEta[,samp],2,function(x)quantile(x,c(0.05,0.95)))
fullInts <- apply(fullEta[,samp],2,function(x)quantile(x,c(0.05,0.95)))

impInts <- impInts+matrix(rep((seq(5)-1)*7,each=40),ncol=100)
fullInts <- fullInts+matrix(rep((seq(5)-1)*7,each=40),ncol=100)
x <- NULL
for(i in 1:5){
    for(j in 1:20){
        x <- c(x,j,j+0.2)
    }}
ydat <- NULL
for(i in 1:100)
    ydat <- cbind(ydat,fullInts[,i],impInts[,i])
errbar(x,y=rep(0,200),yplus=ydat[2,],yminus=ydat[1,],cex=0,errbar.col=rep(c('red','green'),200))
