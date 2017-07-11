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

#######################
#### knots for spline
#######################
makeKnots <- function(nknots)
    qnorm(seq(1/(nknots+1),nknots/(nknots+1),length=nknots))


jagsDat <- list('nsecWorked','grad','studentM','section','X','teacher','Z','Xm','school','nstud','Y','ntch','nscl','nsec','unit','nunit','ncovar','npair','knot','nknots')

params <- c('prob','studEff','secEff','alphaGrad','betaU','teacherEffU','Ynew','tauY','useEff','trtEff','betaY','teacherEffY','Ustd','a0','a1','a2','b0','b1','b2','tauTchU','tauTchY','tauSclU','tauSclY','tauSec','tauUn','tauU','bu','bt','uu','ut')

for(kkk in 1:5){
    print(kkk)
    nknots <- kkk
    knot <- makeKnots(kkk)
    print(Sys.time())
    mod <- jags(jagsDat,parameters=params,model.file='src/psmodLinSpline.bug',n.chains=4,n.iter=4000,n.thin=4)
    print(Sys.time())
    save(mod,file=paste0('splineMod',kkk,'.RData'))
    rm(mod); gc()
}


