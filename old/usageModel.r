### run usage model in jags and / or stan
### first run advanceDataCleaning.r and advanceDataPrep.r

source('src/advanceDataCleaning.r')
source('src/advanceDataPrep.r')



## jags
library(R2jags)

jags.data <- makeData(dat,advance,jags=TRUE)

jags.params <- c('ability','pretest','betaAb','b.x1','betaPre')

mod <- jags(jags.dat,parameters=jags.params,model='usageModel.bug',n.iter=5)


### stan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = 12)



stanDat <- makeData(dat,advance,jags=FALSE)

mods <- stan('usageModel.stan',iter=5000,thin=5,data=stanDat)
save(mods,file='stanUsageMod.RData')

trtSchools <- unique(dat$schoolid2[dat$treatment==1])
set.seed(10)
trainSchools <- sample(trtSchools,floor(length(trtSchools)/2))
testSchools <- setdiff(trtSchools,trainSchools)

datTrain <- subset(dat,schoolid2%in%trainSchools)
advanceTrain <- subset(advance,field_id%in%datTrain$field_id)

stanDatTrain <- makeData(dat,advanceTrain,FALSE)

modStanHalf <- stan('usageModel.stan',iter=5000,thin=5,data=stanDatTrain)
save(modStanHalf,file='halfMod.RData')



###try in jags
library(R2jags)
jdat <- makeData(dat,advance)
modj <- jags.parallel(jdat,parameters=c('prob','avgAb','ability','diff','unitEffect','betaAb','betaPre','bx','teacherEffectAb','teacherEffectPre'),model='src/usageModel.bug',n.chains=4,n.iter=10000)
