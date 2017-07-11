source('src/prelim.r')
#############################################
## Real Models
#######################################
jagsDatReal <- makeJagsDat(dat,advance)

#realModels <- list()

print(Sys.time())
## lin <- jags.parallel(jagsDatReal,parameters=params,model.file='src/psmod.bug',n.chains=4,n.iter=100000,n.thin=50)
## print(Sys.time())

## save(lin,jagsDatReal,file=paste0('~/Google Drive/CTmodels/realModels/lin',Sys.Date(),'.RData'))
## rm(lin);gc()

#### robustness checks
print(Sys.time())
datBC <- dat
rawdat <- read.csv('../../data/RANDstudyData/H2_algebra_rcal_20121119_fieldid.csv')
datBC$Y <- vapply(datBC$field_id, function(id) rawdat$t2score[rawdat$field_id==id]+1,1)
jagsDatBC <- makeJagsDat(datBC,advance)
trans <- with(jagsDatBC,MASS::boxcox(Y[Z==1]~X[Z==1,]+U))
lambda <- trans$x[which.max(trans$y)]
if(lambda !=0) jagsDatBC$Y <- (jagsDatBC$Y^lambda-1)/lambda else jagsDatBC$Y <- log(jagsDatBC$Y)
bc <- jags.parallel(jagsDatBC,parameters=params,model.file='src/psmod.bug',n.chains=4,n.iter=10000,n.thin=5)
print(Sys.time())

save(bc,jagsDatReal,file=paste0('~/Google Drive/CTmodels/realModels/bc',Sys.Date(),'.RData'))
rm(bc);gc()



jagsDatReal2 <- jagsDatReal
jagsDatReal2$X <- scale(model.matrix(~(poly(xirt,2)+race+sex+spec)^2+state,data=dat)[,-1])
jagsDatReal2$ncovar <- ncol(jagsDatReal2$X)
saturated <- jags.parallel(jagsDatReal2,parameters=params,model.file='src/psmod.bug',n.chains=4,n.iter=10000,n.thin=5)
save(saturated,jagsDatReal2,file=paste0('~/Google Drive/CTmodels/realModels/saturated',Sys.Date(),'.RData'))
rm(saturated);gc()

print(Sys.time())
covs$Y <- datOrig$Y
totCCA <- dataPrep(na.omit(covs[,c('field_id','year','treatment','race','sex','xirt','Y','state','teachid2','schoolid2','pair','spec')]),advanceOrig)
advanceCCA <- totCCA$advance
datCCA <- totCCA$dat
jagsDatCCA <- makeJagsDat(datCCA,advanceCCA)

cca <- jags.parallel(jagsDatCCA,parameters=params,model.file='src/psmod.bug',n.chains=4,n.iter=10000,n.thin=5)

save(cca,jagsDatCCA,file=paste0('~/Google Drive/CTmodels/realModels/cca',Sys.Date(),'.RData'))
rm(cca);gc()

### keep all schools, all software sections
fullData <- dataPrep(dat,advance,discard=FALSE)
jagsDatReal3 <- makeJagsDat(fullData$dat,fullData$advance)
full <- jags.parallel(jagsDatReal3,parameters=params,model.file='src/psmod.bug',n.chains=4,n.iter=10000,n.thin=5)
save(full,jagsDatReal3,file=paste0('~/Google Drive/CTmodels/realModels/full',Sys.Date(),'.RData'))
rm(full); gc()


