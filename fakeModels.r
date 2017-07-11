source('src/prelim.r')

################################################
## Fake Models
##############################################

### first delete control schools
datF <- subset(dat,treatment==1)

### now double the dataset
datF$schoolid2 <- as.character(datF$schoolid2)
datF$teachid2 <- as.character(datF$teachid2)
dat2 <- datF
dat2$schoolid2 <- paste0(dat2$schoolid2,'Fake')
dat2$teachid2 <- paste0(dat2$teachid2,'Fake')
dat2$field_id <- dat2$field_id*100+99
dat2$treatment <- 0
datF <- rbind(datF,dat2)

### now delete usage data for "control" group
advanceF <- advance[advance$field_id%in%datF$field_id[datF$treatment==1],]

#########################################################
### Compile JAGS data as before
#####################################################

jagsDatF <- makeJagsDat(datF,advanceF)


print(Sys.time())
noEff <- jags.parallel(jagsDatF,parameters=params,model.file='src/psmod.bug',n.chains=4,n.iter=10000,n.thin=4)


print(Sys.time())
save(list=ls(),file=paste0('~/Google Drive/CTmodels/fakeModels/noEff',Sys.Date(),'.RData'));rm(noEff);gc()




######## constant effect
########
jagsDatFConst <- within(jagsDatF,{
 te <- rnorm(sum(Z),0.18,0.1)
 Y[Z==1] <- Y[Z==1]+ te[Z==1]
 }
)

print(Sys.time())
constEff <- jags.parallel(jagsDatFConst,parameters=params,model.file='src/psmod.bug',n.chains=4,n.iter=10000,n.thin=4)

save(list=ls(),file=paste0('~/Google Drive/CTmodels/fakeModels/constEff',Sys.Date(),'.RData'));rm(constEff);gc()



##################################
######### linear effect ##########
##################################


### Add linear treatment effect
                                        #load('logitUsage/mod1.RData')
## same mean and var of treatment effect as in mod2

### U has a couple outliers:
U[U< -4] <- -4

jagsDatFLin <- within(jagsDatF,{
  te <- 0.1/sd(U,na.rm=TRUE)*U
  te <- te-mean(te)+0.18

  Y[Z==1] <- Y[Z==1]+te
 }
)


print(Sys.time())
linEff <- jags.parallel(jagsDatFLin,parameters=params,model.file='src/psmod.bug',n.chains=4,n.iter=4000,n.thin=4)
save(list=ls(),file=paste0('~/Google Drive/CTmodels/fakeModels/linEff',Sys.Date(),'.RData'));rm(linEff);gc()


########### quadratic effects
jagsDatFQuad <- within(jagsDatF,{
  te <- -(U-mean(U))^2
  te <- te/sd(te)*0.1
  te <- te-mean(te)+0.18
  Y[Z==1] <- Y[Z==1]+te
 }
)

print(Sys.time())
quadEff <- jags.parallel(jagsDatFQuad,parameters=params,model.file='src/psmod.bug',n.chains=4,n.iter=4000,n.thin=4)
print(Sys.time())
save(list=ls(),file=paste0('~/Google Drive/CTmodels/fakeModels/quadEff',Sys.Date(),'.RData'));rm(quadEff);gc()

