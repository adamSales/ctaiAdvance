source('src/prelimStan.r')

#### first main model
main <- stan('src/psmod.stan',data=sdat,warmup=1500,chains=10,iter=5000)

cat('\n\n\n\n',rep('-',40),'\n','MAIN MODEL',rep('-',40),'\n\n\n')
print(main,c('a0','a1','b0','b1'),c(0.05,0.95))

save(main,sdat,file='fittedModels/stanMod.RData'); rm(main); gc();

### interactions model
sdatInt <- makeStanDat(dat,advance,xInteract=TRUE)
xint <- stan('src/psmod.stan',data=sdatInt,warmup=1500,iter=5000,chains=4)
cat('\n\n\n\n',rep('-',40),'\n','INTERACTIONS',rep('-',40),'\n\n\n')
print(xint,c('a0','a1','b0','b1'),c(0.05,0.95))
save(xint,sdatInt,file='fittedModels/xInteractions.RData'); rm(xint); gc()

### only sections with hints
source('src/hardSections.r')
cat('\n\n\n\n',rep('-',40),'\n','HARD SECTIONS',rep('-',40),'\n\n\n')
print(hard,c('a0','a1','b0','b1'),c(0.05,0.95))
rm(hard);gc()

### no teacher
noTeach <- stan('src/psmodNoTeacher.stan',data=sdat,warmup=1500,iter=3000,chains=4)
cat('\n\n\n\n',rep('-',40),'\n','NO TEACHER FX',rep('-',40),'\n\n\n')
print(noTeach,c('a0','a1','b0','b1'),c(0.05,0.95))
save(noTeach,sdat,file='fittedModels/modNoTeacher.RData'); rm(noTeach); gc()

### pooled usage data
stanModPooledU <- stan('src/pooledU.stan',data=sdat,warmup=1500,iter=3000,chains=4)
cat('\n\n\n\n',rep('-',40),'\n','U Pooled Model',rep('-',40),'\n\n\n')
print(pooledU,c('a0','a1','b0','b1'),c(0.05,0.95))
save(pooledU,sdat,file='fittedModels/pooledU.RData'); rm(pooledU); gc()

## 2PL
stanMod2pl <- stan('src/psmod2pl.stan',data=sdat,warmup=1500,iter=5000,chains=4)
cat('\n\n\n\n',rep('-',40),'\n','2PL Model',rep('-',40),'\n\n\n')
print(stanMod2pl,c('a0','a1','b0','b1'),c(0.05,0.95))
save(stanMod2pl,sdat,file='fittedModels/stanMod2pl.RData'); rm(stanMod2pl); gc()

## 3PL
stanMod3pl <- stan('src/psmod3pl.stan',data=sdat,warmup=1500,iter=5000,chains=4)
cat('\n\n\n\n',rep('-',40),'\n','3PL Model',rep('-',40),'\n\n\n')
print(stanMod3pl,c('a0','a1','b0','b1'),c(0.05,0.95))
save(stanMod3pl,sdat,file='fittedModels/stanMod3pl.RData'); rm(stanMod3pl); gc()

## mbarModel
source('gitRepos/ctaiAdvance/prelimStanObs.r')
mbarMod <- stan('~/gitRepos/ctaiAdvance/psmodObs.stan',data=sdatObs); save(mbarMod,sdatObs,file='fittedModels/mbarModel.RData')
print(mbarMod,c('a0','a1','b0','b1'),c(0.05,0.95))
rm(mbarMod); gc()


