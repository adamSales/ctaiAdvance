### simulation to explore model if Mbar is totally observed
### based on fitted Mbar model

library(rstan)
library(lme4)



load('fittedModels/stanMod.RData')
mbar <- colMeans(extract(main,'studEff')[[1]])
mainSumm <- summary(main)
rm(main); gc()

source('~/gitRepos/ctaiAdvance/prelimStanObs.r')

sdatFake <- sdat
sdatFake$MbarTO <- mbar[dat$treatment==1 & !is.na(dat$mbar)]
sdatFake$MbarTM <- mbar[dat$treatment==1 & is.na(dat$mbar)]
sdatFake$MbarC <- mbar[dat$treatment==0]


### using lmer
datFake <- dat
datFake$mbar <- mbar

modF1 <- lmer(Y~poly(xirt,2)+race+sex+spec+state+mbar*treatment+(1|teachid2)+(1|schoolid2),data=datFake)

modF2 <- stan('~/gitRepos/ctaiAdvance/psmodTotObs.stan',data=sdatFake)

save(modF1,modF2,datFake,sdatFake,file='fittedModels/totObsMods.RData')
