### simulation to explore model if Mbar is totally observed
### based on fitted Mbar model

library(rstan)
library(lme4)

load('manifestModel.RData')
mbar <- extract(mod,c('MbarTM','MbarC'))


source('src/prelimStanObs.r')

sdatFake <- sdat
sdatFake$MbarTM <- colMeans(mbar$MbarTM)
sdatFake$MbarC <- colMeans(mbar$MbarC)


### using lmer
datFake <- dat
datFake$mbar[datFake$treatment==0] <- sdatFake$MbarC
datFake$mbar[is.na(datFake$mbar) & datFake$treatment==1] <- sdatFake$MbarTM

modF1 <- lmer(Y~poly(xirt,2)+race+sex+spec+state+mbar*treatment+(1|teachid2)+(1|schoolid2),data=datFake)
