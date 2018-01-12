library(rstan)
library(dplyr)

pload('fittedModels/stanMod.RData')

mbar <- aggregate(sdat$grad,list(sdat$studentM),mean)

#mbar <- mbar$x[match(seq(sdat$nstud),mbar$Group.1)]
###
draws <- extract(main,c('studEff','secEff'))


avgDiff <- vapply(mbar$Group.1,
                  function(s)
                      rowMeans(cbind(draws$secEff[,sdat$section[sdat$student==s]])),
                  numeric(nrow(draws$secEff)))


mbarCor <- apply(avgDiff,1,function(ad) cor(ad,mbar$x,method='spearman',use='pairwise'))

etaObs <- draws$studEff[,mbar$Group.1]

etaCor <- vapply(seq(nrow(etaObs)),function(i) cor(etaObs[i,],avgDiff[i,],method='spearman',use='pairwise'),1)
