library(lme4)
library(rstan)

load('fittedModels/stanMod.RData')


U <- extract(main,'studEff')[[1]]

multImp <- apply(U[sample(1:nrow(U),1000),],1,
                 function(u) summary(
                                 lmer(Y~treatment*u+poly(xirt,2)+race+sex+spec+state+(1|schoolid2)+(1|teachid2),
                                      data=dat))$coef)


save(multImp,file='fittedModels/multImp.RData')



