library(lme4)
library(rstan)

load('fittedModels/stanMod.RData')


U <- extract(main,'studEff')[[1]]

multImp <- apply(U[sample(1:nrow(U),1000),],1,
                 function(u) summary(
                                 lmer(Y~treatment*u+poly(xirt,2)+race+sex+spec+state+(1|schoolid2)+(1|teachid2),
                                      data=dat))$coef)

multImp <- t(multImp)
u <- U[nrow(U),]
mod <- lmer(Y~treatment*u+poly(xirt,2)+race+sex+spec+state+(1|schoolid2)+(1|teachid2),data=dat)
ccc <- summary(mod)$coef
colnames(multImp) <- c(rownames(ccc),paste0(rownames(ccc),'SE'),paste0(rownames(ccc),'Tstat'))

print('Est. Effect:')
mean(multImp[,'treatment:u'])

hist(multImp[,'treatment:u'],freq=FALSE)
lines(density(extract(main,'b1')[[1]]))


print('SD of estimated effects')
sd(multImp[,'treatment:u'])

print('multiple imputation SD')
sqrt(var(multImp[,'treatment:u'])+mean(multImp[,'treatment:uSE']^2))

save(multImp,file='fittedModels/multImp.RData')





