source('src/prelimStan.r')
library(lme4)
library(ggplot2)

rm <- with(sdat,glmer(grad~as.factor(section)+X[studentM,]+(1|studentM),family=binomial))


etaHat <- with(sdat,X%*%fixef(rm)[(nsec+1):(nsec+ncov)])

library(ggplot2)
plotDat <- with(sdat,data.frame(Y=Y,etaHat=etaHat,Z=Z))
p <- ggplot(plotDat,aes(x=etaHat,y=Y,group=Z,color=Z))+geom_point()+geom_smooth(method='lm',se=FALSE)
p

outMod <- with(sdat,lmer(Y~Z*etaHat+X+(1|teacher)+(1|school)))

### m-bar version
source('src/prelimStanObs.r')

mMod <- lmer(mbar~poly(xirt,2)+race+sex+spec+state+(1|teachid2)+(1|schoolid2),data=dat)
dat$mhat <- predict(mMod,dat,allow=TRUE)
dat$mimp <- ifelse(is.na(dat$mbar),dat$mhat,dat$mbar)
yMod <- lmer(Y~treatment*mimp+poly(xirt,2)+race+sex+spec+state+pair+(1|teachid2)+(1|schoolid2),data=dat)

p <- ggplot(dat,aes(x=mimp,y=Y,group=treatment,color=treatment))+geom_point()+geom_smooth(method='lm',se=FALSE)+labs(xlab='$\\bar{m}$ (Imputed for Controls)',ylab='Posttest Score',color=NULL)

