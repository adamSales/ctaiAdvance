source('src/prelimStan.r')
library(lme4)

rm <- with(sdat,glmer(grad~as.factor(section)+X[studentM,]+(1|studentM),family=binomial))


etaHat <- with(sdat,X%*%fixef(rm)[(nsec+1):(nsec+ncov)])

library(ggplot2)
plotDat <- with(sdat,data.frame(Y=Y,etaHat=etaHat,Z=Z))
p <- ggplot2(plotDat,aes(x=etaHat,y=Y,group=Z,color=Z))+geom_point()+geom_smooth(method='lm',se=FALSE)
p

outMod <- with(sdat,lmer(Y~Z*etaHat+X+(1|teacher)+(1|school)))
