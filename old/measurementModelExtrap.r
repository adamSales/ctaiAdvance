library(lme4)

year2 <- TRUE
source('src/advanceDataCleaning.r')

stanDat <- makeData(dat[dat$treatment==1,],advance,FALSE)

comp <- with(stanDat,data.frame(comp=complied,id=advID,section=sectionAdv,X=X[advID,],teacher=teacher[advID],school=school[teacher[advID]]))

comp$xirt <- dat$xirt[stanDat$advID]



set.seed(10)
fakeTrt <- sample(unique(comp$school),12)

comp$Z <- comp$school%in%fakeTrt

mod1 <- glmer(comp~(1|id)+(1|school)+(1|section)+X.stateCT+X.stateLA+X.stateMI+X.stateNJ+X.I.grade....9.TRUE+X.raceBlackMulti+X.raceHispAIAN+X.sexM+X.specspeced+X.specgifted+X.esl1+X.frl1,data=comp,family=binomial,subset=Z==1)

mod2 <- glmer(comp~(1|id)+(1|school)+(1|section)+X.stateCT+X.stateLA+X.stateMI+X.stateNJ+X.I.grade....9.TRUE+X.raceBlackMulti+X.raceHispAIAN+X.sexM+X.specspeced+X.specgifted+X.esl1+X.frl1,data=comp,family=binomial,subset=Z==0)


Zstud <- stanDat$school[stanDat$teacher]%in%fakeTrt
par(mfrow=c(1,2))
plot(mod1Fit[Zstud],mod2Fit[Zstud])
abline(0,1)
plot(mod2Fit[!Zstud],mod1Fit[!Zstud])
abline(0,1)


par(mfrow=c(1,2))
plot(mod1Fit[Zstud],mod2Fit[Zstud]-mod1Fit[Zstud])
abline(h=0,lty=2)
plot(mod2Fit[!Zstud],mod1Fit[!Zstud]-mod2Fit[!Zstud])
abline(h=0,lty=2)
