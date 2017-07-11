library(lme4)
library(arm)

source('advanceDataCleaning.r')
source('advanceDataPrep.r')

#### check IRT model

samp <- sample(1:max(advID),max(advID)/10)
sub <- which(advID%in%samp)
samp2 <- sample(1:length(sub),length(sub)/10)
sub <- sub[samp2]


colnames(X) <- gsub(' |/|=|\\)|\\(|\\-','',colnames(X))

subdat <- data.frame(mastered=mastered,X=X[advID,],id=advID,sec=sectionAdv)

form <- as.formula(paste('mastered~(1|id)+as.factor(sec)',do.call('paste',as.list(paste('+X.',colnames(X),sep='')))))
mod <- glmer(form,data=subdat[sub,],family=binomial)
print(summary(mod))





aa <- irtMod(mastered,X,advID,sectionAdv,sub)

resids <- mastered[-sub]-aa[[2]]

binnedplot(aa[[2]],mastered[-sub]-aa[[2]])

#coefs <- fixef(aa[[1]])
#names(coefs)[-1] <- colnames(X) ### this won't work anymore :/

#sort(abs(coefs),decreasing=TRUE)


### check role of pretest sem (??)
ptsProb <- X[advID[-sub],'xirt_sem']
binnedplot(ptsProb,resids)
binnedplot(ptsProb,mastered[-sub])


### simpler model

mod2 <- glmer(mastered~(1|id)+as.factor(sec)+X.xirt+X.stateCT +X.stateKY+X.stateLA+X.stateMI+
                 X.stateNJ+X.Igrade9TRUE+ X.raceASIANPACIFICISLANDER +X.raceBLACKNONHISPANIC+
                 X.raceHISPANIC        +     X.raceOTHERRACEMULTIRACIAL+
                 X.raceWHITENONHISPANIC  +   X.sexM    +  X.spec_speced        +      X.spec_gifted   +
                 X.spec_esl        +         X.frl,data=subdat[sub,],family=binomial)


### dang this takes forever
### just try CT students
subCT <- which(X[advID,'stateCT']==1)
subCT <- subCT[sample(1:length(subCT),length(subCT)/4)]
datCT <- subdat[subCT,]
secTab <- table(datCT$sec)
secs <- sort(unique(datCT$sec))[secTab>=5]
datCT <- subset(datCT,sec%in%secs)

mod3 <-  glmer(mastered~(1|id)+as.factor(sec)+X.xirt,data=datCT,family=binomial)
binnedplot(predict(mod3,type='response'),datCT$mastered-predict(mod3,type='response'))
binnedplot(datCT$X.xirt,datCT$mastered-predict(mod3,type='response'))


### full data no fixed effects
mod4 <- glmer(mastered~(1|id)+(1|sec),data=subdat,family=binomial)
save(mod4,file='raschTry831.RData')

mod5 <- update(mod4,.~.+X.xirt)
save(mod5,file='raschPretest92.RData')

### try 2PL model
library(ltm)
library(tidyr)

resp <- spread(subset(subdat,select=c(id,sec,mastered)),key=sec,value=mastered)

irt2pl <- ltm(resp[,-1]~z1)



### try stan
irtDat <- with(subdat,list(
    J=max(id),
    K=max(sec),
    N=nrow(subdat),
    jj=id,
    kk=sec,
    y=mastered))

irt2 <- stan('irt2pt.stan',data=irtDat)



#### meanwhile, while that's running, take a gander at the random effects from the flawed Rasch model
#print(load('raschTry831.RData'))
#Xtrt <- X[


rasch <- lmer(mastered~(1|field_id)+as.factor(section),data=advance)

pdf('raschModelBinnedPlot.pdf')
binnedplot(fitted(rasch),resid(rasch))
dev.off()


### rasch w/X
## fit to half the sample
trtID <- ids[dat$treatment==1 & ids%in%unique(advID)]
#advTrtID <- advID[advID%in%trtID]
#advTrt <- advance[advTrtID,]

samp <- sample(trtID,sum(dat$treatment==1)/2)

advSamp <- advance[advID%in%samp,]
Xsamp <- X[advID[advID%in%samp],]

raschCovs1 <- lmer(mastered~Xsamp+(1|field_id)+(1|section),data=advSamp)
pdf('raschModelWCovsBinnedPlot.pdf')
binnedplot(fitted(raschCovs1),resid(raschCovs1))
dev.off()


ability1 <- fixef(raschCovs1)[1]+X[samp,]%*%fixef(raschCovs1)[-1]+ranef(raschCovs)[[1]][[1]]



notSamp <- setdiff(trtID,samp)
advTest <- advance[advID%in%notSamp,]
Xtest <- X[advID[advID%in%notSamp],]

ability2Pred <- fixef(raschCovs1)[1]+X[notSamp,]%*%fixef(raschCovs1)[-1]


raschCovs2 <- lmer(mastered~Xtest+(1|field_id)+(1|section),data=advTest)
ability2 <- fixef(raschCovs2)[1]+X[notSamp,]%*%fixef(raschCovs2)[-1]+ranef(raschCovs2)[[1]][[1]]

pdf('residAbility.pdf')
plot(ability2Pred,ability2-ability2Pred,xlab='out of sample ability predictions (XBeta_1)',ylab='ability measurements (XBeta_2+ranef)')
abline(h=0)
abline(h=mean(ability2-ability2Pred))
dev.off()


Xmod <- X[trtID,]



#### half-dat model checking
datT <- dat[dat$treatment==1,]
trtSchools <-
