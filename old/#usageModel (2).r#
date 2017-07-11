library(lme4)
library(parallelsugar)
if(exists('.Random.seed')) rm(.Random.seed)

library(arm)
load('~/Box Sync/CT/data/sectionLevelUsageData/advanceDatawInfo.RData')

### look at promotion
### delete "CP" sections. Logic: these are missing data, since we don't get to see if student
### would have graduated. Student random effects calculated using observed sections. FIML?
advance <- droplevels(subset(advance, status%in%c('graduated','promoted')))

### just look at algebra I sections--- likely different advance patterns in other curricula
### make sure to keep algebra i units that are also part of other curricula
algUnit <- unique(advance$unit[advance$curriculum=='algebra i'])
advance <- subset(advance,unit%in%algUnit)

advance$grad <- advance$status=='graduated'

## mod1 <- glmer(grad~grade+race+sex+frl+(xirt+state)*year+esl+spec+(1|field_id)+(1+year|teachid2)+(1|schoolid2)+(1|section/unit),family=binomial,data=advance)

## binnedplot(predict(mod1,type='response'),resid(mod1,type='response'))

mod2 <- lmer(grad~(grade+race+sex+frl+xirt+state+esl+spec)*year+(1|field_id)+(1+year|teachid2)+(1|schoolid2)+(1+year|section)+(1|unit),data=advance)

binnedplot(predict(mod2,type='response'),resid(mod2,type='response'))

save(mod2,file='lpm.RData')

### can we predict students' random effects from their covariates?
X <- model.matrix(mod2)
predEf <- function(id,mod) X[id,]%*%fixef(mod)


ids <- advance[rownames(X),]$field_id
Xstud <- aggregate(X,by=list(id=ids),FUN=function(x) x[1])
rownames(Xstud) <- Xstud$id
Xstud <- as.matrix(Xstud[,-1])

predEf <- function(id,mod) Xstud[as.character(id),names(fixef(mod))]%*%fixef(mod)



bigSchools <- levels(advance$schoolid2)[which(table(advance$schoolid2)>1000)]

predFun <- function(scl){
    newMod <- update(mod2,data=advance[advance$schoolid2!=scl,])
    save(newMod,file=paste0('mod',scl,'.RData'))
    loIDs <- unique(advance$field_id[advance$schoolid2==scl])
    pred <- cbind(loIDs,predEf(loIDs,newMod))
    save(pred,file=paste0('pred',scl,'.RData'))
    return(pred)
}


if(exists('.Random.seed')) rm(.Random.seed)
predictions <- mclapply(bigSchools,predFun,mc.cores=14)
save(predictions,file='outOfSampleStudentEffects.RData')

comp <- function(pred){
    err <- eff[rownames(pred),1]
    fit <- Xstud[rownames(pred),]%*%fixef(mod2)
    full <- fit + err
    out <- cbind(pred=pred[,2],fit=fit,full=full,err=err)
    colnames(out) <- c('pred','fit','full','err')
    out
}

compare <- lapply(predictions,comp)
compare <- do.call('rbind',compare)

pdf('extrapolatingLPM.pdf')
plot(compare[,c('pred','full')],xlab='LOO predictions',ylab='Random Effect from Full Model')
abline(0,1)
abline(lm(compare[,'full']~compare[,'pred']),col='red')
legend('bottomright',legend=c('y=x','regression fit'),col=c('black','red'),lty=1)

plot(compare[,'pred'],compare[,'pred']-compare[,'full'],xlab='LOO Predictions',ylab='Prediction Errors')
abline(h=0)

qqnorm(compare[,'pred']-compare[,'full'])
qqline(compare[,'pred']-compare[,'full'])

hist(compare[,'pred']-compare[,'full'])
dev.off()

summary(lm(compare[,'full']~compare[,'pred']))
