library(lme4)
library(arm)
library(parallel)
load('advanceDatawInfo.RData')

### look at promotion
### delete "CP" sections. Logic: these are missing data, since we don't get to see if student
### would have graduated. Student random effects calculated using observed sections. FIML?
advance <- droplevels(subset(advance, status%in%c('graduated','promoted')))

### just look at algebra I sections--- likely different advance patterns in other curricula
### make sure to keep algebra i units that are also part of other curricula
algUnit <- unique(advance$unit[advance$curriculum=='algebra i'])
advance <- subset(advance,unit%in%algUnit)

advance$grad <- advance$status=='graduated'

mod1 <- glmer(grad~grade+race+sex+frl+(xirt+state)*year+esl+spec+(1|field_id)+(1+year|teachid2)+(1|schoolid2)+(1|section/unit),family=binomial,data=advance)

save(mod1,file='mod1.RData')


### can we predict students' random effects from their covariates?
X <- model.matrix(mod1)
Xstud <- aggregate(X,by=list(id=advance$field_id[rownames(X)]),FUN=function(x) x[1])
predEf <- function(id,mod) Xstud[id,]%*%fixef(mod)

bigSchools <- levels(advance$schoolid2)[which(table(advance$schoolid2)>1000)]

predFun <- function(scl){
    newMod <- update(mod1,subset=schoolid2!=scl)
    loIDs <- unique(advance$field_id[advance$schoolid2==scl])
    pred <- cbind(loIDs,predEf(loIDs,newMod))
    save(pred,file=paste0('pred',scl,'.RData'))
    return(pred)
}

predictions <- mclapply(bigSchools,predFun)



save(predictions,file='predictions.RData')
