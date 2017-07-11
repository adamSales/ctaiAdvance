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

load('lpm.RData')

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
    loIDs <- unique(advance$field_id[advance$schoolid2==scl])
    pred <- cbind(loIDs,predEf(loIDs,newMod))
    save(pred,file=paste0('pred',scl,'.RData'))
    return(pred)
}


### load predictions
preds <- list()
fls <- list.files()
schls <- fls[grep('predS[0-9]+.RData',fls)]
for(scl in schls){
    load(scl)
    preds[[scl]] <- pred
}

eff <- ranef(mod2)$field_id

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



