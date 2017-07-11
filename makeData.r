### only look at year 2
### and only at students who weren't in  year 1
library(splines)
library(R2jags)
memory.limit(50000)

load('../../data/RANDstudyData/HSdata.RData')
load('../../data/sectionLevelUsageData/advanceData.RData')

lu <- function(x) length(unique(x))

dat <- dat[!dat$field_id%in%dat$field_id[dat$year==1],]
table(dat$year)

dat <- droplevels(dat)
### look at promotion
### delete "CP" sections. Logic: these are missing data, since we don't get to see if student
### would have graduated. Student random effects calculated using observed sections. FIML?
advance <- droplevels(subset(advance, status%in%c('graduated','promoted')))

advance <- advance[advance$field_id%in%dat$field_id[dat$treatment==1],]

### just look at algebra I sections--- likely different advance patterns in other curricula
### make sure to keep algebra i units that are also part of other curricula
algUnit <- unique(advance$unit[advance$curriculum=='algebra i'])
advance <- subset(advance,unit%in%algUnit)

advance$grad <- advance$status=='graduated'

advance <- droplevels(advance)


### discard some pairs
## discard treatment schools with no usage data
## (do a robustness check with everything left in afterwards)
percUse <- function(scl)
    length(intersect(unique(dat$field_id[dat$schoolid2==scl]),unique(advance$field_id)))/
        length(unique(dat$field_id[dat$schoolid2==scl]))

obsUse <- vapply(unique(dat$schoolid2[dat$treatment==1]),percUse,1)

obsUse <- unique(dat$schoolid2[dat$treatment==1])[obsUse>0.1]
obsUse <- c(as.character(obsUse),as.character(unique(dat$schoolid2[dat$treatment==0])))

dat <- dat[dat$schoolid2%in%obsUse,]

## discard pairs with only a treatment or a control school
pairTrtTab <- with(dat,table(pair,treatment))
trtVar <- apply(pairTrtTab,1,prod)
dat <- dat[trtVar[dat$pair]>0,]

advance <- advance[advance$field_id%in%dat$field_id,]

aaa <- aggregate(advance$grad,by=list(section=advance$section),FUN=mean)
aaa$n <- as.vector(table(advance$section))

advance <- subset(advance,section%in%aaa$section[aaa$n>100 & aaa$x<1] & year==2)

advance <- droplevels(advance)
dat <- droplevels(dat)
