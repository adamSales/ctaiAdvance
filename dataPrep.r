advance <- droplevels(advance)
dat <- droplevels(dat)


dat$sid <- 1:nrow(dat)


nsecWorked <- nrow(advance)
grad <- advance$grad
sid <- dat$sid
names(sid) <- dat$field_id
studentM <- sid[as.character(advance$field_id)]
nstud <- max(sid)
stopifnot(nstud==length(unique(dat$field_id)))
section <- as.numeric(as.factor(advance$section))
nsec <- max(section)
stopifnot(nsec==length(unique(advance$section)))

X <- model.matrix(~race+sex+spec,data=dat)[,-1]
## add in xirt later


Z <- dat$treatment

cid <- as.numeric(droplevels(as.factor(dat$classid2)))
nclass <- max(cid)
stopifnot(nclass==length(unique(cid)))
Zcls <- vapply(1:nclass,function(cls) mean(Z[cid==cls]),1)

teacher <- as.numeric(as.factor(dat$teachid2))
ntch <- max(teacher)
stopifnot(ntch==length(unique(dat$teachid2)))




Xm <- colMeans(X)

school <- as.numeric(as.factor(dat$schoolid2))
nscl <- max(school)
stopifnot(nscl==length(unique(dat$schoolid2)))

Zscl <- vapply(1:nscl,function(scl) mean(Z[school==scl]),1)

Y <- dat$Y

unit <- as.numeric(as.factor(advance$unit))
unit <- vapply(1:nsec,function(i) unit[section==i][1],1)
nunit <- max(unit)
stopifnot(nunit==length(unique(advance$unit)))

##### trying a second time


### forgot pair fixed effects
pair <- as.numeric(dat$pair)
npair <- max(pair)
stopifnot(npair==length(unique(dat$pair)))

