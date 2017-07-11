

noEffect <- function(mod,jagsDat){
    jagsDat <- within(jagsDat,{
        datID <- 1:nrow(X)
#### delete control data
        X <- X[Z==1,]
        cid <- cid[Z==1]
        school <- school[Z==1]
        Y <- Y[Z==1]
        pair <- pair[Z==1]
        datID <- datID[Z==1]
        Z <- Z[Z==1]

        newID <- 1:nrow(X)
        names(newID) <- datID

### create new data
        X <- rbind(X,X)
        Z <- c(Z,1-Z)
        Y <- c(Y,Y)
        pair <- c(pair,pair)



        cid <- c(cid,cid*100+99)
        cid <- as.numeric(as.factor(cid))

        school <- c(school,school*100+99)
        school <- as.numeric(as.factor(school))

        studentM <- newID[as.character(studentM)]

        nstud <- nrow(X)
        nscl <- max(school)

        nclass <- max(cid)

        Zscl <- vapply(1:nscl,function(scl) mean(Z[school==scl]),1)
        Zcls <- vapply(1:nclass,function(cls) mean(Z[cid==cls]),1)

    })

    sink('mod.bug')
    cat(mod$model)
    sink()

    params <- list(data=jagsDat,parameters=mod$parameters,model.file='mod.bug',nchains=mod$BUGSoutput$n.chains,
         n.iter=mod$n.iter,n.thin=mod$BUGSoutput$n.thin )
    rm(mod,envir=.GlobalEnv)
    do.call('jags',params)
    }
