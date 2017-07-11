goodList <- function(sa,params){
    sims.list <- list()
    dn <- dimnames(sa)[[3]]
    n.chain <- dim(sa)[2]
    for(pp in params){
        print(pp)
        ind <- grep(paste0('^',pp),dn)
        if(length(ind)==0) next
        if(length(ind)>1){
            ind <- grep(paste0('^',pp,'\\['),dn)
            if(length(ind)==0) ind <- grep(paste0('^',pp,'$'),dn)
            if(length(ind)==1){
                 for(i in 1:n.chain)
                     sims.list[[pp]] <- c(sims.list[[pp]],sa[,i,ind])
                 next
            }
            sims <- sa[,,ind]
            for(i in 1:n.chain)
                sims.list[[pp]] <- rbind(sims.list[[pp]],sims[,i,])
        } else
            for(i in 1:n.chain)
                sims.list[[pp]] <- c(sims.list[[pp]],sa[,i,ind])
    }
    sims.list$chain <- rep(1:n.chain,each=dim(sa)[1])
    sims.list
}

traceplotSL <- function(sl,param,ask=TRUE){
    sims <- sl[[pmatch(param,names(sl))]]
    n.chains <- max(sl$chain)
    if(!is.null(dim(sims))){
        par(ask=ask)
        for(i in 1:ncol(sims)){
            plot(sims[sl$chain==1,i],type='l',ylim=range(sims[,i]),main=paste0(param,'[',i,']'),col=1)
            for(j in 2:n.chains)
                lines(sims[sl$chain==j,i],col=j)
            legend('bottomleft',legend=seq(n.chains),col=seq(n.chains),lwd=2)
        }
    } else{
        plot(sims[sl$chain==1],type='l',ylim=range(sims),main=param,col=1)
        for(j in 2:n.chains)
            lines(sims[sl$chain==j],col=j)
        legend('bottomleft',legend=seq(n.chains),col=seq(n.chains),lwd=2)
    }
}
