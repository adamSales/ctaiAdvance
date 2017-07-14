plotEff <- function(mod,te,U,leg=TRUE,pos='topright',file){
    if(!missing(file)) pdf(file)
    trtEff <- mod$BUGSoutput$sims.list$trtEff
    studEff <- mod$BUGSoutput$sims.list$studEff
    b0 <- mod$BUGSoutput$sims.list$b0
    b1 <- mod$BUGSoutput$sims.list$b1

    samp <- sample(1:nrow(trtEff),200)
    studEff95 <- quantile(studEff,c(0.025,0.975))
    plot(studEff[1,],trtEff[1,],col='white',ylim=range(trtEff[studEff<studEff95[2] & studEff>studEff95[1]]),
         xlim=studEff95,xlab=expression(eta[T]),ylab='Treatment Effect')
    sapply(samp,function(rr) abline(b0[rr],b1[rr],col=adjustcolor('red',.3)))

    if(!missing(te)){
        if(!missing(U)){
            U <- U-mean(U)+mean(studEff)
            U <- U/sd(U)*sqrt(mean(apply(studEff,1,var)))
            lines(sort(U),te[order(U)],lwd=2)
        } else abline(h=mean(te,na.rm=TRUE),lwd=2)
        if(leg) legend(pos,legend=c('True Effect','MCMC Draws'),lty=1,col=c('black','red'))
    } else{
        abline(mean(b0),mean(b1),lwd=2)
        if(leg) legend(pos,legend=c('Mean Est. Effect','MCMC Draws'),lty=1,col=c('black','red'))
    }
    if(!missing(file)) dev.off()
    invisible(NULL)
}

### sample size figure

sampleSizeFig <- function(jagsDat,mod){
    attach(jagsDat)
    attach.jags(mod)
 mbar <- aggregate(grad,by=list(studentM),FUN=function(x) c(mean(x),length(x)))
 mbar$mbar <- mbar$x[,1]
 mbar$nsec <- mbar$x[,2]
 Eeta <- colMeans(studEff)
 pdf('output/sampleSizeMbar.pdf',height=2,width=6.5)
 par(mfrow=c(1,3),mar=c(5,4,1,2)+0.1)
 plot(mbar$nsec,mbar$mbar,ylab=expression(bar(m)),xlab='Number of Worked Sections',sub='(A)')
 plot(mbar$nsec,Eeta[mbar$Group.1],xlab='Number of Worked Sections',ylab=expression(paste('E',eta[T])),sub='(B)')
 plot(mbar$nsec,apply(studEff,2,sd)[mbar$Group.1],xlab='Number of Worked Sections',ylab=expression(paste('SD(',eta[T],')',sep='')),sub='(C)')
 dev.off()

 #### studEff vs m-bar
    mbar$Eeta <- Eeta[mbar$Group.1]
    mbar$nsec2 <- ifelse(mbar$nsec<50,mbar$nsec,50)
    p <- ggplot2::qplot(Eeta,mbar, size=nsec2,data=mbar,ylab=expression(bar(m)),
                                       xlab=expression(paste('E',eta[T],sep='')),
                        main=paste0('spearman rho=',round(with(mbar,cor(x[,1],Eeta,method='spearman')),2)))+
        ggplot2::scale_size('# Sections',breaks=c(1,5,25,50),labels=c(1,5,25,'50+'),range=c(1,3))
    ggplot2::ggsave(file='output/mbarVsEta.pdf')
    detach(jagsDat)
}


############ plot EY|eta
potentialOutcomesPlot <- function(mod){
 pdf('output/potentialOutcomes.pdf')
 a0 <- mod$BUGSoutput$sims.list$a0
 a1 <- mod$BUGSoutput$sims.list$a1
 b0 <- mod$BUGSoutput$sims.list$b0
 b1 <- mod$BUGSoutput$sims.list$b1

 curve(mean(a0)+mean(a1)*x,from=-3, to=3,ylim=c(-1.05,0.25),lwd=2,col='red',xlab=expression(eta[T]),ylab=expression(paste('E[',Y[Z],'|',eta[T],']',sep='')),xlim=c(-3,3))
 curve(mean(b0)+mean(a0)+(mean(b1)+mean(a1))*x,add=TRUE,lwd=2,col='blue')

 x <- seq(-3,3,length=100)
 Yc <- outer(a1,x)[,1,]
 Yc <- apply(Yc,2,function(rr) rr+a0)
 YcUp <- apply(Yc,2,function(x) quantile(x,0.75))
 YcDown <- apply(Yc,2,function(x) quantile(x,0.25))
 polygon(c(x,rev(x)),c(YcUp,rev(YcDown)),col=adjustcolor('red',0.1))

 Yt <- outer(a1+b1,x)[,1,]
 Yt <- apply(Yt,2,function(rr) rr+a0+b0)
 YtUp <- apply(Yt,2,function(x) quantile(x,0.75))
 YtDown <- apply(Yt,2,function(x) quantile(x,0.25))
 polygon(c(x,rev(x)),c(YtUp,rev(YtDown)),col=adjustcolor('blue',0.1))

 legend('topleft',legend=c(expression(Y[C]),expression(Y[T])),col=c('red','blue'),lwd=2)
 dev.off()
}

stdPlot <- function(mod,jagsDatReal){
    attach.jags(mod)
    Z <- jagsDatReal$Z
    ett <- median(apply(trtEff,1,mean))
    iqr <-apply(studEff,1,IQR)
    b1std <- b1*iqr/ett

    pdf('output/b1stdDens.pdf')
    plot(density(b1std),main='',sub='')
    dev.off()
}

errbar <- function (x, y, yplus, yminus, cap = 0.015, main = NULL, sub = NULL,
    xlab = as.character(substitute(x)), ylab = if (is.factor(x) ||
        is.character(x)) "" else as.character(substitute(y)),
    add = FALSE, lty = 1, type = "p", ylim = NULL, lwd = 1, pch = 16,
    errbar.col = par("fg"), Type = rep(1, length(y)), ...)
{
    if (is.null(ylim))
        ylim <- range(y[Type == 1], yplus[Type == 1], yminus[Type ==
            1], na.rm = TRUE)
    if (is.factor(x) || is.character(x)) {
        x <- as.character(x)
        n <- length(x)
        t1 <- Type == 1
        t2 <- Type == 2
        n1 <- sum(t1)
        n2 <- sum(t2)
        omai <- par("mai")
        mai <- omai
        mai[2] <- max(strwidth(x, "inches")) + 0.25
        par(mai = mai)
        on.exit(par(mai = omai))
        plot(NA, NA, xlab = ylab, ylab = "", xlim = ylim, ylim = c(1,
            n), axes = FALSE, ...)
        axis(1)
        w <- if (any(t2))
            n1 + (1:n2) + 1
        else numeric(0)
        axis(2, at = c(seq.int(length.out = n1), w), labels = c(x[t1],
            x[t2]), las = 1, adj = 1)
        points(y[t1], seq.int(length.out = n1), pch = pch, type = type,
            ...)
        segments(yplus[t1], seq.int(length.out = n1), yminus[t1],
            seq.int(length.out = n1), lwd = lwd, lty = lty, col = errbar.col)
        if (any(Type == 2)) {
            abline(h = n1 + 1, lty = 2, ...)
            offset <- mean(y[t1]) - mean(y[t2])
            if (min(yminus[t2]) < 0 & max(yplus[t2]) > 0)
                lines(c(0, 0) + offset, c(n1 + 1, par("usr")[4]),
                  lty = 2, ...)
            points(y[t2] + offset, w, pch = pch, type = type,
                ...)
            segments(yminus[t2] + offset, w, yplus[t2] + offset,
                w, lwd = lwd, lty = lty, col = errbar.col)
            at <- pretty(range(y[t2], yplus[t2], yminus[t2]))
            axis(side = 3, at = at + offset, labels = format(round(at,
                6)))
        }
        return(invisible())
    }
}


usageCoefPlot <- function(mod,dat){
    pdf('output/usageCoef.pdf')

    coefs <- jagsresults(mod,'betaU')
    X2 <- model.matrix(~poly(xirt,2)+race+sex+spec+state,data=dat)[,-1]

    sdEta <- sqrt(mean(apply(mod$BUGSoutput$sims.list$studEff,1,var)))

    coefs <- coefs/apply(X2,2,sd)/sdEta

    par(mar=c(5,5,0.5,0.5)+0.1)
    errbar(x=c('Black/Multiracial','Hispanic/Native American','Male','Special Ed.','Gifted'),coefs[3:7,1],coefs[3:7,'2.5%'],coefs[3:7,'97.5%'],ylab=expression(hat(beta)[std]))
     segments(coefs[3:7,'25%'],1:5, coefs[3:7,'75%'],1:5, lwd = 2)
    abline(v=0,lty=2)
    dev.off()
}

pretestEtaPlot <- function(mod,dat){
 pdf('output/pretestEta.pdf')
 attach.jags(mod)
 sdEta <- sqrt(mean(apply(studEff,1,var)))
 plot(dat$xirt,colMeans(studEff)/sdEta,col=ifelse(dat$treatment==1,'blue','red'),
     xlab='Pretest (std)',ylab=expression(paste('E[',eta[T],'|x]')),)

 X <- scale(model.matrix(~poly(xirt,2),data=dat)[,-1])
 xpred <- (X[,1]*mean(betaU[,1])+X[,2]*mean(betaU[,2]))/sdEta
 samp <- sample(1:4000,100)
 for(ss in samp){
  xpredS <- (X[,1]*betaU[ss,1]+X[,2]*betaU[ss,2])/sdEta
  lines(sort(dat$xirt),xpredS[order(dat$xirt)],col=adjustcolor('pink',0.5))
 }
 lines(sort(dat$xirt),xpred[order(dat$xirt)],lwd=2)

 legend('bottomright',legend=c('Treated','Control (Imputed)','Model (Avg.)','Model (draws)'),col=c('blue','red','black','pink'),pch=c('o','o','.','.'),lwd=c(0.01,0.01,2,2))
 dev.off()
 detach.jags()
}

robustnessChecks <- function(){
    estimates <- NULL
    modFiles <- list.files('~/Google Drive/CTmodels/realModels/')
    for(nm in c('main','bc','saturated','full','cca','raw')){
        print(nm)
        file <- max(grep(nm,modFiles,value=TRUE))
        load(paste0('~/Google Drive/CTmodels/realModels/',file))
        mod <- get(nm)
        est <- quantile(mod$BUGSoutput$sims.list$b1,c(0.5,0.25,0.75,0.025,0.975))
        if(nm=='bc') est <- est/sd(jagsDatBC$Y)*sd(jagsDatReal$Y)
        if(nm=='raw') est <- est/sd(jagsDatRAW$Y)*sd(jagsDatReal$Y)
        estimates <- rbind(estimates,est)
        rownames(estimates)[nrow(estimates)] <- nm
        rm(list=c(nm,'mod'))
    }
    estimates
}

robustnessPlot <- function(){
 pdf('output/robustness.pdf')
 estimates <- robustnessChecks()
 save(estimates,file='output/robustnessCheckEstimates.RData')

 estimates <- estimates[c('raw','bc','cca','full','saturated','main'),]

 par(mar=c(4,5,0,0))
 errbar( c('Raw Scores','BC-transformed','Complete Cases','Full Data','Covariate Interactions','Main'),estimates[,1],estimates[,3], estimates[,2],ylim=c(min(estimates)-0.01,max(c(max(estimates),0.03))),ylab=expression(paste('Slope of ',tau,'(',eta[T],')')),lwd=2)
 segments(estimates[,5],1:6, estimates[,4],1:6, lwd = 1)

 abline(v=0,lty=2)
 dev.off()
}

plotFakeOne <- function(modName){
    modFiles <- print(list.files('~/Google Drive/CTmodels/fakeModels/'))

    print(modName)
    print(file <- max(grep(modName,modFiles,value=TRUE)))
    load(paste0('~/Google Drive/CTmodels/fakeModels/',file))
    mod <- get(modName)
    if(modName=='noEff'){
        plotEff(mod,te=0,file='output/noEff.pdf')
    }else{
        dat <- get(paste0('jagsDatF',toupper(substr(modName,1,1)),substr(modName,2,nchar(modName)-3)))
        if(modName=='constEff'){
            plotEff(mod,te=dat$te,file=paste0('output/',modName,'.pdf'))
        } else plotEff(mod,te=dat$te,U=U,file=paste0('output/',modName,'.pdf'))
    }

    attach.jags(mod)
    out <- c(mean(b0),sd(b0),mean(b1),sd(b1))
    detach.jags()
    rm(list=c(modName,'mod'))
    return(out)
}

plotFake <- function(){
    fake <- NULL
    for(modName in c('noEff','constEff','linEff','quadEff')){
        fake <- cbind(fake,plotFakeOne(modName))
    }
    fake <- round(fake,2)
 cat('
\\begin{table}
\\begin{center}
\\begin{tabular}{l c c c c }
\\hline
 & No Eff. & Random Eff. & Linear Eff. & Quadratic Eff. \\
\\hline
Intercept & $',file='output/fakeTable.tex')
    cat(fake[1,],sep='$&$',file='output/fakeTable.tex',append=TRUE)
    cat('$\\\\ \n',file='output/fakeTable.tex',append=TRUE)
    cat('&$(',file='output/fakeTable.tex',append=TRUE)
    cat(fake[2,],sep=')$&$(',file='output/fakeTable.tex',append=TRUE)
    cat(')$\\\\ \n',file='output/fakeTable.tex',append=TRUE)
    cat('Slope & $',file='output/fakeTable.tex',append=TRUE)
    cat(fake[1,],sep='$&$',file='output/fakeTable.tex',append=TRUE)
    cat('$\\\\ \n',file='output/fakeTable.tex',append=TRUE)
    cat('&$(',file='output/fakeTable.tex',append=TRUE)
    cat(fake[2,],sep=')$&$(',file='output/fakeTable.tex',append=TRUE)
    cat(')$\\\\ \n',file='output/fakeTable.tex',append=TRUE)
    cat('\\hline
\\end{tabular}
\\caption{Intercept and slope estimates, with standard errors in
  parentheses, for each of the four simulated effects discussed in
  Section \\ref{sec:simulation}}
\\label{tab:sim}
\\end{center}
\\end{table}',file='output/fakeTable.tex',append=TRUE)
}

binnedPlot <- function(mod,jagsDat){
    library(bayesplot)
    attach.jags(mod)
    samp <- sample(1:nrow(studEff),9)
    lp <- with(jagsDat,studEff[samp,studentM]+secEff[samp,section])
    prob <- exp(lp)/(1+exp(lp))
    p <- ppc_error_binned(as.numeric(jagsDat$grad),prob)
    ggplot2::ggsave('output/binnedplot.pdf',p)
    detach.jags()
}
