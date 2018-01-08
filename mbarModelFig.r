library(tikzDevice)
options( tikzLatexPackages = c(
getOption( "tikzLatexPackages" ),
"\\usepackage{amsmath,amsfonts}"
))


source('~/gitRepos/ctaiAdvance/prelimStanObs.r')
pload('fittedModels/mbarModel.RData')

sdatObs <- sdat
datObs <- dat
advObs <- advance

draw <- 1000

samps <- extract(mod)
plotDatObs <- with(sdatObs,data.frame(Y=c(YtO,YtM,Yc),mbar=c(MbarTO,samps$MbarTM[draw,],samps$MbarC[draw,]),Z=c(rep(1,nstudTO),rep(1,nstudTM),rep(0,nstudC))))
plotDatObs$treat <- factor(ifelse(plotDatObs$Z==1,'Treatment','Control'))



tikz(file = "mbarModel.tex",
  standAlone = T,
  width = 3, height = 3)
ggplot(plotDatObs,aes(mbar,Y,fill=treat,group=treat,color=treat))+geom_point(alpha=.2,size=0.5)+geom_smooth(method='lm',se=F,size=2)+labs(group=NULL,fill=NULL,color=NULL)+xlab('$\\bar{m}_T$')+ylab('Posttest Score')+theme(legend.position='top')
dev.off()

tools::texi2dvi('mbarModel.tex', pdf = T, clean = T)


tikz(file='mbarSampleSize.tex',
     standAlone=T,
     width=3,height=3)
ggplot(datObs,aes(mbar,nsec))+geom_point(size=.5)+xlab('$\\bar{m}_T$')+ylab('$n_{sec}$')
dev.off()
tools::texi2dvi('mbarSampleSize.tex', pdf = T, clean = T)


pload('fittedModels/stanMod.RData')
secDiff <- colMeans(extract(main,'secEff')[[1]])
secDiff <- extract(main,'secEff')[[1]][draw,]
sss <- secDiff[sdat$sec]
mDiff <- aggregate(sss,list(stud=sdat$studentM),mean)
mbar <- aggregate(sdat$grad,list(sdat$studentM),mean)

mbarDiffDat <- data.frame(mbar=mbar$x,mDiff=mDiff$x)


tikz(file='mbarDiff.tex',
     standAlone=T,
     width=3,height=3)
ggplot(mbarDiffDat,aes(mbar,mDiff))+geom_point(size=.5)+xlab('$\\bar{m}_T$')+ylab('Avg. Section Difficulty')
dev.off()
tools::texi2dvi('mbarDiff.tex', pdf = T, clean = T)


### now do it fancy like
nsec <- as.vector(table(sdat$studentM))
etaDraws <- extract(main,'studEff')[[1]][,sort(unique(sdat$studentM))]
eta <- colMeans(etaDraws)
eta <- etaDraws[draw,]
etasd <- apply(etaDraws,2,sd)

plotDat <- data.frame(nsec=nsec,eta=eta,mDiff=mDiff$x,etasd=etasd)

tikz(file='etaSampleSize.tex',
     standAlone=T,
     width=3,height=3)
ggplot(plotDat,aes(eta,nsec,size=1/etasd))+geom_point()+ylab('$n_{sec}$')+labs(size='$1/\\text{SE}(\\eta_T)$')+scale_size(range=c(.25,1))+guides(size=FALSE)+xlab(paste0('$\\eta$ (',draw,'th draw)'))#+xlab('$\\mathbb{E}\\eta$')
dev.off()
tools::texi2dvi('etaSampleSize.tex', pdf = T, clean = T)


tikz(file='etaDiff.tex',
     standAlone=T,
     width=3,height=3)
ggplot(plotDat,aes(eta,mDiff,size=1/etasd))+geom_point()+ylab('$Avg. Section Difficulty$')+labs(size='$1/\\text{SE}(\\eta_T)$')+scale_size(range=c(.25,1))+guides(size=FALSE)+xlab(paste0('$\\eta$ (',draw,'th draw)'))
dev.off()
tools::texi2dvi('etaDiff.tex', pdf = T, clean = T)
