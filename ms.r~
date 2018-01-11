pload('~/Box Sync/CT/data/RANDstudyData/MSdata.RData')
datMS <- dat

source('src/prelimStan.r')


totDatMS <- dataPrep(datMS,advanceOrig)
datMS <- totDatMS$dat
advanceMS <- totDatMS$advance
rm(totDat);gc()

sdatMS <- makeStanDat(datMS,advanceMS)


msMod <- stan('src/psmod.stan',data=sdatMS,iter=3000)

