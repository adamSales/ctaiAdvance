library(R2jags)
library(jagstools)
rm(list=ls())

sink('simulationResults.txt')
cat('Modeled Effect: b0+b1*U+b2*U^2\n\n')
cat('-------------------------------------\n')
cat('NO EFFECT\n\n')
cat('-------------------------------------\n')
load('psModNoEff.RData')

print(jagsresults(modNoEff,c('b0','b1','b2')))
rm(modNoEff)
cat('\n\n\n')


cat('-------------------------------------\n')
cat('RANDOM EFFECT NO TREND\n\n')
cat('-------------------------------------\n')
load('psModSameEff.RData')

print(jagsresults(modSameEff,c('b0','b1','b2')))
rm(modSameEff)
cat('\n\n\n')


cat('-------------------------------------\n')
cat('LINEAR EFFECT\n\n')
cat('-------------------------------------\n')
load('psModLinEff.RData')

print(jagsresults(modLinEff,c('b0','b1','b2')))
rm(modLinEff)
cat('\n\n\n')

cat('-------------------------------------\n')
cat('QUADRATIC EFFECT\n\n')
cat('-------------------------------------\n')
load('psModQuadEff.RData')

print(jagsresults(modQuadEff,c('b0','b1','b2')))
rm(modQuadEff)
cat('\n\n\n')

sink()
