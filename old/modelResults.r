library(R2jags)
library(jagstools)

fittedModels <- list.files('fittedModels')


sink('TreatmentEstimates.txt')
#pdf('TreatmentEstimates.pdf')


cat('Effect Estimates from Various Models\n\n')
cat('-----------------------------------\n\n\n\n')

for(modFile in fittedModels){
    cat(modFile,'\n\n')
    modName <- load(paste0('fittedModels/',modFile))
    if(modName!='mod'){
        mod <- get(modName)
        rm(list=modName)
    }
    trtMod <- mod$model$model()[which((grepl('trtEff' ,mod$model$model())*
                                !grepl('betaY' ,mod$model$model()))==1)]

    cat('Treatment Model: ',trtMod,'\n\n')
    cat('Estimates:\n')
    print(jagsresults(mod,c('b0','b1','b2','ab')))
    cat('-----------------------------------\n\n\n\n')
    rm(mod)
}
sink()

