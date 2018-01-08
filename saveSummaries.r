library(rstan)

files <- list.files('fittedModels')

modSum <- function(mod){
    maxes <- suppressWarnings(sapply(mod@par_dims, prod))
    pars <- mod@model_pars[maxes < 200]
    list(summ=summary(mod,pars=pars,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975))$summary,
         info=mod@sim[sapply(mod@sim,length)==1])
}

for(ff in files){
    print(ff)
    objs <- load(paste0('fittedModels/',ff))
    mods <- objs[sapply(objs, function(oo) inherits(get(oo),'stanfit'))]
    if(length(mods)==1){
        modelSummary=modSum(get(mods[1]))
        save(modelSummary,file=paste0('modelSummaries/',ff))
        rm(list=objs); gc()
        next
    }
    for(mm in mods){
         modelSummary=modSum(get(mm))
         save(modelSummary,file=paste0('modelSummaries/',gsub('.RData','',ff),'_',mm,'.RData'))
    }
    rm(list=objs); gc()
}
