library(jagstools)
source('src/figureFunctions.r') ### functions to make the figures
source('src/prelim.r') ### loads in data

### loads the lastest version of the main model
### called "main"
files <- list.files('~/Google Drive/CTmodels/realModels')
mainFile <- max(grep('main',files,value=TRUE))
load(paste0('~/Google Drive/CTmodels/realModels/',mainFile))


### produces b1stdDens.pdf
stdPlot(main,jagsDatReal)

### produces binnedPlot.pdf
binnedPlot(main,jagsDatReal)

### produces plots from models from fake data:
## noEff.pdf
## constEff.pdf
## linEff.pdf
## quadEff.pdf
plotFake()

### produces Figures 1 and 5a:
## sampleSizeMbar.pdf
## mbarVsEta.pdf
sampleSizeFig(jagsDatReal,main)

### produces potentialOutcomes.pdf
potentialOutcomesPlot(main)

### produces pretestEta.pdf
pretestEtaPlot(main,dat)

### produces robustness.pdf
robustnessPlot()

### produces treatmentEffect.pdf
plotEff(main,file='output/treatmentEffect.pdf')

### produces usageCoef.pdf
usageCoefPlot(main,dat)



probSlopeNegative <- mean(main$BUGSoutput$sims.list$b1<0)




R2 <- 1-apply(betaU%*%t(X),1,var)/apply(studEff,1,var)



##### missing data imputation table
load('../../data/RANDstudyData/HSdata.RData')
miss <- NULL
for(i in c('race','sex','spec','xirt')) miss <- rbind(miss,
 c(sum(is.na(covs[[i]])),mean(is.na(covs[[i]])),error[i,'error']))
miss <- as.data.frame(miss)
miss$`Error Type` <- c('PFC','PFC','PFC','SRMSE')
rownames(miss) <- c('Race/Ethnicity','Sex','Special Education','Pretest')
names(miss)[1:3] <- c('# Missing','% Missing','Imputation Error')
miss[,2] <- as.integer(round(miss[,2]*100))
miss[,1] <- as.integer(miss[,1])
miss['Pretest','Imputation Error'] <- sqrt(miss['Pretest','Imputation Error'])/sd(covs$xirt,na.rm=TRUE)


print(xtable::xtable(miss,caption='Missing data and imputation error for pre-treatment covariates. Imputations were generated with the \\texttt{missForest} package in \texttt{R} and the imputation error was estimated from out-of-box samples. For categorical variables, the error is reported as proportion falsly classified (PFC) and for continuous variables, the error is standardized root mean squared error (SRMSE)',label='tab:miss'),file='output/missingTable.tex')




#### covariate Balance
library(RItools) ### this is development version from github

covs2 <- covs[rownames(dat),] ### same as dat but with NAs

balTest <- balanceTest(treatment~race+sex+spec+xirt+state+cluster(schoolid2)+strata(pair),data=covs2,report=c('adj.means','std.diffs','chisquare.test'))


print(xtable::xtable(
