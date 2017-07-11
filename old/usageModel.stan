data{
 int<lower=1> Nadv;
 int<lower=0,upper=1> complied[Nadv];
 int<lower=1> advID[Nadv];
 int<lower=1> sectionAdv[Nadv];
 int<lower=1> nsec;
 int<lower=1> UNIT[nsec];
 int<lower=1> nunit;
 int<lower=1> N;
 int<lower=1> Nobs;
 int<lower=1> ncov;
 matrix[N,ncov] X;
 int<lower=1> teacher[N];
 int<lower=1> nteachers;
 int<lower=1> school[nteachers];
 int<lower=1> nschools;
 real pretestObs[Nobs];
 int<lower=1> obs[Nobs];
 real<lower=0> xsem[Nobs];
}

parameters{
 real avgAb; // mean propensity to comply
 real ability[N];
 real diff[nsec];
 vector[ncov] betaAb;
 vector[ncov] betaPre;
 real bx;
 real unitEffect[nunit];
 real teacherEffectAb[nteachers];
 real teacherEffectPre[nteachers];
 real schoolEffectAb[nschools];
 real schoolEffectPre[nschools];
 real<lower=0> sigUn;
 real<lower=0> sigSec;
 real<lower=0> sigAb;
 real<lower=0> sigPre;
 real<lower=0> sigTeachAb;
 real<lower=0> sigTeachPre;
 real<lower=0> sigSchoolAb;
 real<lower=0> sigSchoolPre;
 real pretest[N];
}

transformed parameters{

 real muAb[N];
 real muPre[N];

 for(i in 1:N){
  muAb[i] = X[i,]*betaAb+teacherEffectAb[teacher[i]]+bx*pretest[i];
  muPre[i] = X[i,]*betaPre+teacherEffectPre[teacher[i]];
 }
}

model{

  //// priors:
 avgAb~normal(0,5);


 betaAb~normal(0,5);
 betaPre~normal(0,5);
 bx~normal(0,2);


 //// measurement model: multilevel rasch w covariates
 //// loops thru worked sections
 for(adv in 1:Nadv){
  complied[adv]~ bernoulli_logit(ability[advID[adv]]+diff[sectionAdv[adv]]+avgAb);
 }

 //// sections nested w/i units
 for(sec in 1:nsec){
  diff[sec]~normal(unitEffect[UNIT[sec]],sigSec);
 }
 unitEffect~normal(0,sigUn);

 /// model for pretest---
 for(iobs in 1:Nobs){
  pretestObs[iobs] ~ normal(pretest[obs[iobs]],xsem[iobs]);
 }

 for(i in 1:N){

//// multilevel model for alpha "ability" parameter
  ability[i]~normal(muAb[i],sigAb);


//// multilevel model for pretest--take care of measurement error & missing data
  pretest[i]~normal(muPre[i],sigPre);

 }

// students nested w/i teachers nested w/i schools
 for(tch in 1:nteachers){
  teacherEffectAb[tch]~normal(schoolEffectAb[school[tch]],sigTeachAb);
  teacherEffectPre[tch]~normal(schoolEffectPre[school[tch]],sigTeachPre);
 }
  schoolEffectAb~normal(0,sigSchoolAb);
  schoolEffectPre~normal(0,sigSchoolPre);
}
