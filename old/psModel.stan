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
 real Y[N];
 int<lower=0,upper=1> Z[N];
 int<lower=1> teacher[N];
 int<lower=1> nteachers;
 int<lower=1> school[N];
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
 vector[ncov] betaY;
 real bxAb;
 real bxY;
 real unitEffect[nunit];
 real teacherEffectAb[nteachers];
 real teacherEffectPre[nteachers];
 real teacherEffectY[nteachers];
 real schoolEffectAb[nschools];
 real schoolEffectPre[nschools];
 real schoolEffectY[nschools];
 real<lower=0> sigY[2]; /// 1 for ctl, 2 for trt
 real<lower=0> sigUn;
 real<lower=0> sigSec;
 real<lower=0> sigAb;
 real<lower=0> sigPre;
 real<lower=0> sigTeachAb;
 real<lower=0> sigTeachPre;
 real<lower=0> sigTeachY;
 real<lower=0> sigSchoolAb;
 real<lower=0> sigSchoolPre;
 real<lower=0> sigSchoolY;
 real<lower=0> knotMult;
 real pretest[N];
 real tau[4];
}

transformed parameters{

 real muAb[N];
 real muPre[N];
 real muY[N];
 real effect[N];


 for(i in 1:N){
  muAb[i] = X[i,]*betaAb+teacherEffectAb[teacher[i]]+bxAb*pretest[i]+schooEffectAb[school[i]];
  muPre[i] = X[i,]*betaPre; ///+teacherEffectPre[teacher[i]]+schoolEffectPre[school[i]];
  effect[i]= tau[1]+tau[2]*ability[i]+tau[3]*fmax(0,(ability[i] + sigAb*knotMult))+
              tau[4]*fmax(0,(ability[i]-sigAb*knotMult));
  muY[i] = X[i,]*betaY+bxY*pretest[i]+teacherEffectY[teacher[i]]+effect[i]*Z[i]+
            teacherEffectY[teacher[i]]+schoolEffectY[school[i]];
 }
}

model{

  //// priors:
 avgAb~normal(0,5);


 betaAb~normal(0,2);
 betaPre~normal(0,2);
 bxAb~normal(0,2);
 betaY~normal(0,2);
 bxY~normal(0,2);
 tau ~normal(0,1);


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

  Y[i] ~ normal(muY[i],sigY[Z[i]+1])


 }

// students nested w/i teachers nested w/i schools
 teacherEffectAb~normal(0,sigTeachAb);
 teacherEffectPre~normal(0,sigTeachPre);
 teacherEffectY~normal(0,sigTeachY);

 schoolEffectAb~normal(0,sigSchoolAb);
 schoolEffectPre~normal(0,sigSchoolPre);
 schoolEffectY~normal(0,sigSchoolPre);
}
