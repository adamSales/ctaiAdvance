data{
//Sample sizes
 int<lower=1> nsecWorked;
 int<lower=1> ncov;
 int<lower=1> nstud;
 int<lower=1> nteacher;
 int<lower=1> nsec;
 int<lower=1> nschool;


// indices
 int<lower=1,upper=nteacher> teacher[nstud];
 int<lower=1,upper=npair> pair[nstud];
 int<lower=1,upper=nschool> school[nstud];
 int<lower=1,upper=nstud> studentM[nsecWorked];
 int<lower=1,upper=nsec> section[nsecWorked];

// data data
 int<lower=0,upper=1> grad[nsecWorked];
 matrix[nstud,ncov] X;

}
parameters{

 vector[nstud] studEff;

 vector[ncov] betaU;


 real teacherEffU[nteacher];
 real schoolEffU[nschool];
 real secEff[nsec];

 real<lower=0> sigTchU;
 real<lower=0> sigSclU;
 real<lower=0> sigU;
}

model{
 real linPred[nsecWorked];
 vector[nstud] muU;


// grad model
 for(i in 1:nsecWorked)
  linPred[i]= secEff[section[i]]+studEff[studentM[i]];

 for(i in 1:nstud){
  muU[i]=teacherEffU[teacher[i]]+schoolEffU[school[i]];
 }

 //priors
 betaU~normal(0,2);


 schoolEffU~normal(0,sigSclU);
 teacherEffU~normal(0,sigTchU);

 grad~bernoulli_logit(linPred);

 studEff~normal(muU+X*betaU,sigU);
}