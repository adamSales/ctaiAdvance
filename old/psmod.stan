data {
  int<lower=1> nsecWorked;
  int<lower=0,upper=1> grad[nsecWorked];
  int<lower=1> studentM[nsecWorked];
  int<lower=1> section[nsecWorked];
  int<lower=1> unit[nsecWorked];
  int<lower=1> nsec;
  int<lower=1> nunit;
  int<lower=1> nscl;
  int<lower=1> nclass;

  int<lower=1> ncovar;
  int<lower=1> nstud;
  matrix[nstud,ncovar] X;
  int<lower=0, upper=1> Z[nstud];
  real Y[nstud];

  int<lower=1> cid[nstud];
  int<lower=1> school[nstud];

  int<lower=1> pair[nstud];
  int<lower=0> npair;


}
parameters {
  real studEff[nstud];

  real secEff[nsec];
  real unitEff[nunit];
  real schoolEffU[nscl];
  real schoolEffY[nscl];
  real classEffY[nclass];
  real pairEffect[npair];

  vector[ncovar] betaU;
  vector[ncovar] betaY;

  real<lower=0> sigSclY;
  real<lower=0> sigSclU;
  real<lower=0,upper=5> sigSec;
  real<lower=0> sigUnit;
  real<lower=0> sigCls;
  real<lower=0,upper=2> sigY[2];
  real<lower=0,upper=5> sigU;

  real ut[3];
  real uu[3];



}
transformed parameters {
  vector[nsecWorked] linPred;
  vector[nstud] muU;
  vector[nstud] muY;
  vector[nstud] useEff;
  vector[nstud] trtEff;
  vector[nstud] studEff_std;
  vector[nstud] studEff_std2;
  real seMu;
  real seSig;
  real<lower=0> sigYbig[nstud];

  seMu = mean(studEff);
  seSig = sd(studEff);

  studEff_std=(to_vector(studEff)-seMu)/seSig;

  for(i in 1:nstud)
   studEff_std2[i]=studEff_std[i]^2;

  for(i in 1:nsecWorked)
   linPred[i]=studEff[studentM[i]]+secEff[section[i]]+unitEff[unit[i]];

  useEff=uu[1]+uu[2]*studEff_std+uu[3]*studEff_std2;
  trtEff=ut[1]+ut[2]*studEff_std+ut[3]*studEff_std2;

  for(i in 1:nstud){
   muU[i]=X[i,]*betaU+schoolEffU[school[i]];
   muY[i]=X[i,]*betaY+ useEff[i]+trtEff[i]*Z[i]+classEffY[cid[i]]+pairEffect[pair[i]]+schoolEffY[school[i]];
   sigYbig[i]=sigY[Z[i]+1];
}

}
model {

  schoolEffY~normal(0,sigSclY);
  schoolEffU~normal(0,sigSclU);
  secEff~normal(0,sigSec);
  unitEff~normal(0,sigUnit);
  classEffY~normal(0,sigCls);

  betaU~normal(0,3);
  betaY~normal(0,3);
  uu~normal(0,1);
  ut~normal(0,1);


  studEff ~ normal(muU,sigU);
  grad ~ bernoulli_logit(linPred);
  Y ~ normal(muY,sigYbig);
 }