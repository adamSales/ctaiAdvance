functions {
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order);
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order) {
    vector[size(t)] b_spline;
    vector[size(t)] w1; //= rep_vector(0, size(t));
    vector[size(t)] w2; // = rep_vector(0, size(t));
    if (order==1)
      for (i in 1:size(t))
        b_spline[i] = (ext_knots[ind] <= t[i]) && (t[i] < ext_knots[ind+1]);
    else {
      if (ext_knots[ind] != ext_knots[ind+order-1])
        w1 = (to_vector(t) - rep_vector(ext_knots[ind], size(t))) /
             (ext_knots[ind+order-1] - ext_knots[ind]);
      if (ext_knots[ind+1] != ext_knots[ind+order])
        w2 = 1 - (to_vector(t) - rep_vector(ext_knots[ind+1], size(t))) /
                 (ext_knots[ind+order] - ext_knots[ind+1]);
      b_spline = w1 .* build_b_spline(t, ext_knots, ind, order-1) +
                 w2 .* build_b_spline(t, ext_knots, ind+1, order-1);
    }
    return b_spline;
  }
}
data {
  int spline_degree;
  int<lower=0> nsecWorked;
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
  int<lower=1> num_knots;
  vector[num_knots] knots;

}
transformed data {
  int num_basis = num_knots + spline_degree - 1;
  vector[spline_degree + num_knots] ext_knots_temp;
  vector[2*spline_degree + num_knots] ext_knots;

  ext_knots_temp = append_row(rep_vector(knots[1], spline_degree), knots);
  ext_knots = append_row(ext_knots_temp, rep_vector(knots[num_knots], spline_degree));

}
parameters {
  real studEff[nstud];
  row_vector[num_basis] ut_raw;
  row_vector[num_basis] uu_raw;

  real secEff[nsec];
  real unitEff[nunit];
  real schoolEffU[nscl];
  real schoolEffY[nscl];
  real classEffY[nclass];
  real pairEffect[npair];

  real u0;
  real t0;

  vector[ncovar] betaU;
  vector[ncovar] betaY;

  real<lower=0> sigSclY;
  real<lower=0> sigSclU;
  real<lower=0> sigSec;
  real<lower=0> sigUnit;
  real<lower=0> sigCls;
  real<lower=0> adjT;
  real<lower=0> adjU;
  real<lower=0> sigY[2];
  real<lower=0> sigU;



}
transformed parameters {
  vector[nsecWorked] linPred;
  vector[nstud] muU;
  vector[nstud] muY;
  vector[nstud] useEff;
  vector[nstud] trtEff;
  real studEff_std[nstud];
  real seMu;
  real seSig;
  matrix[num_basis, nstud] B;
  row_vector[num_basis] ut;
  row_vector[num_basis] uu;
  real<lower=0> sigYbig[nstud];

  seMu = mean(studEff);
  seSig = sd(studEff);

  for(i in 1:nstud)
   studEff_std[i]=(studEff[i]-seMu)/seSig;

  for (ind in 1:num_basis)
    B[ind,:] = to_row_vector(build_b_spline(studEff_std, to_array_1d(ext_knots), ind,spline_degree + 1));
  B[num_knots + spline_degree - 1, nstud] = 1;

  for(i in 1:nsecWorked)
   linPred[i]=studEff[studentM[i]]+secEff[section[i]]+unitEff[unit[i]];

  ut[1]=ut_raw[1];
  uu[1]=uu_raw[1];
  for (i in 2:num_basis){
    ut[i] = ut[i-1] + ut_raw[i]*adjT;
    uu[i] = uu[i-1] + uu_raw[i]*adjU;
  }
  useEff=u0*to_vector(studEff_std)+to_vector(uu*B);
  trtEff=t0*to_vector(studEff_std)+to_vector(ut*B);

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
  u0~normal(0,3);
  t0~normal(0,3);
  adjU~normal(0,1);
  adjT~normal(0,1);
  ut_raw~normal(0,1);
  uu_raw~normal(0,1);


  studEff ~ normal(muU,sigU);
  grad ~ bernoulli_logit(linPred);
  Y ~ normal(muY,sigY);
 }