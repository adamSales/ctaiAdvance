data {
  int<lower=1> J;                // number of students
  int<lower=1> K;                // number of questions
  int<lower=1> N;                // number of observations
  int<lower=1,upper=J> jj[N];    // student for observation n
  int<lower=1,upper=K> kk[N];    // question for observation n
  int<lower=0,upper=1> y[N];     // correctness of observation n
  int<lower=1> D;                // number of latent dimensions
  int<lower=1> ncov;
  matrix[J,ncov] X;
}
transformed data {
  int<lower=1> L;
  L= D*(K-D)+D*(D+1)/2;  // number of non-zero loadings
}
parameters {
  matrix[ncov,D] betaU;
  real<lower=0> sigTheta[D];
  matrix[J,D] theta;         //person parameter matrix
  vector<lower=0>[L] alpha_l;   //first column discrimination matrix
  vector[K] beta;          // vector of thresholds
  real<lower=0> mu_alpha;      // scale prior
  real mu_beta;                 //location prior
  real<lower=0> sigma_beta;     //scale prior
}
transformed parameters{
  matrix[K,D] alpha; // connstrain the upper traingular elements to zero
  for(j in 1:(D-1)){
    for(i in (j+1):D){
      alpha[j,i] = 0;
    }
  }
{
  int index;
  index=0;
  for (j in 1:K) {
    for (i in 1:min(j,D)) {
      index = index + 1;
      alpha[j,i] = alpha_l[index];
    }
  }
}
}
model {
// the hyperpriors
   mu_alpha~ cauchy(0, 0.5);
   mu_beta ~ cauchy(0, 2);
   sigma_beta ~ cauchy(0,1);
// the priors
  //to_vector(theta) ~ normal(0,1);
  alpha_l ~ lognormal(mu_alpha,0.3);
  beta ~ normal(mu_beta,sigma_beta);
// the likelihood
{
   vector[N] nu;  //vector of predictor terms
 for(d in 1:D)
   theta[,d]~normal(X*betaU[,d],sigTheta[d]);

  for (i in 1:N)
    nu[i] = dot_product(theta[jj[i],],row(alpha,kk[i]))+ beta[kk[i]];
  y ~ bernoulli_logit(nu);
}
}