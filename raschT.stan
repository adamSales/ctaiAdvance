data{
//Sample sizes
 int<lower=1> nsecWorked;
 int<lower=1> ncov;
 int<lower=1> nstudT;
 int<lower=1> nteacherT;
 int<lower=1> nsec;
 int<lower=1> nschoolT;
 int<lower=1> nstudC;
 int<lower=1> nteacherC;
 int<lower=1> nschoolC;

// indices
 int<lower=1,upper=nteacherT> teacherT[nstudT];
 int<lower=1,upper=nschoolT> schoolT[nstudT];
 int<lower=1,upper=nteacherC> teacherC[nstudC];
 int<lower=1,upper=nschoolC> schoolC[nstudC];

 int<lower=1,upper=nstudT> studentM[nsecWorked];
 int<lower=1,upper=nsec> section[nsecWorked];

// data data
 int<lower=0,upper=1> grad[nsecWorked];
 matrix[nstudT,ncov] Xtrt;
 matrix[nstudC,ncov] Xctl;

}
parameters{

 vector[nstudT] studEffT;

 vector[ncov] betaU;

 real teacherEffU[nteacherT];
 real schoolEffU[nschoolT];
 real secEff[nsec];

 real<lower=0> sigTchU;
 real<lower=0> sigSclU;
 real<lower=0> sigU;
}

model{
 real linPred[nsecWorked];
 vector[nstudT] muU;


// grad model
 for(i in 1:nsecWorked)
  linPred[i]= secEff[section[i]]+studEffT[studentM[i]];

 for(i in 1:nstudT)
  muU[i]=teacherEffU[teacherT[i]]+schoolEffU[schoolT[i]];


 //priors
 betaU~normal(0,2);

 schoolEffU~normal(0,sigSclU);
 teacherEffU~normal(0,sigTchU);

 grad~bernoulli_logit(linPred);

 studEffT~normal(muU+Xtrt*betaU,sigU);
}
generated quantities{
 real studEffC[nstudC];
 real teacherEffUC[nteacherC];
 real schoolEffUC[nschoolC];

 for(i in 1:nteacherC)
  teacherEffUC[i] = normal_rng(0,sigTchU);

 for(i in 1:nschoolC)
  schoolEffUC[i] = normal_rng(0,sigSclU);

 for(i in 1:nstudC)
  studEffC[i] = Xctl[i,]*betaU+teacherEffUC[teacherC[i]]+schoolEffUC[schoolC[i]]+normal_rng(0,sigU);
}