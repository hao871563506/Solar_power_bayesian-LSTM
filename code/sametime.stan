data {
  int<lower=0> t;           // number of time 
  int<lower=0> k;           // k=331
  real x[t,k];              // x is the real radiation
}

parameters {
  real aalpha;
  real<lower=0> abeta;
  real<lower=0,upper=1> a[k];

  real uv;
  real<lower=0> usigma;
  real<lower=0,upper=1> u[k];
  
  real<lower=0,upper=1> vari[k];
}

model {

  for (m in 1:k)
    for (n in 2:t)
      x[n,m] ~ normal(u[m]+a[m]*(x[n-1,m]-u[m]),vari[m]);

  a~ normal(aalpha,abeta);
  u~ normal(uv,usigma);
}
