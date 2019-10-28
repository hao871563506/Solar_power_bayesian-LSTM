data {
  int<lower=0> t;           // number of time 
  int<lower=0> j;           // j = 1-30, just upcast all the j into t now, so that same dimension
  real x[t];              // x is the real radiation
}

parameters {
  real<lower=0> aalpha;
  real<lower=0> abeta;
  real<lower=0,upper=1> a[j];

  real uv;
  real<lower=0> usigma;
  real<lower=0,upper=1> u[j];
  
  real<lower=0,upper=1> vari[j];
}

model {
  for (n in 2:t)
    x[n] ~ normal(u[n%12+1]+a[n%12+1]*(x[n-1]-u[(n-1)%12+1]),vari[n%12+1]);
  a~ beta(aalpha,abeta);
  u~ normal(uv,usigma);
}




