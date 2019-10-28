data {
  int<lower=0> t;           // number of time 
  int<lower=0> j;           // j = 1-30, just upcast all the j into t now, so that same dimension
  int<lower=0> k;           // k=331
  matrix[t,k] x;              // x is the real radiation
}


parameters {
  matrix<lower=-1,upper=1>[j,k] a;
  
  real uv;
  real<lower=0> usigma;
  real uk[k];
  real<lower=0> usigmak[k];
  real av;
  real<lower=0> asigma;
  real ak[k];
  real<lower=0> asigmak[k];
  matrix[j,k] u;
  
  matrix<lower=0>[j,k] vari;
}

model {
  for(l in 1:k)
    for (i in 1:j)
      u[i,l] ~ normal(uk[l], usigmak[l]);

  for (i in 1:j)
    for(l in 1:k)
      a[i,l] ~ normal(ak[l], asigmak[l]);
  
  for (m in 1:k)
    for (n in 2:t)
      x[n,m] ~ normal(u[n%12+1,m]+a[n%12+1,m]*(x[n-1,m]-u[(n-1)%12+1,m]),vari[n%12+1,m]);
      
  uk~ normal(uv,usigma);
  ak~ normal(av,asigma);
}




