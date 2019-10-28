//time first

data {
  int<lower=0> t;           // number of time 
  int<lower=0> k;           // k=331
  matrix[t,k] mydata;              // x is the real radiation
  matrix[t,3] x;
}

parameters {
  real<lower=0> vari[t];
  matrix[3,1] uc;
  matrix[3,1] ac;
}

transformed parameters{
  matrix[t,1] u;
  matrix[t,1] a;
  u=x*uc;
  a=x*ac;
}

model {
  for (m in 1:k)
    for (n in 2:t)
      mydata[n,m] ~ normal(u[n,1]+a[n,1]*(mydata[n-1,m]-u[(n-1),1]),vari[n]);
}



