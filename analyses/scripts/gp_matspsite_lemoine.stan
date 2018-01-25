// Gaussian Process Model for Regional Risk Analysis
// Started by Cat - 16 January 2018

data {
  int<lower=1> N;
  vector[N] fs;
  vector[N] mat;
  
}

transformed data{
  vector[N] mu;
  
  for(n in 1:N) mu[n] = 0;
  
}

parameters{
  
  real<lower=0> sp;
  real<lower=0> cc;
  real<lower=0> site;
  
}

model {
  matrix[N,N] sigma_y;
  
  for(i in 1:(N-1)){
    for(j in (i+1):N){
      sigma_y[i,j] = sp*exp(-site * pow(mat[i]-mat[j], 2));
      sigma_y[j,i] = sigma_y[i,j];
      
    }
    
  }
  
  for(k in 1:N){
    sigma_y[k,k] = sp + cc;
  }
  
  fs ~ multi_normal(mu, sigma_y);
  mat ~ normal(0,1);
  site ~ normal(0,1);
  cc ~ normal(0,1);
  sp ~ normal(0,1);
  
  
}

