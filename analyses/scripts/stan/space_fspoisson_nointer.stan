// Stan Model attempt for FS# ~ MAT + Site + Spp + CC 
// Started 19 January 2018 by Cat


data {
  int<lower=0> N; // Number of observations
  int<lower=0> fs[N];
  
  real mat[N];
  real sp[N];
  real lat[N];
  real lon[N];
  
}

parameters {
  real a;
  real b_sp;
  real b_mat;
  real b_lat;
  real b_lon;
  
}

transformed parameters { 
  real lp[N];
  real<lower=0> mu[N];
  
  for(i in 1:N){
    lp[i] = a + 
    b_sp*sp[i] +
    b_mat*mat[i] +
    b_lat*lat[i] +
    b_lon*lon[i]
    ;
    
    mu[i] = exp(lp[i]);
  }
	
}

model {
  
	fs ~ poisson(mu);

}

