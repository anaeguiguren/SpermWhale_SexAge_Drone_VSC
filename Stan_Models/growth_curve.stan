data {
  int<lower=0> N;           // number of observations
  vector[N] L;              // total length
  vector[N] R;              // head-to-flipper ratio
  vector[N] sd_R;          // measurement uncertainty in ratio 
}

parameters {
  // Mixture probability
  real<lower=0,upper=1> p_female;  
  
  // Female parameters (logistic curve)
  real<lower=0> f_asymp;    // asymptotic ratio
  real<lower=0> f_k;        // growth rate
  real f_mid;               // length at inflection
  
  // Male parameters (linear after threshold)
  real<lower=0> m_init;     // initial ratio
  real<lower=0> m_slope;    // slope after threshold
  real<lower=5,upper=7> threshold;  // length threshold (~6m)
  
  real<lower=0> sigma;      // residual variance
}

model {
  // Priors
  p_female ~ beta(1, 1);     // uniform prior on sex proportions
  
  // Female curve parameters
  f_asymp ~ normal(0.3, 0.1);
  f_k ~ normal(0.5, 0.25);
  f_mid ~ normal(6, 1);
  
  // Male curve parameters
  m_init ~ normal(0.25, 0.1);
  m_slope ~ normal(0.01, 0.005);
  threshold ~ normal(6, 0.5);
  
  sigma ~ exponential(10);
  
  // Likelihood
  for (i in 1:N) {
    real female_ratio = f_asymp / (1 + exp(-f_k * (L[i] - f_mid)));
    real male_ratio;
    
    if (L[i] <= threshold)
      male_ratio = m_init * L[i] / threshold;
    else
      male_ratio = m_init + m_slope * (L[i] - threshold);
    
    target += log_mix(p_female,
      normal_lpdf(R[i] | female_ratio, sqrt(square(sd_R[i]) + square(sigma))),
      normal_lpdf(R[i] | male_ratio, sqrt(square(sd_R[i]) + square(sigma)))
    );
  }
}

generated quantities {
  vector[N] log_lik;
  vector[N] p_female_ind;  // individual probability of being female
  
  for (i in 1:N) {
    real female_ratio = f_asymp / (1 + exp(-f_k * (L[i] - f_mid)));
    real male_ratio;
    
    if (L[i] <= threshold)
      male_ratio = m_init * L[i] / threshold;
    else
      male_ratio = m_init + m_slope * (L[i] - threshold);
    
    real log_female = normal_lpdf(R[i] | female_ratio, sqrt(square(sd_R[i]) + square(sigma)));
    real log_male = normal_lpdf(R[i] | male_ratio, sqrt(square(sd_R[i]) + square(sigma)));
    
    log_lik[i] = log_sum_exp(log(p_female) + log_female, log1m(p_female) + log_male);
    p_female_ind[i] = exp(log(p_female) + log_female - log_sum_exp(log(p_female) + log_female, log1m(p_female) + log_male));
  }
}
