#### Code for implementing NIMBLE model ####

source('code/upload_packages.R')

#### Male fertility ####

code_male <- nimbleCode({
  
  # Variances
  
  sigma_TFR_nat ~ T(dnorm(0,1),0,)
  tau_s ~ dgamma(1,0.01)
  sigma_rho ~ T(dnorm(0,1),0,)
  sigma_delta ~ T(dnorm(0,1),0,)
  delta[1] ~ dnorm(0,sd=sigma_delta)
  
  for(i in 2:Ti){
    delta[i] ~ dnorm(delta[i-1],sd=sigma_delta)
  }
  
  
  
  for(i in 1:n){
    for(t in 1:Ti){
      rho[i,t] ~ dnorm(0,sd=1)
      
    }
  }
  
  s[1:n] ~ dcar_normal(adj=adj[1:L],weights=weights[1:L],num=num[1:n],tau_s,zero_mean=1)
  
  
  # Priors
  for(i in 1:N) {
    TFR[i] ~ dnorm(TFR_nat[i],sigma_TFR_nat)
    for(j in 1:12) {
      Lx[i, j] ~ dnorm(Lx_star[i, j],sd=VAR_Lx_star[i, j])
    }
    for(p in 1:2) {
      beta[i, p] ~ dnorm(0,1)
    }
    
    gamma[i, 1:9] <- m[1:9] + beta[i, 1] * X[1, 1:9] + beta[i, 2] * X[2, 1:9] + s[county_i[i]] + delta[year_i[i]] + rho[county_i[i],year_i[i]]
    phi[i, 1:9] <- exp(gamma[i, 1:9])/sum(exp(gamma[i,1:9]))
    Fx[i,1] <- 0
    Fx[i,2:10] <- 0.2*phi[i,1:9]*TFR[i]
    Sx[i,1:9] <- Lx[i,3:11]/Lx[i,4:12]
    Kx[i,1:9] <- (Sx[i,1:9]*Fx[i,1:9]+Fx[i,2:10])*Lx[i,1]*0.5
    Kx_star[i] <- inprod(M[i, 1:9],Kx[i, 1:9]) 
    
    C[i] ~ dpois(Kx_star[i])
    
  }
})

#### Female fertility ####

code_female <- nimbleCode({
  # Priors
  sigma_TFR_nat ~ T(dnorm(0,1),0,)
  tau_s ~ dgamma(1,0.01)
  sigma_rho ~ T(dnorm(0,1),0,)
  sigma_delta ~ T(dnorm(0,1),0,)
  delta[1] ~ dnorm(0,sd=sigma_delta)
  
  for(i in 1:N) {
    TFR[i] ~ dnorm(mean=TFR_nat[i],sd=sigma_TFR_nat)
  }
  
  
  #sigma_TFR_nat ~ dunif(0,10)
  
  for(i in 1:N) {
    for(j in 1:10) {
      Lx[i, j] ~ dnorm(mean=Lx_star[i, j],sd=VAR_Lx_star[i, j])
    }
  }
  
  for(i in 1:N) {
    for(p in 1:2) {
      beta[i, p] ~ dnorm(mean=0,sd=1)
    }
  }
  
  s[1:n] ~ dcar_normal(adj[1:L],weights[1:L],num[1:n],tau_s)
  delta[1] ~ dnorm(mean=0,sd=sigma_delta)
  for(i in 2:Ti){
    delta[i] ~ dnorm(mean=delta[i-1],sd=sigma_delta)
  }
  for(i in 1:n){
    for(t in 1:Ti){
      rho[i,t] ~ dnorm(mean=0,sd=sigma_rho)
    }
  }
  # Transformations
  for(i in 1:N){
    gamma[i, 1:7] <- m[1:7] + beta[i, 1] * X[1, 1:7] + beta[i, 2] * X[2, 1:7] + 
      s[county_i[i]] + delta[year_i[i]]+rho[county_i[i],year_i[i]]
    phi[i, 1:7] <- exp(gamma[i, 1:7])/sum(exp(gamma[i,1:7]))
    Fx[i,1] <- 0
    Fx[i,2:8] <- 0.2*phi[i,1:7]*TFR[i]
    Sx[i,1:7] <- Lx[i,3:9]/Lx[i,4:10]
    Kx[i,1:7] <- (Sx[i,1:7]*Fx[i,1:7]+Fx[i,2:8])*Lx[i,1]*0.5
    Kx_star[i] <- inprod(W[i, 1:7],Kx[i, 1:7])
  }
  
  
  # Data model
  
  for(i in 1:N){
    C[i] ~ dpois(Kx_star[i])
  }
})




