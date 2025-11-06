##### Useful functions ####

#### Create adjacency matrix ####

adj_matrix_constr = function(data,fips=NA){
  # Sample data as a data frame
  data <- data %>%
    filter(state == fips,
           substr(neighbor_name,1,2) == fips,
           county_name!=neighbor_name)
  
  # Extract unique counties
  counties <- unique(c(data$county_name, data$neighbor_name))
  
  # Create an empty adjacency matrix
  adj_matrix <- matrix(0, nrow = length(counties), ncol = length(counties),
                       dimnames = list(counties, counties))
  
  # Fill the adjacency matrix
  for (i in 1:nrow(data)) {
    county <- data$county_name[i]
    neighbor <- data$neighbor_name[i]
    adj_matrix[county, neighbor] <- 1
    adj_matrix[neighbor, county] <- 1  # Because adjacency is symmetric
  }
  
  diag(adj_matrix) = 0
  
  return(adj_matrix)
}

#### Run chains using MCMC ####

run_MCMC_allcode <- function(data, code, consts,init,init_name,niter=niter,nburnin=10000) {
  library(nimble)
  
  dynModel = nimbleModel(code = code,
                         data = data, 
                         constants = consts, 
                         inits = init)
  
  
  cat("============\n[compiling]\n============\n")
  CdynModel = compileNimble(dynModel,  showCompilerOutput = TRUE)
  cat("============\n[building]\n============\n")
  dynMCMC = buildMCMC(dynModel,  monitors = init_name, print=T)
  cat("=========\n[merging]\n============\n")
  CdynMCMC = compileNimble(dynMCMC, project = dynModel, showCompilerOutput = T)
  
  
  samples = runMCMC(CdynMCMC, 
                    niter = niter,
                    nburnin = nburnin,
                    thin = 100)
  
  
  
  return(samples)
}



#### Data to fit the model ####

data_input_men = function(data,statefip="06",usa_geo,m_male,
                      X_male,years=1982:2019){
  
  adj_matrix = adj_matrix_constr(usa_geo,fips="06") 
  data = filter(data,year %in% years,state %in% statefip) 
  spatial_object = as.carAdjacency(adj_matrix)
  state_i = data %>%
    ungroup() %>%
    select(state) %>%
    pull()
  
  year_i = data %>%
    ungroup() %>%
    select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    select(fips) %>%
    pull()
  
  
  state_i = as.integer(factor(state_i,levels=unique(data$state)))
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$fips)))
  
  
  
  N = nrow(data)
  Ti = length(unique(data$year))
  S = length(unique(data$state))
  n = length(unique(data$fips))
  
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                S  = length(unique(data$state)),
                n  = length(spatial_object$num),
                L = length(spatial_object$weights),
                state_i = state_i,
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object$adj, 
                num = spatial_object$num,
                weights= spatial_object$weights)
  
  nimble_input = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,55,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,55,5))]),
    VAR_Lx_star = (sqrt(as.matrix(data[, paste0('VAR_L',seq(0,55,5))]))),
    TFR_nat = data$TFR_nat,
    m = m_male,
    X = X_male)
  
  init=list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
            TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
            Lx = matrix(runif(N*12, min=4.5, max=5) , N ,12),
            sigma_TFR_nat =1,
            s = rep(0,n),
            delta = rep(0,Ti),
            sigma_delta=1,
            tau_s=0.1,
            rho = matrix(0,n,Ti),
            sigma_rho=1)
  
  
  return(list(nimble_input=nimble_input,consts=consts,init=init))
  
  
  
}






