#### Code for model fitting ####

#### Utilities ####
source('code/packages_upload.R')
source('code/functions.R')
source('code/model_nimble.R')

#### Upload data ####

load('data/model_male_data.RData')
load('data/model_female_data.RData')

#### Male TFR estimates for California ####

# input data

input_data = data_input_men(data=CM_data,statefip="06",usa_geo=long_usa,m_male,
                            X_male,years=1982:2019)



this_cluster <- makeCluster(4)


chain_output <- parLapply(cl = this_cluster, 
                          X = 1:4, 
                          fun = run_MCMC_allcode, 
                          data = input_data$nimble_input, 
                          code = code_male,
                          consts = input_data$consts,
                          init=input_data$init,
                          init_name = c(names(input_data$init)),
                          niter=100000,
                          nburnin=10000)

stopCluster(this_cluster)




california_TFR_final_male = MCMCvis::MCMCsummary(
  chain_output,
  params = "TFR",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 




#### Female TFR estimates for California ####


input_data = data_input_women(data=CW_data,statefip="06",usa_geo=long_usa,m_female,
                            X_female,years=1982:2019)



this_cluster <- makeCluster(4)


chain_output <- parLapply(cl = this_cluster, 
                          X = 1:4, 
                          fun = run_MCMC_allcode, 
                          data = input_data$nimble_input, 
                          code = code_female,
                          consts = input_data$consts,
                          init=input_data$init,
                          init_name = c(names(input_data$init)),
                          niter=100000,
                          nburnin=1000)

stopCluster(this_cluster)


california_TFR_final_female = MCMCvis::MCMCsummary(
  chain_output,
  params = "TFR",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 

save(california_TFR_final_female,california_TFR_final_male,file='results/california_estimates.RData')



#### Male TFR estimates for Utah ####

# input data

input_data = data_input_men(data=CM_data,statefip="49",usa_geo=long_usa,m_male,
                            X_male,years=1982:2019)



this_cluster <- makeCluster(4)


chain_output <- parLapply(cl = this_cluster, 
                          X = 1:4, 
                          fun = run_MCMC_allcode, 
                          data = input_data$nimble_input, 
                          code = code_male,
                          consts = input_data$consts,
                          init=input_data$init,
                          init_name = c(names(input_data$init)),
                          niter=10000,
                          nburnin=1000)

stopCluster(this_cluster)




utah_TFR_final_male = MCMCvis::MCMCsummary(
  chain_output,
  params = "TFR",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 




#### Female TFR estimates for Utah ####


input_data = data_input_women(data=CW_data,statefip="49",usa_geo=long_usa,m_female,
                            X_female,years=1982:2019)



this_cluster <- makeCluster(4)


chain_output <- parLapply(cl = this_cluster, 
                          X = 1:4, 
                          fun = run_MCMC_allcode, 
                          data = input_data$nimble_input, 
                          code = code_female,
                          consts = input_data$consts,
                          init=input_data$init,
                          init_name = c(names(input_data$init)),
                          niter=10000,
                          nburnin=1000)

stopCluster(this_cluster)


utah_TFR_final_female = MCMCvis::MCMCsummary(
  chain_output,
  params = "TFR",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 

save(utah_TFR_final_female,utah_TFR_final_male,file='results/utah_estimates.RData')


