library(blockrand)
library(tidyverse)

#function to block randomize
block_rand = function(n_cycles = k, n_tx = s, char_tx = c("A", "B")) {
  
  blockrand(n = n_tx*n_cycles, num.levels = n_tx, levels = char_tx, block.sizes = 1) %>% 
    pull(treatment)
  
}


get_n_blockrands = function(n_pt = n, n_cycles = k, n_tx = s, char_tx = c("A", "B")) {
  
  rand_df <- tibble(id = 1:n_pt) %>% 
    mutate(rand = map(id, ~block_rand(n_cycles, n_tx, char_tx)))
  
  rand_df
}

#function to create lambda and tau
get_pt_level_effects = function(dat, pop_mean_Y = Lambda, pop_sd_Y = phi, pop_ave_TE = T, pop_sd_TE = psi ) {
  dat %>% 
    mutate(lambda = map_dbl(id, ~rnorm(1, mean = pop_mean_Y, sd = pop_sd_Y))) %>% 
    mutate(tau = map_dbl(id, ~rnorm(1, pop_ave_TE, pop_sd_TE)))
}

get_cycle_level_effects = function(dat, n_cycles = k, sd_Y_cycles = gamma) {
  dat %>% 
    unnest_longer(col = rand) %>% 
    group_by(id) %>% 
    mutate(cycle = rep(1:n_cycles, each = 2)) %>% 
    nest(data = c(rand)) %>% rename(rand = data) %>% 
    mutate(beta = map_dbl(id, ~rnorm(1, mean = 0, sd = sd_Y_cycles)))
}

get_occasion_level_effects = function(dat, measurements = 2, sd_Y_occassion = sigma) {
  dat %>%
    mutate(rand = map(rand, ~unlist(.x))) %>% 
    mutate(measure = list(1:measurements)) %>% 
    unnest_longer(col = rand) %>% unnest_longer(col = measure) %>%  
    select(-rand_id) %>% 
    mutate(epsilon = map_dbl(rand, ~rnorm(1, mean = 0, sd = sd_Y_occassion)))
}

get_tx_ind = function(dat, char_tx = c("A", "B")) {
  dat %>% 
    mutate(Z = if_else(rand == char_tx[[1]], 1/2, -1/2)) 
}

get_outcome = function(dat) {
  dat %>% 
    mutate(Y = lambda + beta + epsilon + tau*Z) %>% 
    select(id, cycle, rand, Y, lambda, beta, epsilon, Z, tau)
}


fit_mod <- function(dat) {
  lme4::lmer(Y ~ 1 + Z + (1|id:cycle) + (Z|id) , data = dat)
}

get_pair_test = function(dat){
  D <- dat %>% 
    select(id, cycle, rand, Y) %>% 
    pivot_wider(names_from = rand, values_from = Y, values_fn = list(Y = mean)) 
  
  t.test(D$A, D$B, paired = TRUE)
} 

get_summary_measure <- function(dat) {
  dat %>% 
    select(id, cycle, rand, Y) %>% 
    group_by(id, cycle) %>% 
    pivot_wider(names_from = rand, values_from = Y, names_prefix = "t_", values_fn = list(Y  = mean)) %>% 
    summarize(d = t_A - t_B) %>% 
    summarize(d_mean = mean(d),
              d_var = var(d))
}

fit_meta_mod <- function(dat) {
  try_fit = try(metafor::rma.uni(dat$d_mean, dat$d_var, method = "REML"))
  class = class(try_fit)
  if(class == "try-error") {
    list(beta = NA,
                  ci.lb = NA,
                  ci.ub = NA, 
                  tau2 = NA) 
  }
    else {
      try_fit
  }
}

fit_fmeta_mod <- function(dat) {
  metafor::rma.uni(dat$d_mean, dat$d_var, method = "FE")
}


get_sum_table <- function(sim, name = "name") {
  sim %>% 
    summarize(Power_SharpNull = mean(rej_sharp_null, na.rm = TRUE),
              PIS =  mean(wrong_selection, na.rm = TRUE),
              Power_Estimate = mean(T_in_T_CI, na.rm = TRUE),
              MSE_ATE = mean(ate_mse, na.rm = TRUE),
              MSE_HTE = mean(hte_mse, na.rm = TRUE)
              ) %>% 
    mutate(Method = name) %>% 
    dplyr::select(Method, everything())
}

prep_df <- function(dat) {
  dat %>% 
    dplyr::select(sim, T_est, T_clow, T_chigh, rej_sharp_null) %>% 
    pivot_longer(-sim, names_to = "metric", values_to = "value")
  
}
