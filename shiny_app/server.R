#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(here)

source(here::here("functions.R"))
#################################################
# Define server logic 
shinyServer(function(input, output, session) {
  
  ##Creating character vector ##
char_tx <- eventReactive(input$update, {
    stringr::str_split(input$char_tx, ",")[[1]]
  })
  
##ONE SIM##
  ##Creating datatables##
    #generate seed for reproducibility
seed <- eventReactive(input$update,{
    rpois(1, 100000)
  })

df_blockrand <- eventReactive(input$update, {
  set.seed(seed())
  char_tx <- char_tx() 
  get_n_blockrands(n_pt = input$n, n_cycles = input$k, n_tx = input$s, char_tx = char_tx)
  }, ignoreNULL = FALSE)

true_T1 <- eventReactive(input$update, {
  input$T
})

df_rand <- eventReactive(input$update, {
  set.seed(seed())
  char_tx <- char_tx()
  get_n_blockrands(n_pt = input$n, n_cycles = input$k, n_tx = input$s, char_tx = char_tx) %>% 
    get_pt_level_effects(dat = ., pop_mean_Y = input$Lambda, pop_sd_Y = input$phi, pop_ave_TE = input$T, 
                         pop_sd_TE = input$psi) %>% 
    get_cycle_level_effects(dat = ., n_cycles = input$k, sd_Y_cycles = input$gamma) %>% 
    get_occasion_level_effects(dat = ., measurements = input$m, sd_Y_occassion = input$sigma) %>% 
    get_tx_ind(dat = ., char_tx = char_tx) %>% 
    get_outcome(dat = .)
  
}, ignoreNULL = FALSE)
  
summ_df <- eventReactive(input$update, {
  set.seed(seed())
  df_rand <- df_rand()
  get_summary_measure(df_rand)
}, ignoreNULL = FALSE)

mod <- eventReactive(input$update, {
  set.seed(seed())
  df_rand <- df_rand()
  lme4::lmer(Y ~ 1 + Z + (1|id:cycle) + (Z|id) , data = df_rand)
 }, ignoreNULL = FALSE)

tt <- eventReactive(input$update, {
  set.seed(seed())
  df_rand <- df_rand()
  get_pair_test(df_rand)
}, ignoreNULL = FALSE)

ma <- eventReactive(input$update, {
  set.seed(seed())
  summ_df <- summ_df()
  fit_meta_mod(summ_df)
}, ignoreNULL = FALSE)

fma <- eventReactive(input$update, {
  set.seed(seed())
  summ_df <- summ_df()
  fit_fmeta_mod(summ_df)
}, ignoreNULL = FALSE)



occ <- eventReactive(input$update, {
  1:(input$s*input$k)}, ignoreNULL = FALSE)

  ##Plot outputs##
# 
output$plot_rand <- renderPlot({
  df_blockrand <- df_blockrand()
  occ <- occ()
  df_blockrand %>%
    unnest_longer(col = rand) %>%
    group_by(id) %>%
    mutate(occasion = occ) %>%
    ggplot(aes(y = id, x = occasion, fill = rand)) +
    geom_tile(color = "black") +
    ggthemes::scale_fill_few() +
    ggthemes::theme_few()
})
# 
# 
output$plot_summary <- renderPlot({
  df_rand <- df_rand()
  true_TE <- true_T1()
  df_rand %>%
    group_by(id, rand) %>%
    summarize(Y_bar = mean(Y)) %>%
    ungroup() %>%
    pivot_wider(names_from = rand, values_from = Y_bar) %>%
    group_by(id) %>%
    summarize(ITE = A - B) %>%
    mutate(ATE = mean(ITE))  %>%
    ggplot(aes(x = ITE)) + geom_density() + 
    geom_vline(aes(xintercept = ATE, slope = 0), color = "red") +
    geom_vline(aes(xintercept = true_TE, slope = 0), color = "blue") +
    ggthemes::theme_few() + labs(title = "Distribution of ITE, with ATE plotted in red, true TE in blue")

})

##model output##

output$model <- renderPrint({
  mod <- mod()
  summary(mod)
})

output$tt <- renderPrint({
  mod <- mod()
  tt <- tt()
  tt
})

output$ma <- renderPrint({
  ma <- ma()
  summary(ma)
})

output$fma <- renderPrint({
  fma <- fma()
  summary(fma)
})
# ##Table outputs##
# 
output$df_analysis <- DT::renderDataTable({
    df_rand <- df_rand()
    analysis_df <- df_rand %>%
      dplyr::select(id, rand, cycle, Z, Y) %>%
      mutate(Y = round(Y, 2)) %>% 
      ungroup()

    DT::datatable(analysis_df, rownames = FALSE)
  })
  
###REPEATED SIM###
char_tx2 <- eventReactive(input$repeated, {
  stringr::str_split(input$char_tx2, ",")[[1]]
})

true_T <- eventReactive(input$repeated, {
  input$T2
})

true_hte <- eventReactive(input$repeated, {
  input$psi2
})



df_rep <- eventReactive(input$repeated, {
  char_tx <- char_tx2() 
  tibble(sim = 1:input$sim) %>%
    mutate(data = map(sim, ~get_n_blockrands(n_pt = input$n2, n_cycles = input$k2, n_tx = input$s2, char_tx = char_tx))) %>% 
    mutate(data = map(data, ~get_pt_level_effects(dat = .x, pop_mean_Y = input$Lambda2, pop_sd_Y = input$phi2, pop_ave_TE = input$T2,
                                                  pop_sd_TE = input$psi2))) %>%
    mutate(data = map(data, ~get_cycle_level_effects(dat = .x, n_cycles = input$k2, sd_Y_cycles = input$gamma2))) %>%
    mutate(data = map(data, ~get_occasion_level_effects(dat = .x, measurements = input$m2, sd_Y_occassion = input$sigma2))) %>%
    mutate(data = map(data, ~get_tx_ind(dat = .x, char_tx = char_tx))) %>%
    mutate(data = map(data, ~get_outcome(dat = .x)))
}, ignoreNULL = FALSE)


df_fit_rep <- eventReactive(input$repeated, {
  df_rep <- df_rep() 
  true_T <- true_T()
  true_hte <- true_hte()
  df_rep %>% 
    mutate(mod = map(data, ~fit_mod(.x)))  %>% 
    mutate(T_est = map_dbl(mod, ~broom::tidy(.x, "fixed", conf.int = TRUE) %>% filter(term == "Z") %>% pull(estimate))) %>% 
    mutate(T_clow = map_dbl(mod, ~broom::tidy(.x, "fixed", conf.int = TRUE) %>% filter(term == "Z") %>% pull(conf.low))) %>% 
    mutate(T_chigh = map_dbl(mod, ~broom::tidy(.x, "fixed", conf.int = TRUE) %>% filter(term == "Z") %>% pull(conf.high))) %>%
    mutate(hte_est = map_dbl(mod, ~broom::tidy(.x) %>% filter(term == "sd_Z.id") %>% pull(estimate))) %>% 
    mutate(T_in_T_CI = if_else(T_clow <= true_T & T_chigh >= true_T, TRUE, FALSE)) %>% 
    mutate(rej_sharp_null = if_else(T_clow <= 0 & T_chigh >= 0, FALSE, TRUE)) %>% 
    mutate(wrong_selection = if_else(true_T < 0 & T_chigh > 0 | true_T > 0 & T_chigh < 0, TRUE, FALSE)) %>% 
    mutate(hte_mse = sqrt((hte_est - true_hte)^2) ) %>% 
    mutate(ate_mse =  sqrt((T_est - true_T)^2) )
  }, ignoreNULL = FALSE)

df_tt_rep <- eventReactive(input$repeated, {
  df_rep <- df_rep() 
  true_T <- true_T()
  df_rep %>% 
    mutate(ttest = map(data, ~get_pair_test(.x))) %>% 
    mutate(T_est = map_dbl(ttest, ~.x$estimate),
           T_clow = map_dbl(ttest, ~.x$conf.int[1]),
           T_chigh = map_dbl(ttest, ~.x$conf.int[2]),
           rej_sharp_null = map_dbl(ttest, ~.x$p.value),
           wrong_selection = if_else(true_T < 0 & T_chigh > 0 | true_T > 0 & T_chigh < 0, TRUE, FALSE),
           rej_sharp_null = if_else(rej_sharp_null < 0.05, TRUE, FALSE),
           T_in_T_CI = if_else(T_clow <= true_T & T_chigh >= true_T, TRUE, FALSE),
           ate_mse = sqrt((T_est - true_T)^2),
           hte_mse = NA) 
    
}, ignoreNULL = FALSE)

df_fma_rep <- eventReactive(input$repeated, {
  df_rep <- df_rep() 
  true_T <- true_T()
  true_hte <- true_hte()
  df_rep %>% 
    mutate(sum_data = map(data, ~get_summary_measure(.x))) %>% 
    mutate(frma = map(sum_data, ~fit_fmeta_mod(.x))) %>% 
    mutate(T_est = map_dbl(frma, ~.x$beta)) %>% 
    mutate(T_clow = map_dbl(frma, ~.x$ci.lb)) %>% 
    mutate(T_chigh = map_dbl(frma, ~.x$ci.ub)) %>%
    mutate(T_in_T_CI = if_else(T_clow <= true_T & T_chigh >= true_T, TRUE, FALSE)) %>% 
    mutate(rej_sharp_null = if_else(T_clow <= 0 & T_chigh >= 0, FALSE, TRUE)) %>% 
    mutate(wrong_selection = if_else(true_T < 0 & T_chigh > 0 | true_T > 0 & T_chigh < 0, TRUE, FALSE)) %>% 
    mutate(hte_mse = NA) %>% 
    mutate(ate_mse =  sqrt((T_est - true_T)^2) )
  
}, ignoreNULL = FALSE)

df_ma_rep <- eventReactive(input$repeated, {
  df_rep <- df_rep() 
  true_T <- true_T()
  true_hte <- true_hte()
  df_rep %>% 
    mutate(sum_data = map(data, ~get_summary_measure(.x))) %>% 
    mutate(rma = map(sum_data, ~fit_meta_mod(.x))) %>% 
    mutate(T_est = map_dbl(rma, ~.x$beta)) %>% 
    mutate(T_clow = map_dbl(rma, ~.x$ci.lb)) %>% 
    mutate(T_chigh = map_dbl(rma, ~.x$ci.ub)) %>%
    mutate(hte_est = map_dbl(rma, ~sqrt(.x$tau2))) %>% 
    mutate(T_in_T_CI = if_else(T_clow <= true_T & T_chigh >= true_T, TRUE, FALSE)) %>% 
    mutate(rej_sharp_null = if_else(T_clow <= 0 & T_chigh >= 0, FALSE, TRUE)) %>% 
    mutate(wrong_selection = if_else(true_T < 0 & T_chigh > 0 | true_T > 0 & T_chigh < 0, TRUE, FALSE)) %>% 
    mutate(hte_mse = sqrt((hte_est - true_hte)^2) ) %>% 
    mutate(ate_mse =  sqrt((T_est - true_T)^2) )
  
}, ignoreNULL = FALSE)


# output$df_rep_results <- DT::renderDataTable({
#   df_fit_rep <- df_fit_rep() %>% 
#     mutate_if(is.numeric, ~round(.x, digits = 2))
#   DT::datatable(df_fit_rep, rownames = FALSE)
# })

output$table_results <- renderTable({
  summ_fit_rep <- get_sum_table(df_fit_rep(), name = "Mixed Effects Model")
  summ_tt_rep <-  get_sum_table(df_tt_rep() ,name = "Matched Pair T Test") 
  summ_ma_rep <-  get_sum_table(df_ma_rep() , name = "Random Effects Meta-Analysis")
  summ_fma_rep <- get_sum_table(df_fma_rep(), name = "Fixed Effects Meta-Analysis")
  
tab <- summ_fit_rep %>% 
    bind_rows(summ_tt_rep) %>% 
    bind_rows(summ_ma_rep) %>% 
    bind_rows(summ_fma_rep) 
  
colnames(tab) <- c("Method",  "Proportion of studies where sharp null was rejected",
                                 "Proportion of studies where the wrong treatment was recommended",
                                 "Proportion of studies where the CI contained true treatment effect",
                                 "MSE, ATE",
                                 "MSE, HTE")
tab
  })

output$plot_ate <- renderPlot({
  sim_mod <- df_fit_rep()
  sim_tt <- df_tt_rep() 
  sim_ma <- df_ma_rep() 
  sim_fma <- df_fma_rep()
  true_T <- true_T()
  
  prep_df(sim_mod) %>% 
    left_join(prep_df(sim_tt), by = c("sim", "metric"), suffix = c("_mx","_tt")) %>% 
    left_join(prep_df(sim_ma), by = c("sim", "metric")) %>% 
    left_join(prep_df(sim_fma), by = c("sim", "metric"), suffix = c("_ma","_fma")) %>% 
    pivot_longer(starts_with("value"), names_to = "method", values_to = "estimate", names_prefix = "value_") %>% 
    pivot_wider(names_from = "metric", values_from = "estimate") %>% 
    mutate(sim = forcats::fct_reorder(factor(sim), T_est)) %>% 
    mutate(rej_sharp_null = if_else(rej_sharp_null == 1, TRUE, FALSE)) %>% 
    ggplot(aes(x = sim, y = T_est, color = rej_sharp_null)) +
    geom_hline(aes(yintercept = true_T), color = "blue") + 
    facet_grid(~method, labeller = as_labeller(c("mx" = "Mixed Effects Model",
                                                 "tt" = "Matched Paired T-test",
                                                 "fma" = "Fixed Effects Meta-Analysis",
                                                 "ma" = "Random Effects Meta-Analysis")) ) +
    geom_point() + 
    geom_errorbar(aes(ymin = T_clow, ymax = T_chigh)) +
    coord_flip() + 
    ggthemes::theme_few() + ggthemes::scale_color_few() + labs(color = "Rejected Sharp Null of Equality")
  
  
})

output$plot_hte <- renderPlot({
  sim_mod <- df_fit_rep()
  sim_ma <- df_ma_rep() 
  true_hte <- true_hte()
  
  sim_mod %>% 
    dplyr::select(sim, hte_est) %>% 
    left_join(sim_ma %>%  dplyr::select(sim, hte_est), by = "sim", suffix = c("_Mixed Effects Model","_Random Effects Meta Analysis")) %>% 
    pivot_longer(starts_with("hte"), names_to = "method", values_to = "hte_est", names_prefix = "hte_est_") %>% 
    mutate(true_hte  = true_hte) %>% 
    ggplot(aes(y = hte_est, x = method)) + geom_boxplot() + 
    geom_point(aes(y = true_hte, color = true_hte)) +
    ggthemes::theme_few() + labs(x = "", y = "Estimated HTE", color = "True HTE")
  
  
})


})
