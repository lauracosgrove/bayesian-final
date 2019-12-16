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

#################################################
# Define server logic 
shinyServer(function(input, output, session) {
  
  ##Creating character vector ##
char_tx <- eventReactive(input$update, {
    stringr::str_split(input$char_tx, ",")
  })
  
output$char_tx <- renderPrint({ 
    char_tx()
  })
  
##ONE SIM##
  ##Creating datatables##
    #generate seed for reproducibility
  seed <- eventReactive(input$update,{
    rpois(1, 100000)
  })
   #tox.profile: show which doses are considered safe in phase 1
  
df_blockrand <- eventReactive(input$update, {
  set.seed(seed())
  get_n_blockrands(n_pt = input$n, n_cycles = input$k, n_tx = input$s, char_tx = char_tx())
  }, ignoreNULL = FALSE)

df_rand <- eventReactive(input$update, {
  set.seed(seed())
  df_blockrand <- df_blockrand()
  
  df_blockrand %>% 
    get_pt_level_effects(dat = ., pop_mean_Y = input$Lambda, pop_sd_Y = input$phi, pop_ave_TE = input$T, pop_sd_TE = input$psi) %>% 
    get_cycle_level_effects(dat = ., n_cycles = input$k, sd_Y_cycles = input$gamma) %>% 
    get_occasion_level_effects(dat = ., measurements = input$m, sd_Y_occassion = input$sigma) %>% 
    get_tx_ind(dat = ., char_tx = char_tx()) %>% 
    get_outcome(dat = .)
  
}, ignoreNULL = FALSE)
  
  
mod <- eventReactive(input$update, {
  set.seed(seed())
  df_rand <- df_rand()
  lme4::lmer(Y ~ 1 + Z + (1|id:cycle) + (Z|id) , data = df_rand)
 }, ignoreNULL = FALSE)

  
  ##Plot outputs##

output$plot_rand <- renderPlot(input$update, {
  df_blockrand <- df_blockrand() 
  
  df_blockrand %>% 
    unnest_longer(col = rand) %>% 
    group_by(id) %>% 
    mutate(occasion = 1:(input$s*input$k))
    
}, ignoreNULL = FALSE)


output$plot_summary <- renderPlot(input$update, {
  df_rand <- df_rand()
  df_rand %>% 
    group_by(id, rand) %>% 
    summarize(Y_bar = mean(Y)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = rand, values_from = Y_bar) %>% 
    group_by(id) %>% 
    summarize(ITE = A - B) %>% 
    mutate(ATE = mean(ITE))  %>% 
    ggplot(aes(x = ITE)) + geom_histogram() + geom_vline(aes(xintercept = ATE, slope = 0), color = "red") + 
    ggthemes::theme_few() + labs(title = "Distribution of ITE, with ATE plotted in red")
  
}, ignoreNULL = FALSE)

##model output##

output$model <- verbatimTextOutput({
  mod <- mod()
  summary(mod)
})

##Table outputs##

output$df_analysis <- DT::renderDataTable({
    df_rand <- df_rand()
    analysis_df <- df_rand %>% 
      select(id, rand, cycle, Z, Y) %>% 
      ungroup()
    
    DT::datatable(analysis_df, rownames = FALSE)
  })
  
  
})
