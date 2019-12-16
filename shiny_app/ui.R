#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinythemes)
library(shinybusy)

library(here)
source(here::here("functions.R"))

# Define UI 
shinyUI(fluidPage(theme = shinytheme("lumen"),
  
  # Application title
  titlePanel("n-of-1 Design Exploration"),
    tabsetPanel(type = "pills",
     tabPanel("Introduction", fluidPage(    
                         includeHTML("introduction2.html"))
                ),      
     tabPanel("Simulation",
          fluidPage(
             fluidRow(
             column(width = 4,
              wellPanel(
                helpText(h5("First, specify design characteristics for simulation. One series of 
                            n-of-1 trials will be simulated.")),
                numericInput("k",
                             h5("Number of cycles:"),
                             min = 2,
                             max = 8,
                             value = 4),
                numericInput("s",
                             h5("Number of treatments:"),
                             min = 2,
                             max = 2,
                             value = 2),
                helpText("Only an A vs. B treatment contrast is currently implemented."),
                textInput("char_tx", 
                          h5("Treatment Names:"),
                          value = "A,B"),
                numericInput("n",
                             h5("Number of patients in the series:"),
                             min = 10,
                             max = 150,
                             value = 34),
                numericInput("m",
                             h5("Number of measurements per treatment period:"),
                             min = 1,
                             max = 10,
                             value = 1),
                helpText(h5("Next, specify true population parameters for simulation.")),
                numericInput("Lambda",
                             h5("Population mean of outcome:"),
                             min = 1,
                             max = 100,
                             value = 50),
                numericInput("phi",
                             h5("Population standard deviation of outcome:"),
                             min = 5,
                             max = 15,
                             value = 10),
                numericInput("gamma",
                             h5("Noise within a cycle:"),
                             min = 0,
                             max = 4,
                             value = 2),
                numericInput("sigma",
                             h5("Noise within a given treatment period:"),
                             min = 0,
                             max = 4,
                             value = 2),
                numericInput("T",
                             h5("Population average treatment effect:"),
                             min = -40,
                             max = 40,
                             value = 4),
                numericInput("psi",
                             h5("Heterogeneity of treatment effect (standard deviation):"),
                             min = 0,
                             max = 80,
                             value = 16),
                helpText("These ATE and HTE are in the units of the outcome."),
                actionButton("update", "Simulate!")
              )
             ), #end of col
             column(width = 8, 
                    h5("Block Randomization"), 
                    plotOutput("plot_rand")),
             column(width = 4, 
                    h5("Simulated Data"), 
                    DT::dataTableOutput("df_analysis")),
             column(width = 4, 
                    h5("Plot of Individual Treatment Effects (Sufficient Statistic Method)"), 
                    plotOutput("plot_summary")),
             column(width = 8, 
                    h5("Results: Mixed Effects Model"), 
                    verbatimTextOutput("model")),
             column(width = 8, 
                    h5("Results: Matched Pair T Test"), 
                    verbatimTextOutput("tt")),
             column(width = 8, 
                    h5("Results: Random Effects Meta-Analysis"), 
                    verbatimTextOutput("ma")),
             column(width = 8, 
                    h5("Results: Fixed Effects Meta-Analysis"), 
                    verbatimTextOutput("fma"))
             
             
             
             ) #end of row
          ) #end of page
     ), #end of panel
     tabPanel("Operating Characteristics", 
              fluidPage(
                add_busy_spinner(spin = "fading-circle"),
                 column(width = 4,
                        wellPanel(
                           helpText(h5("First, specify design characteristics for simulation. Multiple series of
                            n-of-1 trials will be simulated.")),
                           numericInput("k2",
                                        h5("Number of cycles:"),
                                        min = 2,
                                        max = 8,
                                        value = 4),
                           numericInput("s2",
                                        h5("Number of treatments:"),
                                        min = 2,
                                        max = 2,
                                        value = 2),
                           helpText("Only an A vs. B treatment contrast is currently implemented."),
                           textInput("char_tx2", 
                                     h5("Treatment Names:"),
                                     value = "A,B"),
                           numericInput("n2",
                                        h5("Number of patients in the series:"),
                                        min = 10,
                                        max = 150,
                                        value = 34),
                           numericInput("m2",
                                        h5("Number of measurements per treatment period:"),
                                        min = 1,
                                        max = 10,
                                        value = 1),
                           helpText(h5("Next, specify true population parameters for simulation.")),
                           numericInput("Lambda2",
                                        h5("Population mean of outcome:"),
                                        min = 1,
                                        max = 100,
                                        value = 50),
                           numericInput("phi2",
                                        h5("Population standard deviation of outcome:"),
                                        min = 05,
                                        max = 15,
                                        value = 10),
                           numericInput("gamma2",
                                        h5("Noise within a cycle:"),
                                        min = 0,
                                        max = 4,
                                        value = 2),
                           numericInput("sigma2",
                                        h5("Noise within a given treatment period:"),
                                        min = 0,
                                        max = 4,
                                        value = 2),
                           numericInput("T2",
                                        h5("Population average treatment effect:"),
                                        min = -40,
                                        max = 40,
                                        value = 4),
                           numericInput("psi2",
                                        h5("Heterogeneity of treatment effect (standard deviation):"),
                                        min = 0,
                                        max = 80,
                                        value = 16),
                           helpText("These ATE and HTE are in the units of the outcome"),
                           numericInput("sim",
                                        h5("Number of simulations"),
                                        min = 1,
                                        max = 150,
                                        value = 10),
         actionButton("repeated", "Simulate n times")
        
         )),
        column(width = 8, 
               h5("Results"), 
               tableOutput("table_results")),
        column(width = 8, 
               h5("ATE"), 
               plotOutput("plot_ate")),
        column(width = 4, 
               h5("HTE"), 
               plotOutput("plot_hte"))
        
        
     )
     )
   #  #show what you selected
  # ,
   #  column(width = 4, h5("True Dose-Efficacy Relationship"), plotlyOutput("plot_eff"))
   #  ),
   #    fluidRow(
   #      column(width = 4,
   #      wellPanel(
   #        h3("Stage 1"),
   #        helpText(h5("For stage 1, specify acceptable and unacceptable toxicity rates, likelihood ratio threshold, and cohort size.")),
   #        
   #        #Acceptable (p_yes) and unacceptable (p_no) DLT rates used for establishing safety (select range between 0 and 1)
   #        fluidRow(
   #          box(width = 11, title = "Dose Limiting Toxicity (DLT) Rates",
   #              splitLayout(
   #                sliderInput("p_yes", h5("Acceptable DLT:"),
   #                            min = 0, max = 1, value = 0.15,
   #                            step = diff(0:1/20, 1) #animate=TRUE
   #                ),
   #                uiOutput("Udlt")
   #              )
   #          )
   #        ),
   #        
   #        #Likelihood-ratio (LR) threshold (2, 4, 8?  check paper)
   #        numericInput("K",
   #                     h5("Likelihood Ratio Threshold (k):"),
   #                     min = 1,
   #                     max = 32,
   #                     step = 1,
   #                     value = 2),
   #        helpText("Recommended values are 2, 4, or 8 for small sample sizes (Blume, 2002)."),
   #        
   #        #Cohort size used in stage 1: 
   #        numericInput("coh.size",
   #                     h5("Stage 1 cohort size:"),
   #                     min = 1,
   #                     max = 10,
   #                     step = 1,
   #                     value = 3)
   #      )
   #      ), #end of column
   #    column(width = 4,
   #      h5("Simulated Toxicity Profile"), 
   #      DT::dataTableOutput("dt_tox"),
   #      h5("Doses are considered safe if the likelihood ratio is greater than 1/k (colored green).")
   #    ) #end of column
   #    ), #end of row
   #  fluidRow(
   #    column(width = 4,
   #      wellPanel(
   #        h3("Stage 2"),
   #        helpText(h5("Stage 2 is conducted with only safe doses. Safety and efficacy are monitored, and participants will have a higher likelihood of being assigned safe and efficacious doses.")),
   #        #Total Sample Size
   #        numericInput("N",
   #                     h5("Total Sample Size:"),
   #                     min = 3,
   #                     max = 100,
   #                     step = 1,
   #                     value = 30),
   #        numericInput("stoprule",
   #                     h5("Stop Rule:"),
   #                     min = 1,
   #                     max = 10,
   #                     step = 1,
   #                     value = 9),
   #        helpText("This stop rule determines after how many patients the stage 1 trial should be stopped, if dose 1 is shown to be toxic."),
   #        #Update Button
   #      )
   #      ), #end of col
   #      #plot and table from rand stg 2 and table will go here in fluid row
   #    column(width = 4,
   #      h5("Adaptive Randomization in Stage 2"),
   #      DT::dataTableOutput("dt_rand")
   #      ),
   #    column(width = 4,
   #      h5("Estimated Efficacy of Safe Doses (Stages 1 & 2)"),
   #      plotlyOutput("plot_stg2")
   #      ) #end of col
   #  ) #end of row
   # ) #end fluidPage
   #      ), #end tab panel
   #      tabPanel("Repeated simulation",
   #  wellPanel(
   #    add_busy_spinner(spin = "fading-circle"),
   #    numericInput("sims",
   #                 h5("Number of repeated simulations:"),
   #                 min = 1,
   #                 max = 1000,
   #                 step = 50,
   #                 value = 100),
   #    helpText("Before conducting repeated simulations, specify a design in the Simulation tab."),
   #    #Simulate n times
   #    actionButton("repeated", "Simulate n times")
   #  ),
   #  column(width = 8, h5("Summary Statistics of Simulated Trials"),
   #    h5("Percent allocation per dose"),
   #    DT::dataTableOutput("sim_treated"),
   #    h5("Estimated efficacy outcomes by dose"),
   #    DT::dataTableOutput("sim_eff"))
   #  ),
   # tabPanel("Implementation"
   #          ,fluidPage(    
   # includeMarkdown("Implementation_example.md"))
   #  )
  )
)
)