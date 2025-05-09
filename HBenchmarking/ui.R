library(shiny)
library(plotly)

shinyUI(fluidPage(
  titlePanel("Hospital Benchmarking App"),
  tags$head(
    tags$style(HTML("
    /* Hide up/down arrows in numeric inputs */
    input[type=number]::-webkit-inner-spin-button, 
    input[type=number]::-webkit-outer-spin-button { 
      -webkit-appearance: none; 
      margin: 0; 
    }
    input[type=number] {
      -moz-appearance: textfield;  /* Firefox */
    }
  "))
  ),
  
  tabsetPanel(
    # FIRST TAB: Cluster Overview
    tabPanel("Cluster Overview",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_cluster",
                             "Select a Cluster:",
                             choices = NULL),
                 
                 #Number of clusters label
                 h4("Number of hospitals in this cluster:"),
                 verbatimTextOutput("hospital_count"),
                 
                 #Cluster Stadistics
                 br(),
                 h4("Cluster Statistics:"),
                 tableOutput("hospital_stats"),
                 #Legend
                 br(),
                 helpText("Facility Type Codes:"),
                 tags$ul(
                   tags$li("CAH = Critical Access Hospital"),
                   tags$li("CH = Children's Hospital"),
                   tags$li("LTCH = Long-Term Care Hospital"),
                   tags$li("PH = Psychiatric Hospital"),
                   tags$li("RH = Rehabilitation Hospital"),
                   tags$li("STH = Short-Term (Acute Care) Hospital")
                 )
                 
               ),
               mainPanel(
                 # h4("Number of hospitals in this cluster:"),
                 # verbatimTextOutput("hospital_count"),
                 # 
                 # br(),
                 # h4("Cluster Statistics:"),
                 # tableOutput("hospital_stats"),
                 
                 br(),
                 h4("Cluster Composition Heatmap:"),
                 plotlyOutput("cluster_composition_plot"),
                 
                 # br(),
                 # helpText("Facility Type Codes:"),
                 # tags$ul(
                 #   tags$li("CAH = Critical Access Hospital"),
                 #   tags$li("CH = Children's Hospital"),
                 #   tags$li("LTCH = Long-Term Care Hospital"),
                 #   tags$li("PH = Psychiatric Hospital"),
                 #   tags$li("RH = Rehabilitation Hospital"),
                 #   tags$li("STH = Short-Term (Acute Care) Hospital")
                 # ),
                 
                 br(),
                 h4("Hospitals by County:"),
                 plotlyOutput("county_breakdown_plot"),
                 
                 br(),
                 h4("Beds & Revenue per Hospital:"),
                 plotlyOutput("beds_bubble_plot")
               )
             )
    ),
    
    # SECOND TAB: Regression Plot
    tabPanel("Regression Plot",
             sidebarLayout(
               sidebarPanel(
                 
                 h5(strong("Cluster:")),
                 uiOutput("cluster_card"),  # Rendered from server
                 
                 
                 h5(strong("Confidence intervals")),
                 
                 checkboxInput("show_ci", "Show/ Hide Confidence Interval", value = TRUE),
                 helpText("This band shows a simple linear fit between the total revenue and the combined sum of selected expenses. This is not a multivariable regression line."),
                 checkboxGroupInput("selected_betas", 
                                    "Select Expenses (Predictors) :",
                                    choices = c(
                                      "Staff Salaries" = "Total Salaries From Worksheet A",
                                      "Wage-Related Costs (Benefits)" = "Wage-Related Costs (Core)",
                                      "Contract Labor" = "Contract Labor: Direct Patient Care",
                                      "Other Expenses" = "Total Other Expenses"
                                    ),
                                    selected = c(
                                      "Total Salaries From Worksheet A",
                                      "Wage-Related Costs (Core)",
                                      "Contract Labor: Direct Patient Care",
                                      "Total Other Expenses"
                                    )
                 ),
                 
                 #BETAS INPUTS
                 
                 br(),
                 h5(strong("Input Your Expense Values:")),
                 uiOutput("beta_inputs"),
                 actionButton("calc_expected", "Calculate Expected Revenue"),
                 uiOutput("expected_revenue_card")
                 
                 
                 
               ),
               
               
               mainPanel(
                 div(
                   style = "padding: 10px; background-color: white; border: 1px solid #ddd; border-radius: 8px; font-size: 14px; color: #333; margin-bottom: 20px;",
                   HTML("<b>Note:</b> This model explains the variance in hospital revenue using selected key operating expenses. 
                  It does not include other potential drivers.")
                 ),
                 
                 #Regression Equation above the plot
                 h4("Regression Equation:"),
                 verbatimTextOutput("regression_equation"),
                 
                 # R-squared Card
                 h5(strong("Model R-squared:")),
                 uiOutput("rsquared_card"),
                 
                 #Coefficients Comparison 
                 br(),
                 h4("Coefficient Comparison (Current Cluster):"),
                 plotlyOutput("coef_comparison_plot"),
                 
                 
                 #Regression Summary
                 br(),
                 h4("Regression Model Summary:"),
                 
                 tableOutput("regression_summary"),
                 
                 br(),
                 h4("Expected Revenue (Simulation):"),
                 uiOutput("expected_revenue_card"),
                 
                 
                 
                 #Regression PLot Based on Expenses 
                 
                 
                 h4("Regression Plot: Revenue vs Sum of Expenses"),
                 
                 # The updated plot
                 plotlyOutput("regression_plot")
               )
             )
    ),
    
    # THIRD TAB: Optimization
    tabPanel("Benckmark",
             sidebarLayout(
               sidebarPanel(
                 # Later: select hospitals to compare
               ),
               mainPanel(
                 #not sure yet!
                 h4("Selected Hospital Breakdown:"),
                 tableOutput("selected_hospital_info")
               )
             )
    ),    
    
    
    
    # FOURTH TAB: Optimization
    tabPanel("Optimization",
             sidebarLayout(
               sidebarPanel(
                 # Later: select hospitals to compare
               ),
               mainPanel(
                 # Later: optimization plot + tables
               )
             )
    ),
    
    # FIFTH TAB: Sensitivity Analysis
    tabPanel("Sensitivity Analysis",
             sidebarLayout(
               sidebarPanel(
                 # Later: slider input for % change
               ),
               mainPanel(
                 # Later: sensitivity plot/table
               )
             )
    )
  )
))
