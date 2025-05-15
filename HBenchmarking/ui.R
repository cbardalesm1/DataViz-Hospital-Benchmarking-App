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
                 br(),
                 
                 h5(strong("Confidence intervals")),
                 
                 checkboxInput("show_ci", "Show/ Hide Confidence Interval", value = TRUE),
                 helpText("This band shows a simple linear fit between the total revenue and the combined sum of selected expenses. This is not a multivariable regression line."),
                 br(),
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
    
    # THIRD TAB: Benchmark (Stomatic Frontier Analysis)
    tabPanel("Benchmark-SFA",
             sidebarLayout(
               sidebarPanel(
                 
                 #Cluster Card Tab 3
                 h5(strong("Cluster:")),
                 uiOutput("cluster_card_tab3"),
                 
                 #Select drop down
                 
                 br(),
                 h5(strong("Compare Hospitals:")),
                 br(),
                 selectInput("hospital_1", "Select Hospital 1:", choices = NULL),
                 selectInput("hospital_2", "Select Hospital 2:", choices = NULL)
                 
                 
               ),
               mainPanel(
                 
                 #Efficiency plot
                 h4("Efficiency Plot: Actual vs Predicted Revenue"),
                 plotlyOutput("efficiency_plot"),
                 
                 #TOP 10% Efficiency Table
                 h4("Top 10% Efficient Hospitals"),
                 tableOutput("top_efficiency_table"),
                 br(),
                 
                 #RADAR PLOT
                 br(),
                 h4("Radar Plot: Hospital Comparison"),
                 plotlyOutput("radar_plot")
                 
                 
               )
             )
    ),    
    
    
    
    # FOURTH TAB: Benchmark Data Envvelopment Analysis
    # FOURTH TAB: Benchmark - DEA
    tabPanel("Benchmark - DEA",
             sidebarLayout(
               sidebarPanel(
                 # Cluster Selection
                 h5(strong("Select Cluster:")),
                 selectInput("dea_cluster", "Cluster:", choices = NULL),
                 
                 br(),
                 # Download Button
                 downloadButton("download_dea_data", "Download DEA Scores (.csv)")
               ),
               mainPanel(
                 h4("DEA Input-Oriented Efficiency (Minimize expenses)"),
                 plotlyOutput("dea_input_plot"),
                 br(),
                 
                 # h4("DEA Input-Oriented Frontier Hospitals"),
                 # tableOutput("dea_input_frontier_table"),
                 # br(),
                 
                 h4("DEA Output-Oriented Efficiency (Maximize revenue)"),
                 plotlyOutput("dea_output_plot"),
                 br(),
                 
                 h4("DEA Frontier Hospitals"),
                 tableOutput("dea_output_frontier_table")
               )
             )
    )
    
    
    
  )
))


