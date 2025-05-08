library(shiny)


shinyUI(fluidPage(
  titlePanel("Hospital Benchmarking App"),
  
  #FIRST TAB
  
  tabsetPanel(
    tabPanel("Cluster Overview", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_cluster", 
                             "Select a Cluster:", 
                             choices = NULL)
               ),
               mainPanel(
                 h4("Number of hospitals in this cluster:"),
                 verbatimTextOutput("hospital_count"),
                 
                 
                 h4("Cluster Statistics:"),
                 tableOutput("hospital_stats"),
                 
                 
                 h4("Cluster Composition:"),
                 plotlyOutput("cluster_composition_plot"),
                 br(),
                 helpText("Facility Type Codes:"),
                 tags$ul(
                   tags$li("CAH = Critical Access Hospital"),
                   tags$li("CH = Children's Hospital"),
                   tags$li("LTCH = Long-Term Care Hospital"),
                   tags$li("PH = Psychiatric Hospital"),
                   tags$li("RH = Rehabilitation Hospital"),
                   tags$li("STH = Short-Term (Acute Care) Hospital")
                 ),
                 
                 br(),
                 h4("Hospitals by County:"),
                 plotlyOutput("county_breakdown_plot"),
                 
                 br(),
                 h4("Beds & Revenue per Hospital:"),
                 plotlyOutput("beds_bubble_plot")
                 
                 
                 ),
               

             
             )
    ),
    
    
    
    
    
    
    
    
    
    
    #SECOND TAB
    tabPanel("Regression Plot",
             sidebarLayout(
               sidebarPanel(
                 # later: confidence interval toggle
               ),
               mainPanel(
                 # later: regression plot + table
               )
             )
    ),
    #THIRD TAB
    
    tabPanel("Optimization",
             sidebarLayout(
               sidebarPanel(
                 # later: select hospitals to compare
               ),
               mainPanel(
                 # later: optimization plot + tables
               )
             )
    ),
    #FORTH TAB
    tabPanel("Sensitivity Analysis",
             sidebarLayout(
               sidebarPanel(
                 # later: slider input for % change
               ),
               mainPanel(
                 # later: sensitivity plot/table
               )
             )
    )
  )
))

