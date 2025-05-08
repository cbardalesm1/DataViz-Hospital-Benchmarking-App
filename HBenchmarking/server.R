
#libraries
library(shiny)
library(dplyr)
library(readr)
library(scales)
library(plotly)

# Load data once, globally
df <- read_csv("Data/CostReport_2022_Final_Clustered_type.csv")


shinyServer(function(input, output, session) {
  
  # Populate the cluster dropdown dynamically
  observe({
    clusters <- sort(unique(df$cluster))
    updateSelectInput(session, "selected_cluster",
                      choices = clusters)
  })
  
  # Reactive data filtered by selected cluster (Use User input to provide filter)
  filtered_data <- reactive({
    req(input$selected_cluster)
    df %>% filter(cluster == input$selected_cluster)
  })
  
  # Render hospital count (Provide the output to verbatimTexOutput)
  output$hospital_count <- renderText({
    nrow(filtered_data())
  })
  

  

  # Render Cluster Statistics
  output$hospital_stats <- renderTable({
    req(filtered_data())
    
    data <- filtered_data()
    
    stats <- data.frame(
      Metric = c(
        "Min Beds",
        "Max Beds",
        "Median Beds",
        "Mean Beds",
        "Total Revenue",
        "Average Revenue"
      ),
      Value = c(
        min(data$`Number of Beds`, na.rm = TRUE),
        max(data$`Number of Beds`, na.rm = TRUE),
        median(data$`Number of Beds`, na.rm = TRUE),
        round(mean(data$`Number of Beds`, na.rm = TRUE), 1),
        dollar(sum(data$`Total Patient Revenue`, na.rm = TRUE)),
        dollar(mean(data$`Total Patient Revenue`, na.rm = TRUE))
      )
    )
    
    stats
  })

  
  # CLUSTER COMPOSITION: Render Heatmap (Interactive)
  output$cluster_composition_plot <- renderPlotly({
    req(filtered_data())
    
    # Prepare the data: count hospitals per (Facility Type + Rural/Urban)
    heatmap_data <- filtered_data() %>%
      count(`CCN Facility Type`, `Rural Versus Urban`)
    
    p <- ggplot(heatmap_data, aes(x = `Rural Versus Urban`, y = `CCN Facility Type`,
                                  fill = n,
                                  text = paste("Facility Type:", `CCN Facility Type`,
                                               "<br>Rural/Urban:", `Rural Versus Urban`,
                                               "<br>Hospitals:", n))) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "beige", high = "darkorange") +
      labs(
        title = paste("Cluster", input$selected_cluster, "- Heatmap: Facility Type vs Rural/Urban"),
        x = "Rural vs Urban",
        y = "CCN Facility Type",
        fill = "Hospital Count"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "gray",      # Background of tooltip box
          font = list(color = "white")  # Text color inside tooltip
        )
      )
  })
  
  
  # COUNTY BREAKDOWN: Render County Barplot (Interactive)
  output$county_breakdown_plot <- renderPlotly({
    req(filtered_data())
    
    # Prepare data: count hospitals per county
    county_counts <- filtered_data() %>%
      count(County)
    
    p <- ggplot(county_counts, aes(x = reorder(County, -n), y = n, 
                                   text = paste("County:", County, "<br>Hospitals:", n))) +
      geom_bar(stat = "identity", fill = "orange") +
      labs(
        title = paste("Cluster", input$selected_cluster, "- Hospitals by County"),
        x = "County",
        y = "Number of Hospitals"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "gray",      # Background of tooltip box
          font = list(color = "white")  # Text color inside tooltip
        )
      )
  })
  
  
  # BEDS BUBBLE: Render Bubble Plot (Interactive)
  output$beds_bubble_plot <- renderPlotly({
    req(filtered_data())
    
    data <- filtered_data()
    
    p <- ggplot(data, aes(
      x = County,
      y = `Number of Beds`,
      size = `Number of Beds`,
      color = `Total Patient Revenue`,
      text = paste(
        "<b>Hospital:</b>", `Hospital Name`,
        "<br><b>County:</b>", County,
        "<br><b>Beds:</b>", `Number of Beds`,
        "<br><b>Revenue:</b> $", scales::comma(`Total Patient Revenue`)
      )
    )) +
      geom_point(alpha = 0.8) +
      scale_color_gradient(
        low = "beige",
        high = "darkorange",
        labels = scales::label_dollar(scale = 1e-9, suffix = "B")
      )+
      labs(
        title = paste("Cluster", input$selected_cluster, "- Beds & Revenue per Hospital"),
        x = "County",
        y = "Number of Beds",
        color = "Revenue",
        size = "Beds"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "gray",
          font = list(color = "white")
        )
      )
  })
  
  
  
  
})  