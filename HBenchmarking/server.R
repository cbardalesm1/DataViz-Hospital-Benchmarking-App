

library(shiny)
library(dplyr)
library(readr)
library(scales)
library(ggplot2)
library(plotly)
library(tidyr) 
library(stringr)
library(Benchmarking)
# Load data once, globally
df <- read_csv("Data/CostReport_2022_Final_Clustered_type.csv")

shinyServer(function(input, output, session) {
  
  print("erver loaded successfully!")
  #TAB 1
  
  # Populate the cluster dropdown dynamically
  observe({
    clusters <- sort(unique(df$cluster))
    updateSelectInput(session, "selected_cluster",
                      choices = clusters)
  })
  
  # Reactive data filtered by selected cluster
  filtered_data <- reactive({
    req(input$selected_cluster)
    df %>% filter(cluster == input$selected_cluster)
  })
  
  # Render hospital count
  output$hospital_count <- renderText({
    nrow(filtered_data())
  })
  
  # Render Cluster Statistics
  output$hospital_stats <- renderTable({
    req(filtered_data())
    
    data <- filtered_data()
    
    stats <- data.frame(
      Metric = c(
        "Number of Hospitals",
        "Min Beds",
        "Max Beds",
        "Median Beds",
        "Mean Beds",
        "Total Revenue",
        "Average Revenue",
        "Total Expenses",
        "Average Revenue per Bed"
      ),
      Value = c(
        nrow(data),
        min(data$`Number of Beds`, na.rm = TRUE),
        max(data$`Number of Beds`, na.rm = TRUE),
        median(data$`Number of Beds`, na.rm = TRUE),
        round(mean(data$`Number of Beds`, na.rm = TRUE), 1),
        scales::dollar(sum(data$`Total Patient Revenue`, na.rm = TRUE)),
        scales::dollar(mean(data$`Total Patient Revenue`, na.rm = TRUE)),
        scales::dollar(sum(data$`Total Salaries From Worksheet A` + 
                             data$`Wage-Related Costs (Core)` +
                             data$`Contract Labor: Direct Patient Care` +
                             data$`Total Other Expenses`, na.rm = TRUE)),
        scales::dollar(
          sum(data$`Total Patient Revenue`, na.rm = TRUE) /
            sum(data$`Number of Beds`, na.rm = TRUE)
        )
      )
    )
    
    stats
  })
  
  # Cluster Composition: Heatmap (Interactive)
  output$cluster_composition_plot <- renderPlotly({
    req(filtered_data())
    
    heatmap_data <- filtered_data() %>%
      count(`CCN Facility Type`, `Rural Versus Urban`)
    
    p <- ggplot(heatmap_data, aes(x = `Rural Versus Urban`, y = `CCN Facility Type`,
                                  fill = n,
                                  text = paste(
                                    "Facility Type:", `CCN Facility Type`,
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
          bgcolor = "gray",
          font = list(color = "white")
        )
      )
  })
  
  # County Breakdown: Barplot (Interactive)
  output$county_breakdown_plot <- renderPlotly({
    req(filtered_data())
    
    county_counts <- filtered_data() %>%
      count(County)
    
    p <- ggplot(county_counts, aes(x = reorder(County, -n), y = n,
                                   text = paste(
                                     "County:", County,
                                     "<br>Hospitals:", n))) +
      geom_bar(stat = "identity", fill = "darkorange") +
      #geom_text(aes(label = n), vjust = -0.5, size = 3) +
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
          bgcolor = "gray",
          font = list(color = "white")
        )
      )
  })
  
  # Beds Bubble: Bubble Plot (Interactive)
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
        labels = scales::label_dollar(scale = 1e-6, suffix = "M")
        # or use 1e-9, suffix = "B" if you prefer billions
      ) +
      labs(
        title = paste("Cluster", input$selected_cluster, "- Beds & Revenue per Hospital"),
        x = "County",
        y = "Number of Beds",
        color = "Total Revenue",
        size = "Number of Beds"
      ) +
      guides(
        color = guide_legend(order = 1),
        size = guide_legend(order = 2)
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.background = element_rect(fill = "gray95", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
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
  
  
  #TAB 2
  
  #Cluster Card 
  output$cluster_card <- renderUI({
    req(input$selected_cluster)
    
    div(
      style = "padding: 10px; background-color: white; border: 1px solid #ddd; border-radius: 8px; font-size: 16px; color: #333;",
      paste( input$selected_cluster)
    )
  })
  
  
  
  
  
  #REGRESSION LINE, CI, KIND OF REGRESSION GRAPHIC
  
  #REGRESSION EQUATION (TOP)
  
  # Dynamic regression model based on selected predictors
  regression_model <- reactive({
    req(input$selected_cluster)
    req(input$selected_betas)
    
    data_cluster <- df %>% filter(cluster == input$selected_cluster)
    predictors <- input$selected_betas
    if (length(predictors) == 0) return(NULL)
    
    formula_text <- paste("`Total Patient Revenue` ~", paste0("`", predictors, "`", collapse = " + "))
    lm(as.formula(formula_text), data = data_cluster)
  })
  
  # Show regression equation dynamically
  output$regression_equation <- renderText({
    model <- regression_model()
    req(model)
    
    coefs <- coef(model)
    predictors <- names(coefs)[-1]
    
    eq_parts <- paste0(
      round(coefs[-1], 3), " * ", predictors
    )
    eq_text <- paste("Revenue =", round(coefs[1], 0), "+", paste(eq_parts, collapse = " + "))
    eq_text
  })
  
  ##GRAPHIC
  
  output$regression_plot <- renderPlotly({
    req(filtered_data())
    req(input$selected_betas)
    
    print(paste("Rendering plot - Show Line:", input$show_line, "Show CI:", input$show_ci))
    
    # Get model & data
    model <- regression_model()
    req(model)
    
    data <- filtered_data()
    predictors <- input$selected_betas
    req(predictors)
    
    # Sum of expenses
    data$sum_expenses <- rowSums(data[, predictors, drop = FALSE])
    
    # redicted revenue from the real model
    data$predicted_revenue <- predict(model, newdata = data)
    
    # uild tooltip
    tooltip_vec <- sapply(1:nrow(data), function(i) {
      hospital <- data$`Hospital Name`[i]
      expense_breakdown <- paste(
        sapply(predictors, function(beta) {
          if (beta %in% colnames(data)) {
            paste0("<b>", beta, ":</b> $", scales::comma(data[[beta]][i]))
          } else {
            paste0("<b>", beta, ":</b> (Not Available)")
          }
        }),
        collapse = "<br>"
      )
      paste0(
        "<b>Hospital:</b> ", hospital, "<br>",
        expense_breakdown, "<br>",
        "<b>Total Revenue:</b> $", scales::comma(data$`Total Patient Revenue`[i])
      )
    })
    data$tooltip_custom <- as.character(tooltip_vec)
    
    # Build base plot
    p <- ggplot(data, aes(
      x = sum_expenses,
      y = `Total Patient Revenue`,
      text = tooltip_custom
    )) +
      geom_point(color = "darkorange", size = 3, alpha = 0.7) +
      labs(
        title = paste("Sum of Expenses (Only Predictors Selected) vs. Total Patient Revenue - Cluster", input$selected_cluster),
        x = "Sum of Expenses",
        y = "Total Patient Revenue"
      ) +
      scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
      scale_y_continuous(labels = scales::label_dollar(scale = 1e-9, suffix = "B")) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    
    if (isTRUE(input$show_ci)) {
      p <- p + geom_smooth(
        data = data,
        aes(x = sum_expenses, y = `Total Patient Revenue`),
        method = "lm",
        se = TRUE,
        color = "gray40",
        linetype = "dotdash",
        size = 1,
        inherit.aes = FALSE
      )
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "gray",
          font = list(color = "white")
        )
      )
  })
  #RSquared
  
  output$rsquared_card <- renderUI({
    model <- regression_model()
    req(model)
    
    r2 <- summary(model)$r.squared
    
    div(
      style = "padding: 10px; background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 8px;",
      paste0( round(r2 * 100, 1), "%")
    )
  })
  
  
  ##SUMMARY OF REGRESSION EQUATION:
  output$regression_summary <- renderTable({
    model <- regression_model()
    req(model)
    
    summary_model <- summary(model)
    
    # Extract coefs, p-values, and CIs
    coefs <- coef(summary_model)
    conf_int <- confint(model)
    
    # Build table
    table_data <- data.frame(
      Beta = rownames(coefs),
      Estimate = round(coefs[, "Estimate"], 3),
      `Std. Error` = round(coefs[, "Std. Error"], 3),
      #`t value` = round(coefs[, "t value"], 3),
      `P-value` = signif(coefs[, "Pr(>|t|)"], 3),
      `CI Lower` = round(conf_int[, 1], 3),
      `CI Upper` = round(conf_int[, 2], 3),
      stringsAsFactors = FALSE
    )
    
    table_data
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  
  
  output$beta_inputs <- renderUI({
    req(input$selected_betas)
    
    tagList(
      h5(strong("Custom Beta Inputs:")),
      helpText("Enter numeric values for each selected predictor to simulate expected revenue."),
      lapply(input$selected_betas, function(beta_name) {
        div(
          style = "margin-bottom: 10px; width: 80%;",  # shrink width + spacing
          numericInput(
            inputId = paste0("input_", make.names(beta_name)),
            label = tags$div(style = "text-align: left;", beta_name),  # ✅ left-align label
            value = 0,
            min = 0,
            width = '100%'  # make sure the input box fills the div
          )
        )
      })
    )
  })
  ##Trigger for Expected revenue:
  
  # Reactive trigger when Calculate button is clicked
  expected_revenue_calc <- eventReactive(input$calc_expected, {
    model <- regression_model()
    if (is.null(model) || length(input$selected_betas) == 0) {
      return(NULL)
    }
    
    intercept <- coef(model)[1]
    predictors <- input$selected_betas
    
    expected_value <- intercept
    
    for (beta_name in predictors) {
      input_id <- paste0("custom_", gsub("[^[:alnum:]]", "", beta_name))
      user_value <- input[[input_id]]
      coef_value <- coef(model)[[beta_name]]
      
      if (!is.null(user_value) && !is.na(user_value)) {
        expected_value <- expected_value + (coef_value * user_value)
      }
    }
    
    expected_value
  })
  
  
  
  ##Output Expected Revenue 
  
  # Event reactive that computes expected revenue when the button is clicked
  expected_revenue_calc <- eventReactive(input$calc_expected, {
    model <- regression_model()
    if (is.null(model) || length(input$selected_betas) == 0) {
      return(NULL)
    }
    
    intercept <- coef(model)[1]
    predictors <- input$selected_betas
    expected_value <- intercept
    
    # Clean coef names (remove backticks for matching)
    coef_names_clean <- gsub("`", "", names(coef(model)))
    
    for (beta_name in predictors) {
      input_id <- paste0("custom_", gsub("[^[:alnum:]]", "", beta_name))
      user_value <- input[[input_id]]
      
      # Safely match the coef name
      match_idx <- which(coef_names_clean == beta_name)
      if (length(match_idx) == 0) {
        next  # Skip if no match found
      }
      
      coef_value <- coef(model)[match_idx]
      
      if (!is.null(user_value) && !is.na(user_value)) {
        expected_value <- expected_value + (coef_value * user_value)
      }
    }
    
    expected_value
  })
  
  # Render the expected revenue card in the UI
  output$expected_revenue_card <- renderUI({
    expected_value <- expected_revenue_calc()
    if (is.null(expected_value)) {
      return(NULL)
    }
    div(
      style = "padding: 10px; background-color: white; border: 1px solid #ddd; border-radius: 8px; font-size: 16px; color: #333;",
      paste0("Expected Revenue: $", scales::comma(round(expected_value, 0)))
    )
  })
  
  #OutputRevenue Card!!
  output$expected_revenue_card <- renderUI({
    expected_value <- expected_revenue_calc()
    if (is.null(expected_value)) {
      return(NULL)
    }
    div(
      style = "
      padding: 10px;
      background-color: #f5f5f5;  /* Light gray background */
      border: 1px solid #ddd;
      border-radius: 8px;
      color: #333;                /* Dark gray text */
    ",
      paste0("Expected Revenue: $", scales::comma(round(expected_value, 0)))
    )
  })
  
  #Comparison Coefficients
  output$coef_comparison_plot <- renderPlotly({
    model <- regression_model()
    req(model)
    
    # Get coefficients and clean up (skip intercept)
    coefs <- broom::tidy(model) %>%
      filter(term != "(Intercept)")
    
    # Order: keep same order as in the regression table
    coefs$term <- factor(coefs$term, levels = coefs$term)
    
    # Debug print
    print("Coefficient data (full):")
    print(coefs)
    
    # Plot: keep same color scale tan to darkorange, no x-axis angle, ordered x
    p <- ggplot(coefs, aes(x = term, y = estimate, fill = estimate, text = paste0(
      "<b>Expense:</b> ", term,
      "<br><b>Coefficient:</b> ", round(estimate, 2),
      "<br><b>p-value:</b> ", signif(p.value, 3)
    ))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_gradient(low = "beige", high = "darkorange") +  
      labs(
        title = paste("Coefficient Comparison - Cluster", input$selected_cluster),
        x = "Expense Type",
        y = "Estimated Effect on Revenue"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"  
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "gray",
          font = list(color = "white")
        )
      )
  })
  ##TAB 3
  
  ###Stochastic Frontier Model using log-log regression
  
  
  #Model with   NO STANDARIZATION
  
  no_standard_model <- reactive({
    req(input$selected_cluster, input$selected_betas)
    
    data_cluster <- df %>% filter(cluster == input$selected_cluster)
    predictors <- input$selected_betas
    
    formula_text <- paste("`Total Patient Revenue` ~", paste0("`", predictors, "`", collapse = " + "))
    lm(as.formula(formula_text), data = data_cluster)
  })
  
  
  
  #Plot
  
  
  output$efficiency_plot <- renderPlotly({
    # Model and filtered data
    model <- no_standard_model()
    data <- df %>% filter(cluster == input$selected_cluster)
    
    # Compute predicted and actual revenue
    data$predicted <- predict(model, newdata = data)
    data$actual <- data$`Total Patient Revenue`
    
    # Efficiency metrics
    data$efficiency_score <- data$actual / data$predicted
    data$rank <- rank(-data$efficiency_score, ties.method = "min")
    
    # Plot
    p <- ggplot(data, aes(
      x = predicted,
      y = actual,
      text = paste0(
        "<b>Hospital:</b> ", `Hospital Name`, "<br>",
        "<b>Actual:</b> $", scales::comma(actual, 1), "<br>",
        "<b>Predicted:</b> $", scales::comma(predicted, 1), "<br>",
        "<b>Efficiency Score:</b> ", round(efficiency_score, 3), "<br>",
        "<b>Efficiency Rank:</b> ", rank
      )
    )) +
      geom_point(color = "darkorange", alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, color = "gray40", linetype = "dashed") +
      labs(
        title = "Actual vs Predicted Revenue",
        x = "Predicted Revenue",
        y = "Actual Revenue"
      ) +
      scale_x_continuous(labels = scales::label_dollar(scale = 1e-9, suffix = "B")) +
      scale_y_continuous(labels = scales::label_dollar(scale = 1e-9, suffix = "B")) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "gray", font = list(color = "white"))
      )
  })
  
  
  output$cluster_card_tab3 <- renderUI({
    req(input$selected_cluster)
    div(
      style = "padding: 10px; background-color: white; border: 1px solid #ddd; border-radius: 8px; font-size: 16px; color: #333;",
      input$selected_cluster
    )
  })
  
  #TOP 10% Efficciency Table
  
  output$efficiency_top10_table <- renderTable({
    model <- no_standard_model()
    data <- df %>% filter(cluster == input$selected_cluster)
    
    data$predicted <- predict(model, newdata = data)
    data$actual <- data$`Total Patient Revenue`
    data$efficiency_score <- data$actual / data$predicted
    
    # Rank and filter top 10%
    data <- data %>%
      mutate(rank = rank(-efficiency_score, ties.method = "min")) %>%
      arrange(rank)
    
    top_n <- ceiling(0.10 * nrow(data))  # Top 10%
    top_10 <- head(data, top_n)
    
    # Return only desired columns
    top_10 %>%
      select(`Hospital Name`, County, efficiency_score) %>%
      mutate(efficiency_score = round(efficiency_score, 3))
  })
  
  
  
  # Reactive: Table with Top 10% Efficient Hospitals
  top_efficient_hospitals <- reactive({
    model <- no_standard_model()
    data <- df %>% filter(cluster == input$selected_cluster)
    
    data$predicted <- predict(model, newdata = data)
    data$actual <- data$`Total Patient Revenue`
    data$efficiency_score <- data$actual / data$predicted
    
    # Top 10% threshold
    threshold <- quantile(data$efficiency_score, 0.9, na.rm = TRUE)
    
    data %>%
      filter(efficiency_score >= threshold) %>%
      arrange(desc(efficiency_score)) %>%
      select(`Hospital Name`, County, `Efficiency Score` = efficiency_score)%>%
      mutate(`Efficiency Score` = round(`Efficiency Score`, 2))
  })
  
  output$top_efficiency_table <- renderTable({
    top_efficient_hospitals()
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  #COMPARISON BETWEEN 2 HOSPITALS
  
  # Update hospital dropdowns for comparison
  observe({
    req(filtered_data())
    hospital_names <- filtered_data()$`Hospital Name`
    
    updateSelectInput(session, "hospital_1", choices = hospital_names)
    updateSelectInput(session, "hospital_2", choices = hospital_names)
  })
  
  #RADAR Plot
  output$radar_plot <- renderPlotly({
    req(input$hospital_1, input$hospital_2)
    data <- filtered_data()
    predictors <- input$selected_betas
    req(predictors)
    
    # Subset the two hospitals
    df_compare <- data %>%
      filter(`Hospital Name` %in% c(input$hospital_1, input$hospital_2)) %>%
      select(`Hospital Name`, all_of(predictors))
    
    # Normalize each predictor using range from full cluster data
    df_scaled <- df_compare
    for (col in predictors) {
      col_range <- range(data[[col]], na.rm = TRUE)
      df_scaled[[col]] <- (df_scaled[[col]] - col_range[1]) / (col_range[2] - col_range[1])
    }
    
    # Combine scaled and raw data for tooltip
    df_scaled$hospital <- df_scaled$`Hospital Name`
    df_compare$hospital <- df_compare$`Hospital Name`
    
    df_long_scaled <- pivot_longer(df_scaled, cols = all_of(predictors), names_to = "Metric", values_to = "Scaled")
    df_long_raw <- pivot_longer(df_compare, cols = all_of(predictors), names_to = "Metric", values_to = "Raw")
    
    df_final <- df_long_scaled %>%
      left_join(df_long_raw, by = c("hospital", "Metric")) %>%
      rename(`Hospital Name` = hospital)
    
    df_final$Metric <- gsub("Contract Labor: Direct Patient Care", "Contract Labor<br>Direct Patient Care", df_final$Metric)
    df_final$Metric <- gsub("Total Salaries From Worksheet A", "Total Salaries<br>From Worksheet A", df_final$Metric)
    
    # Plot
    plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
      add_trace(
        r = df_final$Scaled[df_final$`Hospital Name` == input$hospital_1],
        theta = df_final$Metric[df_final$`Hospital Name` == input$hospital_1],
        name = input$hospital_1,
        fill = 'toself',
        opacity = 0.5,
        text = paste0(
          "<b>", df_final$Metric[df_final$`Hospital Name` == input$hospital_1], "</b><br>",
          "Value: $", scales::comma(df_final$Raw[df_final$`Hospital Name` == input$hospital_1])
        ),
        hoverinfo = 'text'
      ) %>%
      add_trace(
        r = df_final$Scaled[df_final$`Hospital Name` == input$hospital_2],
        theta = df_final$Metric[df_final$`Hospital Name` == input$hospital_2],
        name = input$hospital_2,
        fill = 'toself',
        opacity = 0.5,
        text = paste0(
          "<b>", df_final$Metric[df_final$`Hospital Name` == input$hospital_2], "</b><br>",
          "Value: $", scales::comma(df_final$Raw[df_final$`Hospital Name` == input$hospital_2])
        ),
        hoverinfo = 'text'
      ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 1))
        ),
        showlegend = TRUE
      )
  })
  
  ##TAB 4 DEAA
  
  #Button 
  observe({
    clusters <- sort(unique(df$cluster))
    updateSelectInput(session, "dea_cluster", choices = clusters)
  })
  
  
  # DEA reactive results
  dea_results <- reactive({
    req(input$selected_cluster_tab4)
    
    # Filter data by selected cluster
    data_cluster <- df %>% filter(cluster == input$selected_cluster_tab4)
    
    # Define inputs and outputs
    inputs <- data_cluster %>%
      select(
        `Total Salaries From Worksheet A`,
        `Wage-Related Costs (Core)`,
        `Contract Labor: Direct Patient Care`,
        `Total Other Expenses`
      ) %>%
      as.matrix()
    
    outputs <- data_cluster %>%
      select(`Total Patient Revenue`) %>%
      as.matrix()
    
    # Compute DEA input-oriented efficiency
    input_dea <- dea(X = inputs, Y = outputs, ORIENTATION = "in", RTS = "vrs")
    output_dea <- dea(X = inputs, Y = outputs, ORIENTATION = "out", RTS = "vrs")
    
    # Add results to data
    data_cluster$DEA_Input <- round(input_dea$eff, 3)
    data_cluster$DEA_Output <- round(output_dea$eff, 3)
    
    data_cluster
  })
  
  #DEA score computation 
  # Reactive DEA dataset based on selected cluster
  dea_data <- reactive({
    req(input$dea_cluster)
    
    predictors <- c(
      "Total Salaries From Worksheet A",
      "Wage-Related Costs (Core)",
      "Contract Labor: Direct Patient Care",
      "Total Other Expenses"
    )
    
    output_var <- "Total Patient Revenue"
    
    data_cluster <- df %>%
      filter(cluster == input$dea_cluster) %>%
      select(`Hospital Name`, County, all_of(predictors), all_of(output_var)) %>%
      drop_na()
    
    # DEA Input-Oriented (minimize expenses)
    dea_input <- dea(X = as.matrix(data_cluster[, predictors]),
                     Y = as.matrix(data_cluster[, output_var]),
                     ORIENTATION = "in")
    
    # DEA Output-Oriented (maximize revenue)
    dea_output <- dea(X = as.matrix(data_cluster[, predictors]),
                      Y = as.matrix(data_cluster[, output_var]),
                      ORIENTATION = "out")
    
    # Add scores to dataset
    data_cluster$DEA_Input <- round(dea_input$eff, 3)
    data_cluster$DEA_Output <- round(dea_output$eff, 3)
    
    return(data_cluster)
  })
  
  # Step B2: Reactive DEA Data
  dea_data <- reactive({
    req(input$dea_cluster)
    
    df %>%
      filter(cluster == input$dea_cluster) %>%
      select(`Hospital Name`, County,
             `Total Salaries From Worksheet A`,
             `Wage-Related Costs (Core)`,
             `Contract Labor: Direct Patient Care`,
             `Total Other Expenses`,
             `Total Patient Revenue`)
  })
  
  #DEA efficiency calculations
  dea_scores <- reactive({
    data <- dea_data()
    
    inputs <- as.matrix(data[, 3:6])
    outputs <- as.matrix(data[, 7, drop = FALSE])
    
    # Input-oriented
    dea_in <- dea(inputs, outputs, ORIENTATION = "in")
    data$SFA_input <- round(dea_in$eff, 2)
    
    # Output-oriented
    dea_out <- dea(inputs, outputs, ORIENTATION = "out")
    data$SFA_output <- round(dea_out$eff, 2)
    
    data
  })
  #PLOT INPUT
  
  output$dea_input_plot <- renderPlotly({
    data <- dea_scores()
    inputs <- as.matrix(data[, 3:6])  # Expenses
    outputs <- as.matrix(data[, 7, drop = FALSE])  # Revenue
    
    dea_in <- dea(inputs, outputs, ORIENTATION = "in")
    is_efficient <- dea_in$eff == 1
    
    df_plot <- data.frame(
      Input = rowSums(inputs),
      Output = outputs[, 1],
      Hospital = data$`Hospital Name`,
      Efficient = is_efficient
    )
    
    frontier <- df_plot %>%
      filter(Efficient) %>%
      arrange(Output)  # sort by x-axis
    
    p <- ggplot(df_plot, aes(x = Output, y = Input)) +
      geom_point(aes(
        color = Efficient,
        text = paste0(
          "<b>Hospital:</b> ", Hospital, "<br>",
          "<b>Output (Revenue):</b> $", scales::comma(Output), "<br>",
          "<b>Input (Expenses):</b> $", scales::comma(Input)
        )
      ), size = 3, alpha = 0.7) +
      geom_line(data = frontier, aes(x = Output, y = Input), color = "black", linetype = "dashed") +
      scale_color_manual(values = c("gray", "darkorange")) +
      scale_x_continuous(labels = scales::label_dollar(scale = 1e-9, suffix = "B")) +
      scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
      labs(
        title = "DEA Input-Oriented Efficiency Frontier",
        subtitle = "Orientation: Input — minimizing expenses",
        x = "Total Output (Revenue)",
        y = "Total Inputs (Expenses)"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "gray", font = list(color = "white")))
  })
  
  
  #PLOP OUUTPUT
  
  output$dea_output_plot <- renderPlotly({
    data <- dea_scores()
    inputs <- as.matrix(data[, 3:6])  # Expenses
    outputs <- as.matrix(data[, 7, drop = FALSE])  # Revenue
    
    dea_out <- dea(inputs, outputs, ORIENTATION = "out")
    is_efficient <- dea_out$eff == 1
    
    df_plot <- data.frame(
      Input = rowSums(inputs),
      Output = outputs[, 1],
      Hospital = data$`Hospital Name`,
      Efficient = is_efficient
    )
    
    frontier <- df_plot %>%
      filter(Efficient) %>%
      arrange(Input)  # sort by x-axis
    
    p <- ggplot(df_plot, aes(x = Input, y = Output)) +
      geom_point(aes(
        color = Efficient,
        text = paste0(
          "<b>Hospital:</b> ", Hospital, "<br>",
          "<b>Input (Expenses):</b> $", scales::comma(Input), "<br>",
          "<b>Output (Revenue):</b> $", scales::comma(Output)
        )
      ), size = 3, alpha = 0.7) +
      geom_line(data = frontier, aes(x = Input, y = Output), color = "black", linetype = "dashed") +
      scale_color_manual(values = c("gray", "darkorange")) +
      scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
      scale_y_continuous(labels = scales::label_dollar(scale = 1e-9, suffix = "B")) +
      labs(
        title = "DEA Output-Oriented Efficiency Frontier",
        subtitle = "Orientation: Output — maximizing revenue",
        x = "Total Inputs (Expenses)",
        y = "Total Output (Revenue)"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "gray", font = list(color = "white")))
  })
  
  #DEA Efficiency tables
  # DEA Input-Oriented Frontier Table
  output$dea_input_frontier_table <- renderTable({
    data <- dea_scores()
    inputs <- as.matrix(data[, 3:6])
    outputs <- as.matrix(data[, 7, drop = FALSE])
    
    dea_in <- dea(inputs, outputs, ORIENTATION = "in")
    
    df <- data.frame(
      Hospital = data$`Hospital Name`,
      County = data$County,
      DEA_Input_Efficiency = round(dea_in$eff, 3)
    )
    
    df %>% filter(DEA_Input_Efficiency == 1)
  })
  
  
  # DEA Output-Oriented Frontier Table
  output$dea_output_frontier_table <- renderTable({
    data <- dea_scores()
    inputs <- as.matrix(data[, 3:6])
    outputs <- as.matrix(data[, 7, drop = FALSE])
    
    dea_out <- dea(inputs, outputs, ORIENTATION = "out")
    
    df <- data.frame(
      Hospital = data$`Hospital Name`,
      County = data$County,
      DEA_Output_Efficiency = round(dea_out$eff, 3)
    )
    
    df %>% filter(DEA_Output_Efficiency == 1)
  })
  
  
  #Download
  output$download_dea_data <- downloadHandler(
    filename = function() {
      paste0("DEA_SFA_Scores_Cluster_", input$dea_cluster, ".csv")
    },
    content = function(file) {
      # Filter cluster
      data <- df %>% filter(cluster == input$dea_cluster)
      
      # DEA inputs and outputs
      inputs <- as.matrix(data[, c(
        "Total Salaries From Worksheet A",
        "Wage-Related Costs (Core)",
        "Contract Labor: Direct Patient Care",
        "Total Other Expenses"
      )])
      
      outputs <- as.matrix(data[, "Total Patient Revenue", drop = FALSE])
      
      # DEA models
      dea_in <- dea(inputs, outputs, ORIENTATION = "in")
      dea_out <- dea(inputs, outputs, ORIENTATION = "out")
      
      # SFA model using original scale
      sfa_model <- lm(`Total Patient Revenue` ~
                        `Total Salaries From Worksheet A` +
                        `Wage-Related Costs (Core)` +
                        `Contract Labor: Direct Patient Care` +
                        `Total Other Expenses`, data = data)
      
      data$Predicted_Revenue <- predict(sfa_model, newdata = data)
      data$SFA_Efficiency <- round(data$`Total Patient Revenue` / data$Predicted_Revenue, 3)
      
      # Add DEA scores
      data$DEA_Input_Efficiency <- round(dea_in$eff, 3)
      data$DEA_Output_Efficiency <- round(dea_out$eff, 3)
      
      # Drop predicted column (optional)
      data <- data %>% select(-Predicted_Revenue)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  
  
})









