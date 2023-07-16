library(shiny)
library(ggplot2) |> suppressWarnings()



function(input, output, session) {
  
  app_data <- reactive({
    
    req(input$selected_year)
    
    if (stringr::str_detect(input$selected_year, "[:digit:]")) {
      
      dplyr::filter(
        mpg,
        year == as.numeric(input$selected_year)
      )
      
    } else {
      
      mpg
      
    }
    
  })

  
  # Character Count ------------------------------------------------------------
  output$char_count <- renderPlot({

    req(app_data, input$char_count_inp)

    if (!is.na(input$char_count_top_values)) {
      
      app_data() |>
        dplyr::count(.data[[input$char_count_inp]], sort = TRUE) |>
        head(input$char_count_top_values) |>
        
        ggplot(
          aes(x = n, y = reorder(.data[[input$char_count_inp]], n))
        ) +
        geom_col() +
        labs(y = input$char_count_inp) +
        theme_minimal()
      
    }

  })
  
  output$alert <- renderUI({

    if (is.na(input$char_count_top_values)) {

      tagList(
        shinyWidgets::alert(
          p(
            "The", span("`Top` numeric input", class = "fw-bold"), 
            "only allows values between 5 and 20.",
            span("Currently, there is no value entered.",class = "fs-5 fw-bolder")
          ),
          status = "danger",
          dismissible = TRUE
        )
      )

    }

  })
  
  # observe({
  #   
  #   if (is.na(input$char_count_top_values)) {
  #     
  #     shinyWidgets::show_alert(
  #       title = "Input Error !!",
  #       text = "Only values from 5 - 20 are allowed in the Top numeric input",
  #       type = "error"
  #     )
  #     
  #   }
  #   
  # }) 
  
  
  # Single Numeric Summary -----------------------------------------------------
  output$single_num_plot_ui <- renderUI({
    
    if (length(input$num_plot_type) == 1) {
      
      tagList(
        plotOutput("single_num_plot") |>
          shinyWidgets::addSpinner("fading-circle", "#9400D3")
      )
      
    } else {
      
      tagList(
        plotOutput("single_num_hist_plot") |>
          shinyWidgets::addSpinner("fading-circle", "#9400D3"),
        
        hr(),
        
        plotOutput("single_num_box_plot") |>
          shinyWidgets::addSpinner("fading-circle", "#9400D3")
      )
      
    }
    
  })
  
  
  output$zoom_slider_ui <- renderUI({
    
    req(app_data)
    
    min_value <- min(app_data()[input$single_num_variable])
    max_value <- max(app_data()[input$single_num_variable])
    
    tagList(
      sliderInput(
        "zoom_range",
        label = "Zoom-in (X-axis)",
        min = min_value,
        max = max_value,
        value = c(min_value, max_value)
      )
    )
    
  })
  
  
  output$single_num_plot <- renderPlot({
    
    if (length(input$num_plot_type) == 1) {
      
      fig <- ggplot(app_data(), aes(x = .data[[input$single_num_variable]]))
      
      if (input$num_plot_type == "Histogram") {
        
        fig <- fig + geom_histogram(bins = input$bins)
        
      } else {
        
        fig <- fig + geom_boxplot()
        
      }
      
      fig + 
        coord_cartesian(xlim = input$zoom_range) +
        theme_minimal()
      
    }
    
  })
  
  
  output$single_num_hist_plot <- renderPlot({
    
    if (length(input$num_plot_type) == 2) {
      
      app_data() |>
        ggplot(aes(x = .data[[input$single_num_variable]])) +
        geom_histogram(bins = input$bins) +
        coord_cartesian(xlim = input$zoom_range) +
        theme_minimal()
  
    }
  })
  
  
  output$single_num_box_plot <- renderPlot({

    if (length(input$num_plot_type) == 2) {

      app_data() |>
        ggplot(aes(x = .data[[input$single_num_variable]])) +
        geom_boxplot() +
        coord_cartesian(xlim = input$zoom_range) +
        theme_minimal()

    }

  })
  
  
  # Two numeric summary --------------------------------------------------------
  output$include_char_input_ui <- renderUI({
    
    if (input$inclued_character_variable) {
      
      char_cols <- dplyr::select(app_data(), dplyr::where(is.character)) |>
        names()
      
      shinyWidgets::virtualSelectInput(
        inputId = "two_num_char_variable",
        label = "Select",
        choices = char_cols,
        search = TRUE
      )
    }
    
  })
  
  
  output$two_num_plot <- renderPlot({
    
    req(input$two_num_variables)
    
    selected_values <- input$two_num_variables
    selected_values <- selected_values[1:2]
    
    if (!anyNA(selected_values)) {

      if (input$inclued_character_variable & !is.null(input$two_num_char_variable)) {
        
        fig <- app_data() |>
          ggplot(
            aes(x = .data[[selected_values[1]]], 
                y = .data[[selected_values[2]]],
                color = .data[[input$two_num_char_variable]])
          ) +
          geom_point()
        
      } else {
        
        fig <- app_data() |>
          ggplot(
            aes(x = .data[[selected_values[1]]], y = .data[[selected_values[2]]])
          ) +
          geom_point()
        
      }
      
      fig + theme_minimal()
      
    }
    
  })
  
  # Statistics -----------------------------------------------------------------
  
  output$stats_cards <- renderUI({
    
    if (!is.null(input$show_stats)) {
      
      if (input$show_stats == "manufacturer") {
        
        m_value <- app_data()$manufacturer |> unique() |> length()
        
        shinyWidgets::statiCard(
          value = m_value,
          subtitle = "Total Manufacturer",
          icon = icon("cogs"),
          color = "#9400D3",
          animate = TRUE,
          id="m_stat_card"
        )
        
      } else if (input$show_stats == "city") {
        
        shinyWidgets::statiCard(
          value = mean(app_data()$cty, na.rm = TRUE) |> round(3),
          subtitle = "Average City MPG",
          icon = icon("arrow-trend-down"),
          color = "#CD1076",
          animate = TRUE,
          id="city_stat_card"
        )
        
      } else {
        
        shinyWidgets::statiCard(
          value = mean(app_data()$hwy, na.rm = TRUE) |> round(3),
          subtitle = "Average Highway MPG",
          icon = icon("arrow-trend-up"),
          color = "#00CED1",
          animate = TRUE,
          id="highway_stat_card"
        )
        
      }
      
    }
  })
  
}
