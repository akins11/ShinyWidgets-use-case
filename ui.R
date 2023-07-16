library(shiny)


mpg <- ggplot2::mpg

char_cols <- dplyr::select(mpg, dplyr::where(is.character)) |> names()

num_cols <- dplyr::select(mpg, dplyr::where(is.numeric)) |> names()
num_cols <- num_cols[num_cols != "year"]



col_layout <- function(...) {
  
  bslib::layout_columns(
    col_widths = c(-1, 3, 7, -1),
    fillable = FALSE,
    
    ...
  )
  
}

container <- function(...) {
  
  div(
    style = "background-color: #FFFFFF;",
    class = "p-4 rounded rounded-3",
    
    ...
  )
  
}












bslib::page_navbar(
  
  title = "Shiny Widgets",
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "materia",
    bg = "#FAFAFA",
    fg = "#000000",
  ),
  fillable = FALSE,
  position = "fixed-top",
  
  bslib::nav_panel(
    title = "Exploratory Data Analysis",
    value = "EDA",
    
    br(), br(), br(),
    # --------------------------------------------------------------------------
    h3("Unique count of single-character variable", class = "fw-light"),
    
    col_layout(
      container(
        shinyWidgets::pickerInput(
          "char_count_inp",
          label = "Select",
          choices = char_cols
        ),
        
        shinyWidgets::numericInputIcon(
          "char_count_top_values",
          label = "Top",
          value = 10,
          min = 5, max = 20, step = 1,
          # width = "200px",
          help_text = "Number must be from 5 to 20"
        ),
        
        uiOutput("alert")
      ),
      
      container(
        plotOutput("char_count") |> 
          shinyWidgets::addSpinner("fading-circle", "#9400D3")
      )
    ),
    
    br(),
    
    # --------------------------------------------------------------------------
    h3("Distribution of a single numeric variable", class = "fw-light"),
    
    col_layout(
      container(
        shinyWidgets::pickerInput(
          "single_num_variable",
          label = "Select",
          choices = num_cols
        ),
        
        br(),
        
        shinyWidgets::dropMenu(
          shinyWidgets::actionBttn(
            inputId = "show_inputs",
            label = "Additional Inputs",
            style = "stretch",
            color = "royal",
            size = "md"
          ),
          
          shinyWidgets::prettyCheckboxGroup(
            "num_plot_type",
            label = "Plot Type",
            choices = c("Histogram", "Box-Plot"),
            selected = "Histogram",
            status = "primary",
            shape = "curve",
            animation = "pulse"
          ),
          
          br(),
          
          numericInput(
            "bins",
            label = "Number of histogram bins",
            value = 30, 
            min = 10, max = 50, step = 5,
            width = "250px"
          ),
          
          br(),
          
          shinyWidgets::chooseSliderSkin(skin = "Round"),
          
          uiOutput("zoom_slider_ui"),
        ),
        
      ),
      
      container(
        uiOutput("single_num_plot_ui")
      )
    ),
    
    br(),
    
    # --------------------------------------------------------------------------
    h3("Relationship between two numeric variables", class = "fw-light"),
    
    col_layout(
      container(
        shinyWidgets::virtualSelectInput(
          "two_num_variables",
          label = "Select",
          choices = num_cols,
          multiple = TRUE,
          search = TRUE
        ),
        
        br(),
        
        shinyWidgets::materialSwitch(
          "inclued_character_variable",
          label = "Add Character Variable",
          value = FALSE,
          status = "success",
          right = TRUE
        ),
        
        br(),
        
        uiOutput("include_char_input_ui")
      ),
      
      container(
        plotOutput("two_num_plot") |>
          shinyWidgets::addSpinner("fading-circle", "#9400D3")
      )
    ),
    
    # --------------------------------------------------------------------------
    h3("Fuel economy data stats", class = "fw-light"),
    
    bslib::layout_columns(
      col_widths = c(-2, 8, -2),
      fillable = FALSE,
      
      container(
        shinyWidgets::radioGroupButtons(
          inputId = "show_stats",
          label = "",
          choices = c(
            `<i class='fa fa-industry'></i>` = "manufacturer",
            `<i class='fa fa-building'></i>` = "city",
            `<i class='fa fa-road'></i>` = "highway"
          ),
          selected = "Okay",
          status = "default",
          size = "lg",
          justified = TRUE
        ),
        
        br(), br(),
        
        div(
          uiOutput("stats_cards"),
          
          style = "width: 60%; margin: 0.2rem auto;"
        )
      )
    ),
    
    br(), br()
    
  ),
  
  bslib::nav_spacer(),
  
  bslib::nav_item(
    
    shinyWidgets::dropMenu(
      
      actionButton("filter_year", "", icon = icon("filter")),
      
      shinyWidgets::prettyRadioButtons(
        "selected_year",
        label = "Year",
        choices = c("1999", "2008", "All"),
        selected = "All",
        shape = "square",
        bigger = TRUE
      ),
      
      placement = "left",
      theme = "material",
      maxWidth = "100px"
    )
    
  )
  
)
