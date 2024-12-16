library(shiny)
library(ggplot2)
library(rhandsontable)

# Define UI for application 
ui <- fluidPage(
  # Application title
  titlePanel("MELS230"),
  tabsetPanel(
    tabPanel("Urea Determination", tags$br(),
             rHandsontableOutput("table1"),
             fluidRow(
               column(6,
                      sliderInput("range1", "Select Range for Regression:",
                                  min = 0, max = 100, value = c(0, 100), step = 0.1)
               ),
               column(6,
                      numericInput("absorbance_input1", "Enter Absorbance:", 
                                   value = NA, min = 0, step = 0.001)
               )
             ),
             fluidRow(
               column(12,
                      verbatimTextOutput("calculated_amount1")
               )
             ),
             plotOutput("dotPlot1")),
    
    tabPanel("Uric Acid Determination", tags$br(),
             rHandsontableOutput("table2"),
             fluidRow(
               column(6,
                      sliderInput("range2", "Select Range for Regression:",
                                  min = 0, max = 100, value = c(0, 100), step = 0.1)
               ),
               column(6,
                      numericInput("absorbance_input2", "Enter Absorbance:", 
                                   value = NA, min = 0, step = 0.001)
               )
             ),
             fluidRow(
               column(12,
                      verbatimTextOutput("calculated_amount2")
               )
             ),
             plotOutput("dotPlot2")),
    
    tabPanel("Creatinine Determination", tags$br(),
             rHandsontableOutput("table3"),
             fluidRow(
               column(6,
                      sliderInput("range3", "Select Range for Regression:",
                                  min = 0, max = 100, value = c(0, 100), step = 0.1)
               ),
               column(6,
                      numericInput("absorbance_input3", "Enter Absorbance:", 
                                   value = NA, min = 0, step = 0.001)
               )
             ),
             fluidRow(
               column(12,
                      verbatimTextOutput("calculated_amount3")
               )
             ),
             plotOutput("dotPlot3"))
  )
)

# Define server logic 
server <- function(input, output, session) {
  # Initialize data for each tab
  data_list <- list(
    urea = reactiveValues(df = data.frame(
      Sample = c("Water", "Tube 1", "Tube 2", "Tube 3", "Tube 4", "Tube 5"),
      Amount = c(0, rep(NA, 5)),
      Absorbance = c(0, rep(NA, 5)),
      stringsAsFactors = FALSE
    )),
    uric_acid = reactiveValues(df = data.frame(
      Sample = c("Water", "Tube 1", "Tube 2", "Tube 3", "Tube 4", "Tube 5"),
      Amount = c(0, rep(NA, 5)),
      Absorbance = c(0, rep(NA, 5)),
      stringsAsFactors = FALSE
    )),
    creatinine = reactiveValues(df = data.frame(
      Sample = c("Water", "Tube 1", "Tube 2", "Tube 3", "Tube 4", "Tube 5"),
      Amount = c(0, rep(NA, 5)),
      Absorbance = c(0, rep(NA, 5)),
      stringsAsFactors = FALSE
    ))
  )
  
  # Function to create reactive table rendering
  create_table_render <- function(data_reactive, output_name) {
    output[[output_name]] <- renderRHandsontable({
      rhandsontable(data_reactive$df, rowHeaders = FALSE) %>%
        hot_col(1, readOnly = TRUE) %>%
        hot_col("Amount", type = "numeric", format = "0.000") %>%
        hot_col("Absorbance", type = "numeric", format = "0.000")
    })
  }
  
  # Render tables
  create_table_render(data_list$urea, "table1")
  create_table_render(data_list$uric_acid, "table2")
  create_table_render(data_list$creatinine, "table3")
  
  # Function to observe table changes
  create_table_observe <- function(data_reactive, input_name) {
    observe({
      req(input[[input_name]])
      data_reactive$df <- hot_to_r(input[[input_name]])
    })
  }
  
  # Observe table changes
  create_table_observe(data_list$urea, "table1")
  create_table_observe(data_list$uric_acid, "table2")
  create_table_observe(data_list$creatinine, "table3")
  
  # Function to create plot data
  create_plot_data <- function(data_reactive) {
    reactive({
      df <- data_reactive$df
      # Remove any rows with NA values
      df[!is.na(df$Amount) & !is.na(df$Absorbance),]
    })
  }
  
  # Create plot data for each tab
  plot_data_list <- list(
    urea = create_plot_data(data_list$urea),
    uric_acid = create_plot_data(data_list$uric_acid),
    creatinine = create_plot_data(data_list$creatinine)
  )
  
  # Function to create regression data
  create_regression_data <- function(plot_data, range_input) {
    reactive({
      df <- plot_data()
      # Filter data based on slider range
      subset_data <- df[df$Amount >= range_input[1] & 
                          df$Amount <= range_input[2],]
      return(subset_data)
    })
  }
  
  # Create regression data for each tab
  regression_data_list <- list(
    urea = create_regression_data(plot_data_list$urea, input$range1),
    uric_acid = create_regression_data(plot_data_list$uric_acid, input$range2),
    creatinine = create_regression_data(plot_data_list$creatinine, input$range3)
  )
  
  # Function to create regression model
  create_regression_model <- function(regression_data) {
    reactive({
      data <- regression_data()
      # Ensure at least 2 valid data points
      if(nrow(data) < 2) {
        return(NULL)
      }
      # Fit linear regression through origin
      lm(Absorbance ~ 0 + Amount, data = data)
    })
  }
  
  # Create regression models for each tab
  regression_model_list <- list(
    urea = create_regression_model(regression_data_list$urea),
    uric_acid = create_regression_model(regression_data_list$uric_acid),
    creatinine = create_regression_model(regression_data_list$creatinine)
  )
  
  # Function to calculate amount from absorbance
  create_calculated_amount <- function(regression_model, absorbance_input) {
    reactive({
      # Require both regression model and absorbance input
      req(regression_model(), absorbance_input)
      
      # Get the slope (coefficient) from the regression model
      slope <- coef(regression_model())
      
      # Calculate amount by dividing absorbance by slope
      amount <- absorbance_input / slope
      
      return(amount)
    })
  }
  
  # Calculate amounts for each tab
  calculated_amount_list <- list(
    urea = create_calculated_amount(regression_model_list$urea, input$absorbance_input1),
    uric_acid = create_calculated_amount(regression_model_list$uric_acid, input$absorbance_input2),
    creatinine = create_calculated_amount(regression_model_list$creatinine, input$absorbance_input3)
  )
  
  # Function to output calculated amount
  create_amount_output <- function(calculated_amount, output_name) {
    output[[output_name]] <- renderText({
      # Use tryCatch to handle potential errors
      tryCatch({
        amount <- calculated_amount()
        sprintf("Calculated Amount: %.3f", amount)
      }, error = function(e) {
        "Unable to calculate amount. Check regression and input."
      })
    })
  }
  
  # Create amount outputs
  create_amount_output(calculated_amount_list$urea, "calculated_amount1")
  create_amount_output(calculated_amount_list$uric_acid, "calculated_amount2")
  create_amount_output(calculated_amount_list$creatinine, "calculated_amount3")
  
  # Function to create plot
  create_plot <- function(all_data, regression_subset, regression_model, 
                          absorbance_input, calculated_amount, output_name) {
    output[[output_name]] <- renderPlot({
      # Get full plot data
      if(nrow(all_data()) == 0) {
        # Return an empty plot if no data
        ggplot() + 
          labs(title = "No data available",
               x = "Amount", 
               y = "Absorbance")
      } else {
        # Base plot with all data points
        p <- ggplot(all_data(), aes(x = Amount, y = Absorbance)) +
          geom_point(color = "blue", size = 3) +
          labs(title = "Dot Plot of Determination",
               x = "Amount", 
               y = "Absorbance")
        
        # Add regression line for selected range
        if(nrow(regression_subset()) >= 2) {
          model <- regression_model()
          if(!is.null(model)) {
            p <- p + 
              geom_smooth(data = regression_subset(), 
                          method = "lm", 
                          formula = y ~ 0 + x, 
                          se = FALSE, 
                          color = "red", 
                          linetype = "dashed")
            
            # If absorbance input is provided, add a point
            if(!is.na(absorbance_input)) {
              p <- p + 
                geom_point(data = data.frame(
                  Amount = calculated_amount(), 
                  Absorbance = absorbance_input
                ), 
                color = "green", 
                size = 5, 
                shape = 17)
            }
          }
        }
        
        p
      }
    })
  }
  
  # Create plots for each tab
  create_plot(plot_data_list$urea, regression_data_list$urea, 
              regression_model_list$urea, input$absorbance_input1, 
              calculated_amount_list$urea, "dotPlot1")
  
  create_plot(plot_data_list$uric_acid, regression_data_list$uric_acid, 
              regression_model_list$uric_acid, input$absorbance_input2, 
              calculated_amount_list$uric_acid, "dotPlot2")
  
  create_plot(plot_data_list$creatinine, regression_data_list$creatinine, 
              regression_model_list$creatinine, input$absorbance_input3, 
              calculated_amount_list$creatinine, "dotPlot3")
}

# Run the application 
shinyApp(ui = ui, server = server)