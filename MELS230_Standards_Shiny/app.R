library(shiny)
library(ggplot2)
library(rhandsontable)

# Define custom theme for all plots
custom_theme <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = "gray80"),
    panel.grid.major = element_line(color = "gray90", size = 0.2),
    panel.grid.minor = element_line(color = "gray95", size = 0.1),
    axis.line = element_line(color = "black", size = 0.5),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# Define common plotting parameters
plot_params <- list(
  point_size = 3,
  point_color = "#0D98BA",
  regression_color = "#0D98BA",
  regression_size = 1.5,
  reference_color = "#BA2F0D",
  reference_size = 1.5,
  highlight_color = "#BA2F0D",
  highlight_size = 4
)

# Function to create standardized plot
create_determination_plot <- function(plot_data, filtered_data, regression_model, 
                                      absorbance_input, title) {
  p <- ggplot(plot_data, aes(x = Amount, y = Absorbance)) +
    geom_point(color = plot_params$point_color, 
               size = plot_params$point_size) +
    labs(title = title,
         x = "Amount", 
         y = "Absorbance") +
    custom_theme
  
  if(!is.null(regression_model)) {
    p <- p + geom_smooth(data = filtered_data,
                         method = "lm",
                         formula = y ~ 0 + x,
                         se = FALSE,
                         color = plot_params$regression_color,
                         size = plot_params$regression_size,
                         linetype = "dashed")
    
    if(!is.na(absorbance_input)) {
      amount <- absorbance_input / coef(regression_model)[1]
      p <- p + 
        geom_point(data = data.frame(Amount = amount,
                                     Absorbance = absorbance_input),
                   color = plot_params$highlight_color,
                   size = plot_params$highlight_size,
                   shape = 17) +
        annotate("segment", 
                 x = 0, xend = amount,
                 y = absorbance_input, yend = absorbance_input,
                 linetype = "dotted", 
                 color = plot_params$reference_color,
                 size = plot_params$reference_size) +
        annotate("segment",
                 x = amount, xend = amount,
                 y = 0, yend = absorbance_input,
                 linetype = "dotted", 
                 color = plot_params$reference_color,
                 size = plot_params$reference_size)
    }
  }
  return(p)
}

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
                                  min = 0, max = 80, value = c(0, 80), step = 20)
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
                                  min = 0, max = 40, value = c(0, 40), step = 8)
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
                                  min = 0, max = .1, value = c(0, .1), step = .02)
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
    urea = reactiveVal(data.frame(
      Sample = c("Tube 1", "Tube 2", "Tube 3", "Tube 4", "Tube 5"),
      Amount = c(0, rep(NA, 4)),
      Absorbance = c(0, rep(NA, 4)),
      stringsAsFactors = FALSE
    )),
    uric_acid = reactiveVal(data.frame(
      Sample = c("Tube 1", "Tube 2", "Tube 3", "Tube 4", "Tube 5", "Tube 6"),
      Amount = c(0, rep(NA, 5)),
      Absorbance = c(0, rep(NA, 5)),
      stringsAsFactors = FALSE
    )),
    creatinine = reactiveVal(data.frame(
      Sample = c("Tube 1", "Tube 2", "Tube 3", "Tube 4", "Tube 5", "Tube 6"),
      Amount = c(0, rep(NA, 5)),
      Absorbance = c(0, rep(NA, 5)),
      stringsAsFactors = FALSE
    ))
  )
  
  # Observe table changes and update reactive values
  observeEvent(input$table1, {
    data_list$urea(hot_to_r(input$table1))
  })
  
  observeEvent(input$table2, {
    data_list$uric_acid(hot_to_r(input$table2))
  })
  
  observeEvent(input$table3, {
    data_list$creatinine(hot_to_r(input$table3))
  })
  
  # Render tables
  output$table1 <- renderRHandsontable({
    rhandsontable(data_list$urea(), rowHeaders = FALSE) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col("Amount", type = "numeric", format = "0.00") %>%
      hot_col("Absorbance", type = "numeric", format = "0.000")
  })
  
  output$table2 <- renderRHandsontable({
    rhandsontable(data_list$uric_acid(), rowHeaders = FALSE) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col("Amount", type = "numeric", format = "0.00") %>%
      hot_col("Absorbance", type = "numeric", format = "0.000")
  })
  
  output$table3 <- renderRHandsontable({
    rhandsontable(data_list$creatinine(), rowHeaders = FALSE) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col("Amount", type = "numeric", format = "0.00") %>%
      hot_col("Absorbance", type = "numeric", format = "0.000")
  })
  
  # Create reactive expressions for filtered data based on slider ranges
  filtered_data <- list(
    urea = reactive({
      req(input$range1)
      df <- data_list$urea()
      df <- df[!is.na(df$Amount) & !is.na(df$Absorbance),]
      df[df$Amount >= input$range1[1] & df$Amount <= input$range1[2],]
    }),
    uric_acid = reactive({
      req(input$range2)
      df <- data_list$uric_acid()
      df <- df[!is.na(df$Amount) & !is.na(df$Absorbance),]
      df[df$Amount >= input$range2[1] & df$Amount <= input$range2[2],]
    }),
    creatinine = reactive({
      req(input$range3)
      df <- data_list$creatinine()
      df <- df[!is.na(df$Amount) & !is.na(df$Absorbance),]
      df[df$Amount >= input$range3[1] & df$Amount <= input$range3[2],]
    })
  )
  
  # Create reactive expressions for regression models
  regression_models <- list(
    urea = reactive({
      req(filtered_data$urea())
      if(nrow(filtered_data$urea()) >= 2) {
        lm(Absorbance ~ 0 + Amount, data = filtered_data$urea())
      }
    }),
    uric_acid = reactive({
      req(filtered_data$uric_acid())
      if(nrow(filtered_data$uric_acid()) >= 2) {
        lm(Absorbance ~ 0 + Amount, data = filtered_data$uric_acid())
      }
    }),
    creatinine = reactive({
      req(filtered_data$creatinine())
      if(nrow(filtered_data$creatinine()) >= 2) {
        lm(Absorbance ~ 0 + Amount, data = filtered_data$creatinine())
      }
    })
  )
  
  # Calculate amounts based on absorbance inputs
  output$calculated_amount1 <- renderText({
    req(input$absorbance_input1, regression_models$urea())
    tryCatch({
      slope <- coef(regression_models$urea())[1]
      amount <- input$absorbance_input1 / slope
      sprintf("Calculated Amount: %.3f", amount)
    }, error = function(e) {
      "Unable to calculate amount. Check regression and input."
    })
  })
  
  output$calculated_amount2 <- renderText({
    req(input$absorbance_input2, regression_models$uric_acid())
    tryCatch({
      slope <- coef(regression_models$uric_acid())[1]
      amount <- input$absorbance_input2 / slope
      sprintf("Calculated Amount: %.3f", amount)
    }, error = function(e) {
      "Unable to calculate amount. Check regression and input."
    })
  })
  
  output$calculated_amount3 <- renderText({
    req(input$absorbance_input3, regression_models$creatinine())
    tryCatch({
      slope <- coef(regression_models$creatinine())[1]
      amount <- input$absorbance_input3 / slope
      sprintf("Calculated Amount: %.3f", amount)
    }, error = function(e) {
      "Unable to calculate amount. Check regression and input."
    })
  })
  

  
  # Update plot outputs to use the new function
  output$dotPlot1 <- renderPlot({
    plot_data <- data_list$urea()[!is.na(data_list$urea()$Amount) & 
                                    !is.na(data_list$urea()$Absorbance),]
    
    create_determination_plot(
      plot_data = plot_data,
      filtered_data = filtered_data$urea(),
      regression_model = regression_models$urea(),
      absorbance_input = input$absorbance_input1,
      title = "Urea Determination"
    )
  })
  
  output$dotPlot2 <- renderPlot({
    plot_data <- data_list$uric_acid()[!is.na(data_list$uric_acid()$Amount) & 
                                         !is.na(data_list$uric_acid()$Absorbance),]
    
    create_determination_plot(
      plot_data = plot_data,
      filtered_data = filtered_data$uric_acid(),
      regression_model = regression_models$uric_acid(),
      absorbance_input = input$absorbance_input2,
      title = "Uric Acid Determination"
    )
  })
  
  output$dotPlot3 <- renderPlot({
    plot_data <- data_list$creatinine()[!is.na(data_list$creatinine()$Amount) & 
                                          !is.na(data_list$creatinine()$Absorbance),]
    
    create_determination_plot(
      plot_data = plot_data,
      filtered_data = filtered_data$creatinine(),
      regression_model = regression_models$creatinine(),
      absorbance_input = input$absorbance_input3,
      title = "Creatinine Determination"
    )
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)