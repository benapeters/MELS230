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
  
  # Create plots
  output$dotPlot1 <- renderPlot({
    plot_data <- data_list$urea()[!is.na(data_list$urea()$Amount) & 
                                    !is.na(data_list$urea()$Absorbance),]
    
    p <- ggplot(plot_data, aes(x = Amount, y = Absorbance)) +
      geom_point(color = "blue", size = 3) +
      labs(title = "Urea Determination",
           x = "Amount", 
           y = "Absorbance")
    
    if(!is.null(regression_models$urea())) {
      p <- p + geom_smooth(data = filtered_data$urea(),
                           method = "lm",
                           formula = y ~ 0 + x,
                           se = FALSE,
                           color = "red",
                           linetype = "dashed")
      
      if(!is.na(input$absorbance_input1)) {
        amount <- input$absorbance_input1 / coef(regression_models$urea())[1]
        p <- p + geom_point(data = data.frame(Amount = amount,
                                              Absorbance = input$absorbance_input1),
                            color = "green",
                            size = 5,
                            shape = 17)
      }
    }
    p
  })
  
  output$dotPlot2 <- renderPlot({
    plot_data <- data_list$uric_acid()[!is.na(data_list$uric_acid()$Amount) & 
                                         !is.na(data_list$uric_acid()$Absorbance),]
    
    p <- ggplot(plot_data, aes(x = Amount, y = Absorbance)) +
      geom_point(color = "blue", size = 3) +
      labs(title = "Uric Acid Determination",
           x = "Amount", 
           y = "Absorbance")
    
    if(!is.null(regression_models$uric_acid())) {
      p <- p + geom_smooth(data = filtered_data$uric_acid(),
                           method = "lm",
                           formula = y ~ 0 + x,
                           se = FALSE,
                           color = "red",
                           linetype = "dashed")
      
      if(!is.na(input$absorbance_input2)) {
        amount <- input$absorbance_input2 / coef(regression_models$uric_acid())[1]
        p <- p + geom_point(data = data.frame(Amount = amount,
                                              Absorbance = input$absorbance_input2),
                            color = "green",
                            size = 5,
                            shape = 17)
      }
    }
    p
  })
  
  output$dotPlot3 <- renderPlot({
    plot_data <- data_list$creatinine()[!is.na(data_list$creatinine()$Amount) & 
                                          !is.na(data_list$creatinine()$Absorbance),]
    
    p <- ggplot(plot_data, aes(x = Amount, y = Absorbance)) +
      geom_point(color = "blue", size = 3) +
      labs(title = "Creatinine Determination",
           x = "Amount", 
           y = "Absorbance")
    
    if(!is.null(regression_models$creatinine())) {
      p <- p + geom_smooth(data = filtered_data$creatinine(),
                           method = "lm",
                           formula = y ~ 0 + x,
                           se = FALSE,
                           color = "red",
                           linetype = "dashed")
      
      if(!is.na(input$absorbance_input3)) {
        amount <- input$absorbance_input3 / coef(regression_models$creatinine())[1]
        p <- p + geom_point(data = data.frame(Amount = amount,
                                              Absorbance = input$absorbance_input3),
                            color = "green",
                            size = 5,
                            shape = 17)
      }
    }
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)