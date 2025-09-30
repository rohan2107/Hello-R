# install.packages(c("shiny", "ggplot2", "dplyr", "scales"))
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

ui <- fluidPage(
  titlePanel("Example: Bayes' Theorem and Diagnostic Testing"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("prev", "Prevalence (P(disease)):", 
                  min = 0.001, max = 0.5, value = 0.05, step = 0.001),
      sliderInput("sens", "Sensitivity (P(test + | disease)):", 
                  min = 0.5, max = 1, value = 0.9, step = 0.01),
      sliderInput("spec", "Specificity (P(test - | no disease)):", 
                  min = 0.5, max = 1, value = 0.9, step = 0.01),
      numericInput("N", "Population size (for counts table):", 1000, min = 100)
    ),
    
    mainPanel(
      h4("Expected counts"),
      tableOutput("contingencyTable"),
      h4("Bayes' theorem"),
      withMathJax("$$
        P(\\text{disease} \\mid \\text{test} = +) =
        \\frac{P(\\text{test} = + \\mid \\text{disease}) \\, P(\\text{disease})}
             {P(\\text{test} = +)}
      $$"),
      br(),
      h4("Posterior probability: P(disease | test = +)"),
      plotOutput("posteriorPlot", height = "300px")
    )
  )
)

server <- function(input, output) {
  
  results <- reactive({
    N <- input$N
    prev <- input$prev
    sens <- input$sens
    spec <- input$spec
    
    diseased <- round(N * prev)
    healthy  <- N - diseased
    
    TP <- round(diseased * sens)
    FN <- diseased - TP
    TN <- round(healthy * spec)
    FP <- healthy - TN
    
    # Posterior probability (PPV)
    post <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
    
    list(
      table = data.frame(
        Test.positive = as.integer(c(TP, FP)),
        Test.negative = as.integer(c(FN, TN)),
        row.names = c("Disease", "No Disease")
      ),
      posterior = post
    )
  })
  
  # Table output
  output$contingencyTable <- renderTable({
    results()$table
  }, rownames = TRUE)
  
  # Plot output
  # Plot output with probability labels
  output$posteriorPlot <- renderPlot({
    post <- results()$posterior
    df <- data.frame(
      event = c("P(disease | test +)", "P(no disease | test +)"),
      prob = c(post, 1 - post)
    )
    
    ggplot(df, aes(x = event, y = prob, fill = event)) +
      geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
      geom_text(aes(label = percent(prob, accuracy = 0.1)), 
                vjust = -0.5, size = 5) +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,1)) +
      labs(x = "", y = "Probability", 
           title = "Posterior probability given a positive test") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)
