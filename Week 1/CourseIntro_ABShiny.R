# install.packages(c("shiny", "ggplot2", "dplyr", "tidyr"))
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(xlsx)

# A function that loads and preprocesses the dataset
load_data <- function() {
  
  # Load data
  myfile <- "./Sep23_data.xlsx"
  df <- xlsx::read.xlsx(myfile, 1L)
  
  df <- df[c(1, 7:8)]
  names(df) <- c("Id", "Value.G1", "Value.G2")
  
  df <- df %>%
    pivot_longer(-Id, names_to = "Group", values_to = "Value") %>%
    mutate(Group = ifelse(grepl("G1", Group), "Group_A", "Group_B"),
           Value = as.numeric(gsub("\\$", "", Value))) %>%
    filter(!is.na(Value))
  
  return(df)
}

# Define UI
ui <- fluidPage(
  titlePanel("Exploratory Data Analysis & inference: Value estimates"),
  sidebarLayout(
    sidebarPanel(
      actionButton("reload", "Reload Data", icon = icon("refresh")),
      br(), br(),
      selectInput("plotType", "Choose plot type:",
                  choices = c("Boxplot", "Histogram", "Violin plot"),
                  selected = "Histogram"),
      checkboxInput("showSummary", "Show group summaries", FALSE),
      checkboxInput("showTest", "Show t-test results", FALSE)
    ),
    mainPanel(
      plotOutput("edaPlot"),
      tableOutput("summaryTable"),
      verbatimTextOutput("ttestOutput")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive dataset that reloads when the button is pressed
  dataset <- reactiveVal(load_data())
  
  observeEvent(input$reload, {
    dataset(load_data())
  })
  
  output$edaPlot <- renderPlot({
    df <- dataset()
    if (input$plotType == "Boxplot") {
      ggplot(df, aes(x = Group, y = Value, fill = Group)) +
        geom_boxplot(alpha = 0.6, show.legend = FALSE) +
        labs(y = "Estimated value ($)", x = "Group") +
        theme_light()
    } else if (input$plotType == "Histogram") {
      ggplot(df, aes(x = Value, fill = Group)) +
        geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
        facet_wrap(~Group, ncol = 1) +
        labs(x = "Estimated value ($)", y = "Count") +
        theme_light()
    } else {
      ggplot(df, aes(y = Value, x = Group, fill = Group)) +
        geom_violin(alpha = 0.15, draw_quantiles = c(.5)) +
        geom_jitter(size = 1.4, stroke = 0, height = 0, width = .25) +
        labs(x = "Estimated value ($)", y = "Density") +
        theme_light()
    }
  })
  
  output$summaryTable <- renderTable({
    if (input$showSummary) {
      dataset() %>% group_by(Group) %>%
        summarise(
          n = n(),
          mean = mean(Value),
          median = median(Value),
          sd = sd(Value),
          .groups = "drop"
        )
    }
  })
  
  # Two-sample t-test
  output$ttestOutput <- renderPrint({
    if (input$showTest) {
      t.test(Value ~ Group, data = dataset(), var.equal = FALSE)
    }
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
