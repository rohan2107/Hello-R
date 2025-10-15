library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

ui <- fluidPage(
  titlePanel("Confidence Intervals for Normal Parameters"),
  sidebarLayout(
    sidebarPanel(
      numericInput("mu", "True mean:", value = 0, step = 0.1),
      numericInput("sigma", "True sd:", value = 1, min = 0.0001, step = 0.1),
      selectInput("param", "Parameter to estimate:", 
                  choices = c("Mean" = "mean", "Variance" = "variance")),
      hr(),
      numericInput("n", "Sample size (n):", value = 30, min = 2, step = 1),
      sliderInput("conf", "Confidence level:", min = 0.50, max = 0.99, value = 0.95, step = 0.01),
      numericInput("nsim", "Number of intervals to generate:", value = 100, min = 1, max = 5000, step = 1),
      checkboxInput("seed_on", "Use fixed seed", value = FALSE),
      conditionalPanel(condition = "input.seed_on == true",
                       numericInput("seed", "PRNG seed:", value = 2025, step = 1)
      ),
      actionButton("go", "Generate intervals", class = "btn-primary"),
      width = 3
    ),
    mainPanel(
      fluidRow(
        column(8, plotOutput("ciPlot", height = "600px")),
        column(4, 
               h4("Summary"),
               verbatimTextOutput("summaryText"),
               hr(),
               h5("Histogram of point estimates"),
               plotOutput("histPlot", height = "220px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive simulation triggered by button
  simulate_intervals <- eventReactive(input$go, {
    if (input$seed_on) set.seed(input$seed)
    
    mu <- input$mu
    sigma <- input$sigma
    n <- as.integer(input$n)
    conf <- input$conf
    nsim <- as.integer(input$nsim)
    param <- input$param
    alpha <- 1 - conf
    
    # Simulate nsim samples
    sims <- replicate(nsim, rnorm(n, mean = mu, sd = sigma), simplify = FALSE)
    
    if (param == "mean") {
      res <- lapply(seq_len(nsim), function(i) {
        x <- sims[[i]]
        xbar <- mean(x)
        s <- sd(x)
        se <- s / sqrt(n)
        tcrit <- qt(1 - alpha/2, df = n - 1)
        lower <- xbar - tcrit * se
        upper <- xbar + tcrit * se
        captured <- (lower <= mu) & (mu <= upper)
        data.frame(index = i, estimate = xbar, lower, upper, captured, width = upper - lower)
      }) %>% bind_rows()
      true_value <- mu
    } else {
      res <- lapply(seq_len(nsim), function(i) {
        x <- sims[[i]]
        s2 <- var(x)
        chi_lower <- qchisq(1 - alpha/2, df = n - 1)
        chi_upper <- qchisq(alpha/2, df = n - 1)
        lower <- (n - 1) * s2 / chi_lower
        upper <- (n - 1) * s2 / chi_upper
        captured <- (lower <= sigma^2) & (sigma^2 <= upper)
        data.frame(index = i, estimate = s2, lower, upper, captured, width = upper - lower)
      }) %>% bind_rows()
      true_value <- sigma^2
    }
    
    list(res = res, true_value = true_value, param = param, conf = conf)
  })
  
  # Main CI plot
  output$ciPlot <- renderPlot({
    sim <- simulate_intervals()
    if (is.null(sim)) return()
    
    res <- sim$res
    true_value <- sim$true_value
    param <- sim$param
    conf <- sim$conf
    
    res <- res %>% mutate(capturedf = ifelse(captured, "Captured", "Missed"))
    ggplot(res, aes(y = index, x = estimate)) +
      geom_errorbarh(aes(xmin = lower, xmax = upper, color = capturedf), height = 0.4, size = 1) +
      geom_point(size = 1.5, color = "black") +
      geom_vline(xintercept = true_value, linetype = "dashed", color = "blue", size = 1) +
      scale_color_manual(values = c("Captured" = "forestgreen", "Missed" = "firebrick")) +
      labs(
        x = ifelse(param == "mean", expression(hat(mu)), expression(hat(sigma)^2)),
        y = "Simulation index",
        title = paste0("Simulated ", nrow(res), " Confidence Intervals"),
        subtitle = paste0(round(conf * 100), "% confidence level (dashed = true value)"),
        color = ""
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom") +
      guides(colour = "none")
  })
  
  # Summary stats
  output$summaryText <- renderPrint({
    sim <- simulate_intervals()
    if (is.null(sim)) return()
    
    res <- sim$res
    true_value <- sim$true_value
    conf <- sim$conf
    nsim <- nrow(res)
    emp_coverage <- mean(res$captured)
    avg_width <- mean(res$width)
    
    cat("Parameter:", ifelse(sim$param == "mean", "Mean (μ)", "Variance (σ²)"), "\n")
    cat("True value:", signif(true_value, 6), "\n\n")
    cat("Confidence level:", round(conf * 100, 1), "%\n")
    cat("Simulations:", nsim, "\n")
    cat("Empirical coverage:", scales::percent(emp_coverage), "\n")
    cat("Average interval width:", signif(avg_width, 6), "\n")
  })
  
  # Histogram of estimates
  output$histPlot <- renderPlot({
    sim <- simulate_intervals()
    if (is.null(sim)) return()
    
    res <- sim$res
    true_value <- sim$true_value
    param <- sim$param
    
    ggplot(res, aes(x = estimate)) +
      geom_histogram() +
      geom_vline(xintercept = true_value, linetype = "dashed", color = "blue", size = 1) +
      labs(
        x = ifelse(param == "mean", "Sample means", "Sample variances"),
        y = "Count",
        title = "Distribution of sample estimates"
      ) +
      theme_minimal(base_size = 12)
  })
}

shinyApp(ui, server)
