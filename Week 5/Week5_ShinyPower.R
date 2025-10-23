# install.packages(c("shiny", "ggplot2", "dplyr", "scales"))
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

ui <- fluidPage(
  titlePanel("Exploring Statistical Power of a z-Test"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parameters"),
      numericInput("mu0", "Null mean (μ₀):", value = 0, step = 0.1),
      numericInput("mu1", "True mean (μ₁):", value = 0.5, step = 0.1),
      numericInput("sigma", "Population SD (σ):", value = 1, min = 0.01, step = 0.1),
      numericInput("n", "Sample size (n):", value = 30, min = 2, step = 1),
      sliderInput("alpha", "Significance level (α):", min = 0.001, max = 0.2, value = 0.05, step = 0.001),
      selectInput("tail", "Alternative hypothesis:", 
                  choices = c("Two-sided" = "two", "Upper-tailed (>)" = "upper", "Lower-tailed (<)" = "lower")),
      actionButton("go", "Update", class = "btn-primary"),
      width = 3
    ),
    
    mainPanel(
      fluidRow(
        column(7, plotOutput("powerPlot", height = "500px")),
        column(5,
               h4("Results"),
               verbatimTextOutput("summaryText"),
               hr(),
               plotOutput("effectPlot", height = "200px"))
      ),
      hr(),
      helpText("Notes:",
               "Power changes with n, α, σ, and effect size (μ₁ − μ₀).",
               "\nPower = Probability of rejecting H₀ when it is false.")
    )
  )
)

server <- function(input, output, session) {
  
  sim_params <- eventReactive(input$go, {
    list(
      mu0 = input$mu0,
      mu1 = input$mu1,
      sigma = input$sigma,
      n = input$n,
      alpha = input$alpha,
      tail = input$tail
    )
  })
  
  # Compute power analytically for z-test
  compute_power <- reactive({
    p <- sim_params()
    mu0 <- p$mu0
    mu1 <- p$mu1
    sigma <- p$sigma
    n <- p$n
    alpha <- p$alpha
    tail <- p$tail
    
    se <- sigma / sqrt(n)
    z_alpha <- qnorm(1 - alpha / ifelse(tail == "two", 2, 1))
    
    if (tail == "two") {
      crit_upper <- mu0 + z_alpha * se
      crit_lower <- mu0 - z_alpha * se
      power <- pnorm(crit_lower, mean = mu1, sd = se) + (1 - pnorm(crit_upper, mean = mu1, sd = se))
    } else if (tail == "upper") {
      crit <- mu0 + z_alpha * se
      power <- 1 - pnorm(crit, mean = mu1, sd = se)
    } else { # lower-tailed
      crit <- mu0 - z_alpha * se
      power <- pnorm(crit, mean = mu1, sd = se)
    }
    list(power = power, se = se, z_alpha = z_alpha)
  })
  
  output$summaryText <- renderPrint({
    p <- sim_params()
    c <- compute_power()
    
    cat("Hypotheses:\n")
    cat("  H₀: μ =", p$mu0, "\n")
    cat("  H₁: μ", 
        ifelse(p$tail == "upper", ">", ifelse(p$tail == "lower", "<", "≠")),
        p$mu0, "\n\n")
    
    cat("Sample size (n):", p$n, "\n")
    cat("σ:", p$sigma, "\n")
    cat("α:", p$alpha, "\n")
    cat("Effect size (Cohen's d):", round((p$mu1 - p$mu0) / p$sigma, 3), "\n")
    cat("Power:", percent(c$power), "\n\n")
    cat("Interpretation:\n  With n =", p$n, "and α =", p$alpha, 
        ", there is a", percent(c$power),
        "chance of correctly rejecting H₀ when μ₁ =", p$mu1, ".")
  })
  
  output$powerPlot <- renderPlot({
    p <- sim_params()
    c <- compute_power()
    mu0 <- p$mu0
    mu1 <- p$mu1
    se <- c$se
    alpha <- p$alpha
    tail <- p$tail
    z_alpha <- c$z_alpha
    
    x <- seq(mu0 - 4*se, mu1 + 4*se, length.out = 500)
    df <- data.frame(x = x,
                     H0 = dnorm(x, mu0, se),
                     H1 = dnorm(x, mu1, se))
    
    # Critical regions
    crits <- switch(tail,
                    two = c(mu0 - z_alpha * se, mu0 + z_alpha * se),
                    upper = mu0 + z_alpha * se,
                    lower = mu0 - z_alpha * se)
    
    ggplot(df, aes(x = x)) +
      geom_line(aes(y = H0, color = "H₀ true"), linewidth = 1.2) +
      geom_line(aes(y = H1, color = "H₁ true"), linewidth = 1.2, linetype = "dashed") +
      geom_vline(xintercept = crits, linetype = "dotted", color = "black", linewidth = 1) +
      scale_color_manual(values = c("H₀ true" = "blue", "H₁ true" = "red")) +
      labs(
        x = "Sample mean (x̄)",
        y = "Density",
        color = "",
        title = "Sampling Distributions Under H₀ and H₁",
        subtitle = paste0("Shaded red = Power (P(reject H₀ | H₁ true))")
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
   })
  

}

shinyApp(ui, server)
