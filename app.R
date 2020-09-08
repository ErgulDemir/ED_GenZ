##ShinyApp_Generating random values and show graph (08.09.2020)

library(shiny)

ui <- fluidPage(
  headerPanel("Generate Normal Distribution"),
  sidebarPanel(
    h3(p("Please select a sample size, mean and standard deviation.")),
    br(),
    numericInput("n", "n", 30),
    br(),
    numericInput("m", "Mean", 0),
    br(),
    numericInput("s", "Standard Deviation", 1),
    br(),
    actionButton("go", h5("Show Graph"))),
    br(),
  mainPanel(plotOutput("plot"))
)

server <- function(input, output){
  output$plot <- renderPlot({
    input$go
    rand <- isolate(rnorm(input$n, input$m, input$s))
    isolate(hist(rand, col = "grey", freq = FALSE, xlab = "Random Values", main = ""))
    isolate(curve(dnorm(x, input$m, input$s), lwd = 2, lty = 2, col = "red", add = TRUE))
    isolate(legend("topright", bty = "n", paste(
      "n =", length(rand), "\n",
      "mean =", round(mean(rand), 2), "\n", 
      "sd =", round(sd(rand), 2), "\n",
      "skew = ", round(sum((rand - mean(rand))^3)/(length(rand)*sd(rand)^4), 2), "\n",
      "kurt = ", round(sum((rand - mean(rand))^4)/(length(rand)*sd(rand)^4)- 3, 2))
     ))
  })
}

shinyApp(ui, server)