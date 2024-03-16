library(shiny)
library(ggplot2)
library(nortest)  # Pakiet do testowania normalności

# UI
ui <- fluidPage(
  titlePanel("Dopasowanie modelu"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("sigma", "Wybierz wartość sigma:", choices = c(1, 1.5, 2), selected = 2),
      textInput("functionInput", "Wprowadź funkcję f(x):", "2 * x^2 + 3 * x + 5"),
      
      sliderInput("maxX", "Maksymalna wartość x:", min = 5, max = 15, value = 10, step = 2),
      actionButton("fitButton", "Dopasuj modele"),
    ),
    
    mainPanel(
      plotOutput("dataPlot"),
      plotOutput("modelPlot"),
      verbatimTextOutput("modelSummary2"),
      verbatimTextOutput("modelSummary"),
      verbatimTextOutput("normalityTest"),
      plotOutput("normalityPlot")
    )
  )
)

# Server
server <- function(input, output) {
  data <- reactive({
    tryCatch({
      set.seed(123)  # Zapewnienie powtarzalności danych
      x <- runif(300, min = 0, max = input$maxX)
      f <- function(x) eval(parse(text = input$functionInput))
      y <- f(x)
      error <- rnorm(300, mean = 0, sd = as.numeric(input$sigma))
      data.frame(x = x, y = y + error)
    }, error = function(e) {
      NULL
    })
  })
  
  fitted_model <- eventReactive(input$fitButton, {
    if (!is.null(data())) {
      model_formula <- as.formula(paste("y ~ x + I(x^2) + I(x^3) + sin(x) + cos(x)"))
      initial_model <- lm(model_formula, data = data())
      
      step(initial_model)
    }
  })
  
  output$dataPlot <- renderPlot({
    if (!is.null(data())) {
      ggplot(data(), aes(x = x, y = y)) + geom_point() + labs(title = "Wykres danych")
    } else {
      ggplot() + labs(title = "Wykres danych")
    }
  })
  
  output$modelPlot <- renderPlot({
    if (!is.null(data()) && !is.null(fitted_model())) {
      data_pred <- data()
      data_pred$y_predicted <- predict(fitted_model())
      ggplot(data_pred, aes(x = x)) +
        geom_point(aes(y = y), color = "blue") +
        geom_line(aes(y = y_predicted), color = "red") +
        labs(title = "Wykres danych i dopasowanych wartości")
    } else {
      ggplot() + labs(title = "Wykres danych i dopasowanych wartości")
    }
  })
  
  output$modelSummary <- renderPrint({
    if (!is.null(fitted_model())) {
      summary(fitted_model())
    }
  })
  
  output$normalityTest <- renderPrint({
    if (!is.null(fitted_model())) {
      residuals <- residuals(fitted_model())
      normal_test <- ad.test(residuals)
      if (normal_test$p.value < 0.05) {
        "Reszty NIE są zgodne z rozkładem normalnym"
      } else {
        "Reszty są zgodne z rozkładem normalnym"
      }
    }
  })
  
  
  output$modelSummary2 <- renderPrint({
    if (!is.null(fitted_model())) {
      coefficients(fitted_model())
    }
  })
  
  output$normalityPlot <- renderPlot({
    if (!is.null(fitted_model())) {
      qqnorm(fitted_model()$residuals)
      qqline(fitted_model()$residuals)
    }
  })
}

# Uruchomienie aplikacji Shiny
shinyApp(ui = ui, server = server)
