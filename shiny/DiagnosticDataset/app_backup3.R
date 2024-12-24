#ui
library(shiny)

ui <- fluidPage(
    titlePanel("Ferramenta de Diagnóstico de Redes Neurais"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Carregar Conjunto de Dados (CSV)", accept = ".csv"),
            uiOutput("varSelectUI"),
            numericInput("size", "Número de Neurônios na Camada Oculta:", value = 5, min = 1),
            numericInput("decay", "Taxa de Decaimento:", value = 0.1, min = 0),
            numericInput("maxit", "Número Máximo de Iterações:", value = 100, min = 1),
            actionButton("train", "Treinar Modelo"),
            hr(),
            selectInput("variable", "Selecione a Variável para Visualizar", choices = NULL),
            actionButton("plotVar", "Plotar Variável"),
            actionButton("plotAllVars", "Plotar Todas as Variáveis Preditivas")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Diagnóstico do Conjunto de Dados",
                         verbatimTextOutput("dataSummary")),
                tabPanel("Gráficos",
                         plotOutput("scatterPlot"),
                         plotOutput("variablePlot"))
            )
        )
    )
)



#server
library(shiny)
library(caret)
library(nnet)
library(ggplot2)
library(dplyr)

server <- function(input, output, session) {
    data <- reactive({
        req(input$file)
        read.csv(input$file$datapath)
    })
    
    observe({
        req(data())
        updateSelectInput(session, "variable", choices = names(data()))
    })
    
    output$varSelectUI <- renderUI({
        req(data())
        tagList(
            selectInput("predictors", "Variáveis Preditivas:", names(data()), multiple = TRUE),
            selectInput("response", "Variável Alvo:", names(data()))
        )
    })
    
    output$dataSummary <- renderPrint({
        req(data())
        df <- data()
        summary <- list()
        summary$n_preditivas <- length(input$predictors)
        summary$n_total <- nrow(df)
        summary$n_balance <- table(df[[input$response]])
        summary$n_duplicados <- sum(duplicated(df))
        summary$n_na <- sum(is.na(df))
        summary$outliers <- lapply(df[input$predictors], function(x) {
            sum(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
        })
        summary
    })
    
    trainModel <- eventReactive(input$train, {
        req(input$predictors, input$response)
        formula <- as.formula(paste(input$response, "~", paste(input$predictors, collapse = "+")))
        
        tuneGrid <- expand.grid(size = seq(input$size - 2, input$size + 2, by = 1), 
                                decay = seq(input$decay - 0.05, input$decay + 0.05, by = 0.05))
        
        model <- train(formula, data = data(), method = "nnet", 
                       tuneGrid = tuneGrid,
                       trControl = trainControl(method = "cv", number = 5),
                       linout = TRUE, trace = FALSE, maxit = input$maxit)
        return(model)
    })
    
    output$scatterPlot <- renderPlot({
        req(input$predictors, input$response)
        ggplot(data(), aes_string(x = input$predictors[1], y = input$response)) +
            geom_point() +
            labs(x = input$predictors[1], y = input$response)
    })
    
    output$variablePlot <- renderPlot({
        req(input$variable)
        ggplot(data(), aes_string(x = input$variable)) +
            geom_histogram(binwidth = 30, fill = "blue", color = "white") +
            labs(x = input$variable, y = "Frequência")
    })
    
    observeEvent(input$plotVar, {
        output$variablePlot <- renderPlot({
            req(input$variable)
            ggplot(data(), aes_string(x = input$variable)) +
                geom_histogram(binwidth = 30, fill = "blue", color = "white") +
                labs(x = input$variable, y = "Frequência")
        })
    })
    
    observeEvent(input$plotAllVars, {
        output$scatterPlot <- renderPlot({
            req(input$predictors, input$response)
            ggplot(data(), aes_string(x = input$predictors[1], y = input$response)) +
                geom_point() +
                labs(x = input$predictors[1], y = input$response)
        })
    })
    
    output$modelSummary <- renderPrint({
        req(trainModel())
        summary(trainModel())
    })
    
    output$learningCurve <- renderPlot({
        req(trainModel())
        plot(trainModel())
    })
    
    output$confusionMatrix <- renderTable({
        req(trainModel())
        predicted <- predict(trainModel(), newdata = data())
        actual <- data()[[input$response]]
        if (!is.factor(actual)) actual <- factor(actual)
        if (!is.factor(predicted)) predicted <- factor(predicted, levels = levels(actual))
        confusionMatrix(predicted, actual)$table
    })
}

shinyApp(ui = ui, server = server)
