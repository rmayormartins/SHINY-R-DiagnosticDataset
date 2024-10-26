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
            actionButton("train", "Treinar Modelo")
        ),
        mainPanel(
            plotOutput("scatterPlot"),
            plotOutput("learningCurve"),
            verbatimTextOutput("modelSummary"),
            tableOutput("confusionMatrix")
        )
    )
)

#server
library(shiny)
library(caret)
library(nnet)
library(ggplot2)

server <- function(input, output, session) {
    data <- reactive({
        req(input$file)
        read.csv(input$file$datapath)
    })
    
    output$varSelectUI <- renderUI({
        req(data())
        tagList(
            selectInput("predictors", "Variáveis Preditivas:", names(data()), multiple = TRUE),
            selectInput("response", "Variável Alvo:", names(data()))
        )
    })
    
    trainModel <- eventReactive(input$train, {
        req(input$predictors, input$response)
        formula <- as.formula(paste(input$response, "~", paste(input$predictors, collapse = "+")))
        
        # Garantir múltiplos valores para tuneGrid
        tuneGrid <- expand.grid(size = c(input$size, input$size + 1), decay = c(input$decay, input$decay + 0.1))
        
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
