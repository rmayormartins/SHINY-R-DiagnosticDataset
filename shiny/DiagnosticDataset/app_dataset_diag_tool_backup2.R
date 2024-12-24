#ui
library(shiny)
ui <- fluidPage(
    titlePanel("Ferramenta de Diagnóstico de Dataset"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Carregar Conjunto de Dados (CSV)", accept = ".csv"),
            uiOutput("varSingleSelectUI"),
            hr(),
            actionButton("checkTotal", "Verificar Total"),
            actionButton("checkDuplicates", "Verificar Duplicatas"),
            actionButton("checkBalance", "Verificar Balanceamento"),
            actionButton("checkNA", "Verificar NA"),
            actionButton("checkOutliers", "Verificar Outliers"),
            hr(),
            uiOutput("varSelectUI")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Diagnóstico do Conjunto de Dados",
                         verbatimTextOutput("dataSummary"),
                         verbatimTextOutput("variableStats")),
                tabPanel("Gráficos",
                         plotOutput("scatterPlot"),
                         plotOutput("boxPlot"),
                         plotOutput("distributionPlot"))
            )
        )
    )
)





#server
library(shiny)
library(ggplot2)
library(dplyr)
library(e1071)

server <- function(input, output, session) {
    data <- reactive({
        req(input$file)
        read.csv(input$file$datapath)
    })
    
    output$varSingleSelectUI <- renderUI({
        req(data())
        selectInput("singleVariable", "Selecione a Variável para Visualizar", choices = names(data()))
    })
    
    output$varSelectUI <- renderUI({
        req(data())
        tagList(
            selectInput("predictors", "Variáveis Preditivas:", names(data()), multiple = TRUE),
            selectInput("response", "Variável Alvo:", names(data()))
        )
    })
    
    output$dataSummary <- renderPrint({
        input$checkTotal
        input$checkDuplicates
        input$checkBalance
        input$checkNA
        input$checkOutliers
        
        isolate({
            df <- data()
            summary <- list()
            
            if (input$checkTotal > 0) {
                summary$total <- paste("Total de Linhas:", nrow(df), " | Total de Colunas:", ncol(df))
            }
            if (input$checkDuplicates > 0) {
                summary$duplicatas <- paste("Total de Duplicatas:", sum(duplicated(df)))
            }
            if (input$checkBalance > 0) {
                balanceamento <- table(df[[input$response]])
                summary$balance <- paste("Balanceamento das Classes:\n", 
                                         paste(names(balanceamento), balanceamento, sep = ": ", collapse = "\n"))
            }
            if (input$checkNA > 0) {
                summary$na <- paste("Total de Valores NA:", sum(is.na(df)))
            }
            if (input$checkOutliers > 0) {
                outliers <- lapply(df[input$predictors], function(x) {
                    sum(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
                })
                summary$outliers <- paste("Outliers por variável:\n", 
                                          paste(names(outliers), outliers, sep = ": ", collapse = "\n"))
            }
            
            summary
        })
    })
    
    output$variableStats <- renderPrint({
        req(input$singleVariable)
        df <- data()
        var <- df[[input$singleVariable]]
        
        stats <- list(
            mean = mean(var, na.rm = TRUE),
            median = median(var, na.rm = TRUE),
            q1 = quantile(var, 0.25, na.rm = TRUE),
            q3 = quantile(var, 0.75, na.rm = TRUE),
            iqr = IQR(var, na.rm = TRUE),
            kurtosis = kurtosis(var, na.rm = TRUE),
            skewness = skewness(var, na.rm = TRUE)
        )
        
        stats
    })
    
    output$scatterPlot <- renderPlot({
        req(input$predictors, input$response)
        ggplot(data(), aes_string(x = input$predictors[1], y = input$response)) +
            geom_point() +
            labs(x = input$predictors[1], y = input$response)
    })
    
    output$boxPlot <- renderPlot({
        req(input$singleVariable)
        ggplot(data(), aes_string(y = input$singleVariable)) +
            geom_boxplot() +
            labs(y = input$singleVariable)
    })
    
    output$distributionPlot <- renderPlot({
        req(input$singleVariable)
        ggplot(data(), aes_string(x = input$singleVariable)) +
            geom_histogram(binwidth = 30, fill = "blue", color = "white") +
            labs(x = input$singleVariable, y = "Frequência")
    })
}

shinyApp(ui = ui, server = server)
