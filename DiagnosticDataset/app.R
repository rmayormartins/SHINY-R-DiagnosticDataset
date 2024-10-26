#ui
library(shiny)
library(DT)

ui <- fluidPage(
    titlePanel("Ferramenta de Diagnóstico de Dataset"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Carregar Conjunto de Dados (CSV)", accept = ".csv"),
            hr(),
            actionButton("checkTotal", "Verificar Total"),
            actionButton("checkClasses", "Verificar Classes"),
            actionButton("checkDuplicates", "Verificar Duplicatas"),
            actionButton("checkBalance", "Verificar Balanceamento"),
            actionButton("checkNA", "Verificar NA"),
            actionButton("checkOutliers", "Verificar Outliers"),
            hr(),
            uiOutput("varSingleSelectUI"),
            uiOutput("varSelectUI")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Dados: Visualizar",
                         verbatimTextOutput("dataPreview"),
                         verbatimTextOutput("dataStructure")),
                tabPanel("Dados: brutos",
                         DTOutput("dataTable")),
                tabPanel("Dados: resumo",
                         verbatimTextOutput("dataSummaryDetailed")),
                tabPanel("Diagnóstico do Conjunto de Dados",
                         verbatimTextOutput("dataSummary"),
                         verbatimTextOutput("variableStats")),
                tabPanel("Gráficos",
                         plotOutput("boxPlot"),
                         plotOutput("distributionPlot"),
                         plotOutput("densityPlot"),
                         plotOutput("scatterPlot"))
            )
        )
    )
)








#server
library(shiny)
library(ggplot2)
library(dplyr)
library(e1071)
library(DT)
library(skimr)

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
    
    output$dataPreview <- renderPrint({
        req(data())
        head(data(), 10)
    })
    
    output$dataStructure <- renderPrint({
        req(data())
        str(data())
    })
    
    output$dataTable <- renderDT({
        req(data())
        datatable(data(), options = list(pageLength = 10, autoWidth = TRUE, searchHighlight = TRUE))
    })
    
    output$dataSummaryDetailed <- renderPrint({
        req(data())
        skim(data())
    })
    
    output$dataSummary <- renderPrint({
        input$checkTotal
        input$checkClasses
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
            if (input$checkClasses > 0) {
                classes_info <- sapply(df, function(col) {
                    if (is.factor(col) || is.character(col)) {
                        paste(names(table(col)), table(col), sep = ": ", collapse = ", ")
                    } else {
                        NULL
                    }
                })
                summary$classes <- paste("Classes por variável categórica:\n", 
                                         paste(names(classes_info), classes_info, sep = ": ", collapse = "\n"))
            }
            if (input$checkDuplicates > 0) {
                summary$duplicatas <- paste("Total de Duplicatas:", sum(duplicated(df)))
            }
            if (input$checkBalance > 0) {
                balanceamento <- table(df[[input$response]])
                proporcoes <- prop.table(balanceamento)
                esta_equilibrado <- all(proporcoes > 0.2 & proporcoes < 0.8)  # Ajuste conforme necessário
                conclusao <- ifelse(esta_equilibrado, "O conjunto de dados está equilibrado.", "O conjunto de dados não está equilibrado.")
                summary$balance <- paste("Balanceamento das Classes:\n", 
                                         paste(names(balanceamento), balanceamento, sep = ": ", collapse = "\n"),
                                         "\nConclusão:", conclusao)
            }
            if (input$checkNA > 0) {
                summary$na <- paste("Total de Valores NA:", sum(is.na(df)))
            }
            if (input$checkOutliers > 0) {
                outliers <- lapply(df[input$predictors], function(x) {
                    if (is.numeric(x)) {
                        sum(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
                    } else {
                        NA
                    }
                })
                outlier_text <- paste("Outliers por variável:\n", 
                                      paste(names(outliers), sapply(outliers, function(x) ifelse(is.na(x), "N/A", x)), sep = ": ", collapse = "\n"))
                summary$outliers <- outlier_text
            }
            
            summary
        })
    })
    
    output$variableStats <- renderPrint({
        req(input$singleVariable)
        df <- data()
        var <- df[[input$singleVariable]]
        
        stats <- list(
            total_linhas = length(var),
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
    
    output$boxPlot <- renderPlot({
        req(input$singleVariable)
        ggplot(data(), aes_string(x = input$singleVariable)) +
            geom_boxplot() +
            coord_flip() +
            labs(x = input$singleVariable)
    })
    
    output$distributionPlot <- renderPlot({
        req(input$singleVariable)
        ggplot(data(), aes_string(x = input$singleVariable)) +
            geom_histogram(binwidth = 30, fill = "blue", color = "white") +
            labs(x = input$singleVariable, y = "Frequência")
    })
    
    output$densityPlot <- renderPlot({
        req(input$singleVariable)
        ggplot(data(), aes_string(x = input$singleVariable)) +
            geom_density(fill = "blue", alpha = 0.5) +
            labs(x = input$singleVariable, y = "Densidade")
    })
    
    output$scatterPlot <- renderPlot({
        req(input$predictors, input$response)
        ggplot(data(), aes_string(x = input$predictors[1], y = input$response)) +
            geom_point() +
            labs(x = input$predictors[1], y = input$response)
    })
}

shinyApp(ui = ui, server = server)
