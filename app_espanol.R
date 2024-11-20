# UI
library(shiny)
library(DT)

ui <- fluidPage(
    titlePanel("Herramienta de Diagnostico de Dataset"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Cargar Conjunto de Datos (CSV)", accept = ".csv"),
            hr(),
            actionButton("checkTotal", "Verificar Total"),
            actionButton("checkClasses", "Verificar Clases"),
            actionButton("checkDuplicates", "Verificar Duplicados"),
            actionButton("checkBalance", "Verificar Balanceo"),
            actionButton("checkNA", "Verificar NA"),
            actionButton("checkOutliers", "Verificar Outliers"),
            hr(),
            uiOutput("varSingleSelectUI"),
            uiOutput("varSelectUI")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Datos: Visualizacion",
                         verbatimTextOutput("dataPreview"),
                         verbatimTextOutput("dataStructure")),
                tabPanel("Datos: Brutos",
                         DTOutput("dataTable")),
                tabPanel("Datos: Resumen",
                         verbatimTextOutput("dataSummaryDetailed")),
                tabPanel("Diagnostico del Conjunto de Datos",
                         verbatimTextOutput("dataSummary"),
                         verbatimTextOutput("variableStats")),
                tabPanel("Graficos",
                         plotOutput("boxPlot"),
                         plotOutput("distributionPlot"),
                         plotOutput("densityPlot"),
                         plotOutput("scatterPlot"))
            )
        )
    )
)

# Server
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
        selectInput("singleVariable", "Seleccione la Variable para Visualizar", choices = names(data()))
    })
    
    output$varSelectUI <- renderUI({
        req(data())
        tagList(
            selectInput("predictors", "Variables Predictoras:", names(data()), multiple = TRUE),
            selectInput("response", "Variable Objetivo:", names(data()))
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
        options(skimr_strip_characters = TRUE)
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
                summary$total <- paste("Total de Filas:", nrow(df), " | Total de Columnas:", ncol(df))
            }
            if (input$checkClasses > 0) {
                classes_info <- sapply(df, function(col) {
                    if (is.factor(col) || is.character(col)) {
                        paste(names(table(col)), table(col), sep = ": ", collapse = ", ")
                    } else {
                        NULL
                    }
                })
                summary$classes <- paste("Clases por Variable Categorica:\n", 
                                         paste(names(classes_info), classes_info, sep = ": ", collapse = "\n"))
            }
            if (input$checkDuplicates > 0) {
                summary$duplicados <- paste("Total de Duplicados:", sum(duplicated(df)))
            }
            if (input$checkBalance > 0) {
                balanceo <- table(df[[input$response]])
                proporciones <- prop.table(balanceo)
                esta_equilibrado <- all(proporciones > 0.2 & proporciones < 0.8)
                conclusion <- ifelse(esta_equilibrado, "El conjunto de datos esta balanceado.", "El conjunto de datos no esta balanceado.")
                summary$balance <- paste("Balanceo de Clases:\n", 
                                         paste(names(balanceo), balanceo, sep = ": ", collapse = "\n"),
                                         "\nConclusion:", conclusion)
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
                outlier_text <- paste("Outliers por Variable:\n", 
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
            total_filas = length(var),
            media = mean(var, na.rm = TRUE),
            mediana = median(var, na.rm = TRUE),
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
            labs(x = input$singleVariable, y = "Frecuencia")
    })
    
    output$densityPlot <- renderPlot({
        req(input$singleVariable)
        ggplot(data(), aes_string(x = input$singleVariable)) +
            geom_density(fill = "blue", alpha = 0.5) +
            labs(x = input$singleVariable, y = "Densidad")
    })
    
    output$scatterPlot <- renderPlot({
        req(input$predictors, input$response)
        ggplot(data(), aes_string(x = input$predictors[1], y = input$response)) +
            geom_point() +
            labs(x = input$predictors[1], y = input$response)
    })
}

shinyApp(ui = ui, server = server)
