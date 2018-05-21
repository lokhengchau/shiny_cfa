library(shiny)
library(shinycssloaders)
library(lavaan)
library(semPlot)
library(psych)
library(grid)
library(gridExtra)
library(tidyverse)

ui <- fluidPage(
  titlePanel(
    "Confirmatory Factor Analysis"
    ),
  sidebarLayout(
    sidebarPanel(
      # tags$head(tags$style(type="text/css", "
      #        #loadmessage {
      #                      position: fixed;
      #                      top: 0px;
      #                      left: 0px;
      #                      width: 100%;
      #                      padding: 5px 0px 5px 0px;
      #                      text-align: center;
      #                      font-weight: bold;
      #                      font-size: 100%;
      #                      color: #000000;
      #                      z-index: 105;
      #                      }
      #                      ")),
      # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
      #                  tags$div("Loading... Please wait.",id="loadmessage")),
      fileInput(inputId = "file", label = "Import data file (.csv, .sav)"),
      uiOutput('main')
      ),
    mainPanel(
      uiOutput('main_results')
      )
    )
  )

server <- function(input, output, session) {
  filedata <- reactive({
    rdata <- input$file
    path <- rdata$datapath
    
    if (is.null(rdata)){
      return(NULL)
    } else if (substr(path, nchar(path) - 2, nchar(path)) == 'csv') {
      read.csv(path, stringsAsFactors = FALSE)
    } else if (substr(path, nchar(path) - 2, nchar(path)) == 'sav') {
      as.data.frame(foreign::read.spss(path))
    }
  })
  
  #Render main output once file is loaded
  output$main <- renderUI({
    df <- filedata()
    if (is.null(df)){
      return(NULL)
      }
    tabsetPanel(
      tabPanel("Factors",
               uiOutput('num_factor'),
               uiOutput('fact_select')
               ),
      tabPanel("Specifications",
               p(),
               radioButtons("constraints", label = 'Constraints',
                            choices = list("Scale factor = scale first indicator" = 1,
                                           "Factor variances = 1" = 2),
                            selected = 1),
               radioButtons("missing", label = "Missing Values Method",
                            choices = list("Listwise deletion" = 1,
                                           "Full information ML"= 2),
                            selected = 1),
               hr(),
               strong("Display summary:"),
               checkboxInput(inputId = 'fit', label = 'Fit Measures'),
               checkboxInput(inputId = 'std', label = "Standardized"),
               checkboxInput(inputId = 'rsq', label = "R-Square"),
               hr(),
               strong("Display covariance matrix:"),
               checkboxInput(input='obs_cov', label = 'Observed covariance matrix'),
               checkboxInput(inputId = 'implied_cov', label = 'Model-implied covariance matrix'),
               checkboxInput(inputId = 'resid_cov', label = "Residual matrix"),
               hr(),
               checkboxInput(inputId = 'cat', label = 'Ordinal categorical variables'),
               uiOutput('actionbutton')
               ),
      tabPanel("Plot",
               selectInput(inputId = 'layout', label = 'Select layout',
                           choices = list('tree','circle', 'spring'),
                           selected = 'tree'),
               checkboxInput(input = 'plot_residual', label = 'Display residuals',
                             value = 1),
               checkboxInput(input = 'plot_intercept', label = 'Display intercepts',
                             value = 1),
               checkboxInput(input = 'plot_threshold', label = 'Display thresholds'),
               selectInput(input = 'edge', label = 'Edge Weighted by:',
                           choices = list('Unweighted (gray ediges)' = 'path',
                                          'Parameter estimates' = 'est',
                                          'Standardized parameter estimates' = 'std'),
                           selected = 'path'),
               selectInput(input = 'edge_label', label = "Edge label:",
                           choices = list("Parameter estimates" = 'est',
                                          "Standardized parameter estimates" = 'std',
                                          "None" = 'no'),
                           selected = 'std'),
               numericInput(inputId = 'edge_font_size', label = "Font size for edge lables",
                            value = 1.2, min = .2, max = 3, step = .1),
               hr(),
               actionButton(inputId = 'replot', label = 'Apply')
               ))
    })
  
  output$main_results <- renderUI({
    if (is.null(filedata())){
      return(NULL)
    } else {
      tabsetPanel(id = 'outputtabs',
                  tabPanel(title = 'Data Table',
                           tags$head(tags$style(type='text/css',
                                                '#summary {font-size: 18px;}',
                                                '#obs_cov_out {font-size: 18px;}',
                                                '#implied_cov_out {font-size: 18px;}',
                                                '#resid_cov_out {font-size: 18px}')),
                           dataTableOutput('datatable')))
    }
  })
  
  output$datatable <- renderDataTable(
    filedata(),
    options = list(pageLength = 10)
  )
  
  observeEvent(input$cat, {
    if (input$cat == TRUE){
      output$ord_list <- renderUI({
        df <- filedata()
        selectInput(inputId = 'ord_input', label ='Select variables',
                    choices = colnames(df), multiple = TRUE)
        })
      } else {
        output$ord_list <- renderUI(
          return(NULL))
        }
    })
  
  output$num_factor <- renderUI({
    df <- filedata()
    if (is.null(df)){
      return(NULL)
    }
    tagList(numericInput(inputId = 'num_factor_input', label = "Number of factors",
              value = 1, min = 1, max = 5, step = 1))
  })

  output$fact_select <- renderUI({
    df <- filedata()
    if(is.null(input$num_factor_input)){
      return(NULL)
    }
    col_names <- colnames(df)
    factor_list <- 'tagList('
    for (i in seq_len(input$num_factor_input)){
      factor_list <- paste(factor_list,'selectInput(inputId="factor',toString(i),
                           '",label="Factor ',toString(i),
                           '",choices = col_names,multiple=TRUE),',sep='')
    }

    substr(factor_list, nchar(factor_list), nchar(factor_list)) <- ')'
    eval(parse(text = factor_list))
  })

  output$actionbutton <- renderUI({
    df <- filedata()
    if (is.null(df)){
      return(NULL)
    }
    tagList(hr(),
            actionButton(inputId = 'run', label = 'Run'))
  })
  
  check_input <- eventReactive(input$run, {
    proceed <- TRUE
    for (i in seq(input$num_factor_input)){
      e <- paste('input$factor',toString(i),sep='')
      if(is.null(eval(parse(text = e)))){
        proceed <- FALSE
      }
    }
    proceed
  })

  model_spec <- eventReactive(input$run,{
    df <- filedata()
    spec_list <- vector('list', input$num_factor_input)
    if (check_input() == TRUE){
      if (input$constraints == 2){
        for (i in seq(input$num_factor_input)){
          e <- paste('input$factor',toString(i),sep='')
          if(!is.null(eval(parse(text=e)))){
            f <- eval(parse(text=e))
            spec_list[[i]] <- paste('f',toString(i),'=~','NA*',
                                    paste(f,collapse="+"),'\n','f',
                                    toString(i),'~~','1*','f',
                                    toString(i),sep="")
          } else {
            spec_list[[i]] <- ""
          }
        }
      } else {
        for (i in seq_len(input$num_factor_input)){
          e <- paste('input$factor',toString(i),sep='')
          if(!is.null(eval(parse(text=e)))){
            f <- eval(parse(text=e))
            spec_list[[i]] <- paste('f',toString(i),'=~',
                                    paste(f,collapse="+"),sep="")
          } else {
            spec_list[[i]] <- ""
          }
        }
      }
      spec <- ''
      for (i in spec_list){
        if (spec == ''){
          spec <- i
        } else{
          spec <- paste(spec, i, sep='\n')
        }
      }
      ord <- NULL
      if (input$cat){
        ord <- vector()
        for (i in seq(input$num_factor_input)){
          vars <- eval(parse(text = paste0('input$factor',toString(i))))
          ord <- c(ord, vars)
        }
      }
      
      if (input$missing == 2){
        modelcfa <- cfa(spec, filedata(), ordered = ord, missing = "ML")
      } else {
        modelcfa <- cfa(spec, filedata(), ordered = ord)
      }
      
    } else {
      return(NULL)
    }
    })
  
  # observeEvent(input$run, {
  #   showModal(modalDialog(p("Running CFA. This might take a while."),
  #                         title = "Calculating...", easyClose = FALSE,
  #                         footer = NULL))
  #   model_spec()
  #   removeModal()
  # })
  
  observeEvent(input$run, {
    if (check_input() == TRUE){
      removeTab(inputId = 'outputtabs', target = 'Descriptives')
      removeTab(inputId = 'outputtabs', target = 'Summary')
      removeTab(inputId = 'outputtabs', target = 'Plot')
      appendTab(inputId = 'outputtabs',
                tab = tabPanel('Descriptives',
                               p(),
                               h3('Descriptive Summary'),
                               verbatimTextOutput('descriptive_summary_out'),
                               h3('Frequeny Distrubitons'),
                               withSpinner(plotOutput('descriptive_plots'))))
      appendTab(inputId = 'outputtabs', select = TRUE,
                tab = tabPanel('Summary',
                               fluidRow(
                                 column(8, h3('CFA Summary')),
                                 column(4, align = 'right',
                                        downloadButton('downloadsummary', 'Save summary'))
                               ),
                               verbatimTextOutput('summary'),
                               if (input$obs_cov == 1) {
                                 tagList(
                                   h3('Observed Covariance Matrix'),
                                   verbatimTextOutput('obs_cov_out')
                                 )
                               },
                               if (input$implied_cov == 1) {
                                 tagList(
                                   h3('Model-implied Covariance Matrix'),
                                   verbatimTextOutput('implied_cov_out')
                                 )
                               },
                               if (input$resid_cov == 1) {
                                 tagList(
                                   h3('Residual Covariance Matrix'),
                                   verbatimTextOutput('resid_cov_out')
                                 )
                               }
                               ))
      appendTab(inputId = 'outputtabs',
                tab = tabPanel('Plot',
                               withSpinner(plotOutput('semPlot', width = '900px', height = '600px'))))
    } else {
      showNotification("Please select variables for all factors.", action = NULL,
                       duration = 5, closeButton = TRUE,
                       id = NULL, type = 'warning',
                       session = session)
    }
  })
  
  observeEvent(input$replot, {
    updateTabsetPanel(session, inputId = 'outputtabs', selected = 'Plot')
  })
  
  descriptive_summary <- eventReactive(input$run, {
    if (check_input() == TRUE){
      vars_all <- vector()
      for (i in seq(input$num_factor_input)){
        vars <- eval(parse(text = paste0('input$factor',toString(i))))
        vars_all <- c(vars_all, vars)
      }
      filedata()[vars_all] %>% gather(key = 'Variable', value = 'val') %>%
        group_by(Variable) %>%
        dplyr::summarize(valid = sum(!is.na(val)), `NA` = sum(is.na(val)), 
                  min = min(val, na.rm = TRUE), max = max(val, na.rm = TRUE),
                  range = range(val, na.rm = TRUE)[2] - range(val, na.rm = TRUE)[1],
                  mean = round(mean(val, na.rm = TRUE), 2),
                  sd = round(sd(val, na.rm = TRUE), 2),
                  median = median(val, na.rm = TRUE),
                  skewness = round(skew(val, na.rm = TRUE), 2),
                  kurtosis = round(kurtosi(val, na.rm = TRUE), 2)) %>%
        as.data.frame()
    } else {
      return(NULL)
    }
    
  })
  
  output$descriptive_summary_out <- renderPrint({
    descriptive_summary()
  })
  
  descriptive_plots <- eventReactive(input$run, {
    if (check_input() == TRUE){
      vars_all <- vector()
      for (i in seq(input$num_factor_input)){
        vars <- eval(parse(text = paste0('input$factor',toString(i))))
        vars_all <- c(vars_all, vars)
      }
      p = list()
      df <- filedata()
      if (input$cat == 1){
        plot_data_column <- function (data, column){
          ggplot(data = data, aes_string(x = column)) +
            geom_bar(color = 'white', fill = 'skyblue') +
            labs(title = paste('Distribution of', column),
                 x = '') +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black")) +
            scale_y_continuous(expand = c(0,0))
        }
        
        p <- lapply(vars_all, plot_data_column, data = df)
      } else {
        
        plot_data_column <- function (data, column){
          ggplot(data = data, aes_string(x = column)) +
            geom_density() +
            labs(title = paste('Distribution of', column),
                 x = '') +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"))
        }
        
        p <- lapply(vars_all, plot_data_column, data = df)
      }
      grid.arrange(grobs = p, ncol = 4)
    } else {
      return(NULL)
    }
    
  })
  
  output$descriptive_plots <- renderPlot({
    descriptive_plots()
  })

  get_summary <- eventReactive(input$run ,{
    if (check_input() == TRUE){
      summary(model_spec(),
              fit.measures=input$fit, standardized=input$std, rsquare=input$rsq)
    } else {
      return(NULL)
    }
  })
  
  output$summary <- renderPrint({
    get_summary()
  })
  
  obs_cov <- eventReactive(input$run, {
    if (input$obs_cov == 1) {
      vars <- vector()
      for (i in seq(input$num_factor_input)){
        e <- paste('input$factor',toString(i),sep='')
        vars <- c(vars, eval(parse(text = e)))
      }
      if (input$cat == 1) {
        covar <- round(polychoric(filedata()[vars], na.rm = TRUE)$rho, 2)
      } else {
        covar <- round(var(filedata()[vars], na.rm = TRUE), 2)
      }
      covar[upper.tri(covar, diag = FALSE)] <- 0
      covar
    } else {
      return(NULL)
    }
  })
  
  output$obs_cov_out <- renderPrint({
    obs_cov()
    })
  
  implied_cov <- eventReactive(input$run, {
    if (input$implied_cov == 1) {
      round(lavInspect(model_spec(), what = 'cov.ov'),2)
    } else {
      return(NULL)
    }
    
  })
  
  output$implied_cov_out <- renderPrint({
    implied_cov()
  })
  
  resid_cov <- eventReactive(input$run, {
    if (input$resid_cov == 1) {
      round(resid(model_spec())$cov, 2)
    } else {
      return(NULL)
    }
  })
  
  output$resid_cov_out <- renderPrint({
    resid_cov()
  })
  
  get_plot <- eventReactive(input$run | input$replot,{
    if (check_input() == TRUE){
      semPaths(model_spec(), what = input$edge, whatLabels = input$edge_label, layout = input$layout,
               intercepts = input$plot_intercept, residuals = input$plot_residual,
               thresholds = input$plot_threshold, sizeMan = 8,
               edge.label.cex = input$edge_font_size)
    } else {
      return(NULL)
    }
  })
  
  output$semPlot <- renderPlot({
    get_plot()
  })
  
  output$downloadsummary <- downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      chars <- capture.output(print(
        summary(model_spec(),
                  fit.measures=input$fit, standardized=input$std,
                  rsquare=input$rsq)))
      chars <- chars[-length(chars)]
      if (!is.null(obs_cov())) {
        chars <- c(chars, '','Observed Covariance Matirx', '',
                   capture.output(obs_cov()))
      }
      if (!is.null(implied_cov())){
        chars <- c(chars, '', 'Model-implied Covariance Matrix', '',
                   capture.output(implied_cov()))
      }
      if (!is.null(resid_cov())){
        chars <- c(chars, '', 'Residual Covariance Matrix', '',
                   capture.output(resid_cov()))
      }
      writeLines(chars, file, sep = "\r\n")
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)