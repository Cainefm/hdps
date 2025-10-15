#' Interactive HDPS Covariate Selection App
#'
#' @title Interactive HDPS App
#' @description Shiny app for interactive covariate selection using HDPS algorithm
#' @return Shiny app
#' @export
#'
hdps_interactive <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny package is required for interactive app")
  }
  
  ui <- shiny::fluidPage(
    titlePanel("HDPS Interactive Covariate Selection"),
    
    sidebarLayout(
      sidebarPanel(
        h3("Data Input"),
        fileInput("data_file", "Upload CSV file", accept = c(".csv")),
        
        h3("Column Selection"),
        selectInput("id_col", "Patient ID Column", choices = NULL),
        selectInput("code_col", "Code Column", choices = NULL),
        selectInput("exposure_col", "Exposure Column", choices = NULL),
        selectInput("outcome_col", "Outcome Column", choices = NULL),
        
        h3("Parameters"),
        numericInput("n_candidates", "Max Candidates", value = 200, min = 10, max = 1000),
        numericInput("min_patients", "Min Patients", value = 10, min = 1, max = 100),
        checkboxInput("parallel", "Use Parallel Processing", value = FALSE),
        numericInput("n_cores", "Number of Cores", value = 2, min = 1, max = 8),
        
        h3("Actions"),
        actionButton("run_analysis", "Run HDPS Analysis", class = "btn-primary"),
        actionButton("download_results", "Download Results", class = "btn-success")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Data Preview",
            h3("Data Preview"),
            DT::dataTableOutput("data_preview")
          ),
          
          tabPanel("Candidates",
            h3("Candidate Covariates"),
            DT::dataTableOutput("candidates_table")
          ),
          
          tabPanel("Recurrence",
            h3("Recurrence Assessment"),
            DT::dataTableOutput("recurrence_table")
          ),
          
          tabPanel("Prioritization",
            h3("Covariate Prioritization"),
            DT::dataTableOutput("prioritization_table")
          ),
          
          tabPanel("Visualizations",
            h3("Bias Distribution"),
            plotOutput("bias_plot"),
            
            h3("Covariate Strength"),
            plotOutput("strength_plot"),
            
            h3("Bias vs Prevalence"),
            plotOutput("prevalence_plot")
          ),
          
          tabPanel("Summary",
            h3("Analysis Summary"),
            verbatimTextOutput("summary_text")
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    # Reactive data
    data <- reactive({
      req(input$data_file)
      read.csv(input$data_file$datapath)
    })
    
    # Update column choices when data is loaded
    observe({
      if (!is.null(data())) {
        choices <- names(data())
        updateSelectInput(session, "id_col", choices = choices)
        updateSelectInput(session, "code_col", choices = choices)
        updateSelectInput(session, "exposure_col", choices = choices)
        updateSelectInput(session, "outcome_col", choices = choices)
      }
    })
    
    # Data preview
    output$data_preview <- DT::renderDataTable({
      if (!is.null(data())) {
        DT::datatable(data(), options = list(pageLength = 10))
      }
    })
    
    # HDPS results
    hdps_results <- reactive({
      req(input$run_analysis)
      
      withProgress(message = "Running HDPS Analysis...", {
        # Convert to data.table
        dt <- as.data.table(data())
        
        # Run HDPS analysis
        results <- hdps_screen(
          data = dt,
          id_col = input$id_col,
          code_col = input$code_col,
          exposure_col = input$exposure_col,
          outcome_col = input$outcome_col,
          n_candidates = input$n_candidates,
          min_patients = input$min_patients,
          parallel = input$parallel,
          n_cores = input$n_cores
        )
        
        results
      })
    })
    
    # Candidates table
    output$candidates_table <- DT::renderDataTable({
      if (!is.null(hdps_results())) {
        DT::datatable(hdps_results()$candidates$candidates, 
                     options = list(pageLength = 10))
      }
    })
    
    # Recurrence table
    output$recurrence_table <- DT::renderDataTable({
      if (!is.null(hdps_results())) {
        DT::datatable(hdps_results()$recurrence, 
                     options = list(pageLength = 10))
      }
    })
    
    # Prioritization table
    output$prioritization_table <- DT::renderDataTable({
      if (!is.null(hdps_results())) {
        DT::datatable(hdps_results()$prioritization, 
                     options = list(pageLength = 10))
      }
    })
    
    # Bias plot
    output$bias_plot <- renderPlot({
      if (!is.null(hdps_results())) {
        plot_bias_distribution(hdps_results()$prioritization, top_n = 20)
      }
    })
    
    # Strength plot
    output$strength_plot <- renderPlot({
      if (!is.null(hdps_results())) {
        plot_covariate_strength(hdps_results()$prioritization)
      }
    })
    
    # Prevalence plot
    output$prevalence_plot <- renderPlot({
      if (!is.null(hdps_results())) {
        plot_bias_vs_prevalence(hdps_results()$prioritization)
      }
    })
    
    # Summary text
    output$summary_text <- renderText({
      if (!is.null(hdps_results())) {
        paste(
          "HDPS Analysis Summary:\n",
          "Number of candidates:", nrow(hdps_results()$candidates$candidates), "\n",
          "Number of recurrence variables:", ncol(hdps_results()$recurrence) - 1, "\n",
          "Number of prioritized covariates:", nrow(hdps_results()$prioritization), "\n",
          "Top covariate:", hdps_results()$prioritization[order(absLogBias, decreasing = TRUE)]$code[1]
        )
      }
    })
    
    # Download results
    output$download_results <- downloadHandler(
      filename = function() {
        paste("hdps_results_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        if (!is.null(hdps_results())) {
          write.csv(hdps_results()$prioritization, file, row.names = FALSE)
        }
      }
    )
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
