#' Interactive HDPS Covariate Selection App
#'
#' @description Shiny app for interactive covariate selection using HDPS algorithm
#' @return Shiny app
#' @export
hdps_interactive <- function() {
  required_packages <- c("shiny", "DT", "data.table")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(pkg, " package is required for interactive app")
    }
  }
  
  # Load required packages
  
  if (!"data.table" %in% loadedNamespaces()) {
    stop("Failed to load data.table package")
  }
  
  ui <- shiny::fluidPage(
    shiny::titlePanel("HDPS Interactive Covariate Selection"),
    
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h3("Data Input"),
        shiny::p("Upload RDS or RDA files containing data.table objects:"),
        shiny::fileInput("master_file", "Upload Master Dataset (RDS/RDA)", accept = c(".rds",".rda")),
        shiny::fileInput("other_file", "Upload Other Data (dx/px/rx) (RDS/RDA)", accept = c(".rds",".rda")),
        shiny::selectInput("data_type", "Data Type", 
                          choices = c("dx" = "dx", "px" = "px", "rx" = "rx"),
                          selected = "dx"),
        
        shiny::h3("Column Selection"),
        shiny::selectInput("id_col", "Patient ID Column", choices = NULL),
        shiny::selectInput("code_col", "Code Column", choices = NULL),
        shiny::selectInput("exposure_col", "Exposure Column", choices = NULL),
        shiny::selectInput("outcome_col", "Outcome Column", choices = NULL),
        
        shiny::h3("Parameters"),
        shiny::numericInput("n_candidates", "Max Candidates", value = 200, min = 10, max = 1000),
        shiny::numericInput("min_patients", "Min Patients", value = 10, min = 1, max = 100),
        
        shiny::h3("Actions"),
        shiny::actionButton("run_analysis", "Run HDPS Analysis", class = "btn-primary"),
        shiny::actionButton("download_results", "Download Results", class = "btn-success")
      ),
      
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Data Preview",
            shiny::h3("Data Preview"),
            DT::dataTableOutput("data_preview")
          ),
          
          shiny::tabPanel("Candidates",
            shiny::h3("Candidate Covariates"),
            DT::dataTableOutput("candidates_table")
          ),
          
          shiny::tabPanel("Recurrence",
            shiny::h3("Recurrence Assessment"),
            DT::dataTableOutput("recurrence_table")
          ),
          
          shiny::tabPanel("Prioritization",
            shiny::h3("Covariate Prioritization"),
            DT::dataTableOutput("prioritization_table")
          ),
          
          shiny::tabPanel("Visualizations",
            shiny::h3("Bias Distribution"),
            shiny::plotOutput("bias_plot"),
            
            shiny::h3("Covariate Strength"),
            shiny::plotOutput("strength_plot"),
            
            shiny::h3("Bias vs Prevalence"),
            shiny::plotOutput("prevalence_plot")
          ),
          
          shiny::tabPanel("Summary",
            shiny::h3("Analysis Summary"),
            shiny::verbatimTextOutput("summary_text")
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    # Helper function to load data based on file extension
    load_data_file <- function(file_path) {
      dt <- if (grepl("\\.rda$", file_path, ignore.case = TRUE)) {
        env <- new.env()
        load(file_path, envir = env)
        get(ls(env)[1], envir = env)
      } else {
        readRDS(file_path)
      }
      
      if (!data.table::is.data.table(dt)) {
        data.table::setDT(dt)
      }
      dt
    }
    
    # Reactive data
    master_data <- shiny::reactive({
      shiny::req(input$master_file)
      load_data_file(input$master_file$datapath)
    })
    
    other_data <- shiny::reactive({
      shiny::req(input$other_file)
      load_data_file(input$other_file$datapath)
    })
    
    # Update column choices when data is loaded
    shiny::observe({
      if (!is.null(master_data()) && !is.null(other_data())) {
        # Use master data for exposure/outcome columns
        master_choices <- names(master_data())
        # Use other data for code columns
        other_choices <- names(other_data())
        
        shiny::updateSelectInput(session, "id_col", choices = master_choices)
        shiny::updateSelectInput(session, "exposure_col", choices = master_choices)
        shiny::updateSelectInput(session, "outcome_col", choices = master_choices)
        shiny::updateSelectInput(session, "code_col", choices = other_choices)
      }
    })
    
    # Data preview
    output$data_preview <- DT::renderDataTable({
      if (!is.null(master_data()) && !is.null(other_data())) {
        # Show master data preview
        DT::datatable(master_data(), options = list(pageLength = 10))
      }
    })
    
    # HDPS results
    hdps_results <- shiny::reactive({
      shiny::req(input$run_analysis)
      
      shiny::withProgress(message = "Running HDPS Analysis...", {
        # Single domain analysis with master data
        results <- hdps(
          data = other_data(),
          id_col = input$id_col,
          code_col = input$code_col,
          exposure_col = input$exposure_col,
          outcome_col = input$outcome_col,
          master_data = master_data(),
          n_candidates = input$n_candidates,
          min_patients = input$min_patients
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
    output$bias_plot <- shiny::renderPlot({
      if (!is.null(hdps_results())) {
        plot_bias_distribution(hdps_results()$prioritization, top_n = 20)
      }
    })
    
    # Strength plot
    output$strength_plot <- shiny::renderPlot({
      if (!is.null(hdps_results())) {
        plot_covariate_strength(hdps_results()$prioritization)
      }
    })
    
    # Prevalence plot
    output$prevalence_plot <- shiny::renderPlot({
      if (!is.null(hdps_results())) {
        plot_bias_vs_prevalence(hdps_results()$prioritization)
      }
    })
    
    # Summary text
    output$summary_text <- shiny::renderText({
      if (!is.null(hdps_results())) {
        paste(
          "HDPS Analysis Summary:\n",
          "Data type:", input$data_type, "\n",
          "Number of candidates:", nrow(hdps_results()$candidates$candidates), "\n",
          "Number of recurrence variables:", ncol(hdps_results()$recurrence) - 1, "\n",
          "Number of prioritized covariates:", nrow(hdps_results()$prioritization), "\n",
          "Top covariate:", hdps_results()$prioritization[order(absLogBias, decreasing = TRUE)]$code[1]
        )
      }
    })
    
    # Download results
    output$download_results <- shiny::downloadHandler(
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
