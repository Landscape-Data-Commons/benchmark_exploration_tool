#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(httr)
library(stringr)
# library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        ".shiny-notification {
          position:fixed;
          top: calc(30%);
          left: calc(5%);
          width: calc(25%);
          opacity: 1;
          font-weight: bold;
          box-shadow: 0 0 0 rgba(181,181,181, 0.4);
          animation: pulse 2s infinite;
        }
        @-webkit-keyframes pulse {
          0% {
            -webkit-box-shadow: 0 0 0 0 rgba(181,181,181, 0.4);
          }
          70% {
            -webkit-box-shadow: 0 0 0 10px rgba(181,181,181, 0);
          }
          100% {
            -webkit-box-shadow: 0 0 0 0 rgba(181,181,181, 0);
          }
        }
        @keyframes pulse {
          0% {
            -moz-box-shadow: 0 0 0 0 rgba(181,181,181, 0.4);
            box-shadow: 0 0 0 0 rgba(181,181,181, 0.4);
          }
          70% {
            -moz-box-shadow: 0 0 0 10px rgba(181,181,181, 0);
            box-shadow: 0 0 0 10px rgba(181,181,181, 0);
          }
          100% {
            -moz-box-shadow: 0 0 0 0 rgba(181,181,181, 0);
            box-shadow: 0 0 0 0 rgba(181,181,181, 0);
          }
        }"
      )
    )
  ),
  
  # Application title
  titlePanel(img(src = "combined_logos.png",
                 align = "right"),
             windowTitle = "Plotting Indicator Values and Benchmarks"),
  titlePanel(title = "Plotting Indicator Values and Benchmarks"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "data_source",
                   label = "Data source",
                   choices = c("Upload" = "upload", "Query Landscape Data Commons" = "ldc")),
      conditionalPanel(condition = "input.data_source == 'upload'",
                       fileInput(inputId = "raw_data",
                                 label = "Exported TerrADat CSV",
                                 multiple = FALSE,
                                 accept = "CSV")),
      conditionalPanel(condition = "input.data_source == 'ldc'",
                       textInput(inputId = "ecosite_id",
                                 label = "Ecological Site ID",
                                 value = "",
                                 placeholder = "R042XB012NM"),
                       actionButton(inputId = "fetch_data",
                                    label = "Fetch data from the Landscape Data Commons")),
      
      hr(),
      selectInput(inputId = "id_variables",
                  label = "Variable(s) with identifying, non-indicator information",
                  multiple = TRUE,
                  choices = c("")),
      selectInput(inputId = "variable",
                  label = "Indicator to plot",
                  choices = c("")),
      textInput(inputId = "variable_name",
                label = "X-axis label",
                value = ""),
      textInput(inputId = "quantiles",
                label = "Quantile break percentages, separated by commas",
                value = "25, 50, 75"),
      
      checkboxInput(inputId = "compare",
                    label = "Mark comparison value on figures",
                    value = FALSE),
      
      conditionalPanel(condition = "input.compare",
                       radioButtons(inputId = "comparison_type",
                                    label = "Comparison value source",
                                    choices = c("Manual" = "manual", "From current data" = "plot_id"),
                                    selected = "manual"),
                       conditionalPanel(condition = "input.comparison_type == 'manual'",
                                        numericInput(inputId = "manual_comparison_value",
                                                     label = "Indicator value",
                                                     value = 0)),
                       conditionalPanel(condition = "input.comparison_type == 'plot_id'",
                                        selectInput(inputId = "identifying_variable",
                                                    label = "Uniquely identifying variable",
                                                    choices = c("")),
                                        selectInput(inputId = "comparison_plot_id",
                                                    label = "Comparison unique ID(s)",
                                                    multiple = TRUE,
                                                    choices = c("")))
      ),
      
      checkboxInput(inputId = "time_series",
                    label = "Plot yearly time series",
                    value = FALSE),
      conditionalPanel(condition = "input.time_series",
                       selectInput(inputId = "date_variable",
                                   label = "Date variable",
                                   choices = c("")),
                       radioButtons(inputId = "date_type",
                                    label = "The date format is:",
                                    choices = c("2020-10-30" = "iso8601",
                                                "October 30, 2020" = "month_day_year",
                                                "2020" = "year"))
                       
      ),
      
      hr(),
      # Only show the plot button if data have been uploaded/downloaded
      conditionalPanel(condition = "input.variable != ''",
                       actionButton(inputId = "plot_button",
                                    label = "Plot indicator distribution!")),
      # Only show if there are plot images to download
      conditionalPanel(condition = "input.plot_button >= 1",
                       downloadButton(outputId = 'downloadData',
                                      label = 'Download results')),
      
      helpText("Created at the USDA-ARS Jornada Experimental Range in collaboration with the Bureau of Land Management")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "maintabs",
                  
                  tabPanel(title = "Instructions",
                           includeHTML("instructions.html")),
                  
                  tabPanel(title = "Benchmark ranges",
                           checkboxInput(inputId = "use_benchmarks",
                                         label = "Apply benchmarks",
                                         value = FALSE),
                           
                           conditionalPanel(condition = "input.use_benchmarks",
                                            selectInput(inputId = "range_count",
                                                        label = "Number of benchmark ranges",
                                                        choices = 2:6,
                                                        selected = 2),
                                            # Labels
                                            fluidRow(column(width = 2,
                                                            helpText("Lower limit")),
                                                     column(width = 1,
                                                            helpText("Lower limit relation")),
                                                     column(width = 1,
                                                            helpText("Indicator value")),
                                                     column(width = 1,
                                                            helpText("Upper limit relation")),
                                                     column(width = 2,
                                                            helpText("Upper limit")),
                                                     
                                                     column(width = 3,
                                                            helpText("Benchmark category")),
                                                     hr()
                                            ),
                                            # Row 1
                                            fluidRow(column(width = 2,
                                                            numericInput(inputId = "benchmark_range_limit_lower_1",
                                                                         label = "",
                                                                         value = 0,
                                                                         min = -Inf,
                                                                         max = Inf)),
                                                     column(width = 1,
                                                            selectInput(inputId = "benchmark_relationship_lower_1",
                                                                        label = "",
                                                                        choices = c("<", "<="),
                                                                        selected = "<=")),
                                                     column(width = 1,
                                                            helpText("x")),
                                                     column(width = 1,
                                                            selectInput(inputId = "benchmark_relationship_upper_1",
                                                                        label = "",
                                                                        choices = c("<", "<="),
                                                                        selected = "<=")),
                                                     column(width = 2,
                                                            numericInput(inputId = "benchmark_range_limit_upper_1",
                                                                         label = "",
                                                                         value = 50,
                                                                         min = -Inf,
                                                                         max = Inf)),
                                                     
                                                     column(width = 3,
                                                            textInput(inputId = "benchmark_category_1",
                                                                      label = "",
                                                                      placeholder = "Not Meeting",
                                                                      value = "Not Meeting")),
                                                     hr()
                                            ),
                                            # Row 2
                                            fluidRow(column(width = 2,
                                                            numericInput(inputId = "benchmark_range_limit_lower_2",
                                                                         label = "",
                                                                         value = 50,
                                                                         min = -Inf,
                                                                         max = Inf)),
                                                     column(width = 1,
                                                            selectInput(inputId = "benchmark_relationship_lower_2",
                                                                        label = "",
                                                                        choices = c("<", "<="),
                                                                        selected = "<")),
                                                     column(width = 1,
                                                            helpText("x")),
                                                     column(width = 1,
                                                            selectInput(inputId = "benchmark_relationship_upper_2",
                                                                        label = "",
                                                                        choices = c("<", "<="),
                                                                        selected = "<=")),
                                                     column(width = 2,
                                                            numericInput(inputId = "benchmark_range_limit_upper_2",
                                                                         label = "",
                                                                         value = 100,
                                                                         min = -Inf,
                                                                         max = Inf)),
                                                     
                                                     column(width = 3,
                                                            textInput(inputId = "benchmark_category_2",
                                                                      label = "",
                                                                      placeholder = "Not Meeting",
                                                                      value = "Meeting")),
                                                     hr()
                                            ),
                                            # Row 3
                                            conditionalPanel(condition = "input.range_count > 2",
                                                             fluidRow(column(width = 2,
                                                                             numericInput(inputId = "benchmark_range_limit_lower_3",
                                                                                          label = "",
                                                                                          value = 0,
                                                                                          min = -Inf,
                                                                                          max = Inf)),
                                                                      column(width = 1,
                                                                             selectInput(inputId = "benchmark_relationship_lower_3",
                                                                                         label = "",
                                                                                         choices = c("<", "<="),
                                                                                         selected = "<")),
                                                                      column(width = 1,
                                                                             helpText("x")),
                                                                      column(width = 1,
                                                                             selectInput(inputId = "benchmark_relationship_upper_3",
                                                                                         label = "",
                                                                                         choices = c("<", "<="),
                                                                                         selected = "<=")),
                                                                      column(width = 2,
                                                                             numericInput(inputId = "benchmark_range_limit_upper_3",
                                                                                          label = "",
                                                                                          value = 50,
                                                                                          min = -Inf,
                                                                                          max = Inf)),
                                                                      
                                                                      column(width = 3,
                                                                             textInput(inputId = "benchmark_category_3",
                                                                                       label = "",
                                                                                       placeholder = "Not Meeting",
                                                                                       value = "")),
                                                                      hr()
                                                             )),
                                            # Row 4
                                            conditionalPanel(condition = "input.range_count > 3",
                                                             fluidRow(column(width = 2,
                                                                             numericInput(inputId = "benchmark_range_limit_lower_4",
                                                                                          label = "",
                                                                                          value = 0,
                                                                                          min = -Inf,
                                                                                          max = Inf)),
                                                                      column(width = 1,
                                                                             selectInput(inputId = "benchmark_relationship_lower_4",
                                                                                         label = "",
                                                                                         choices = c("<", "<="),
                                                                                         selected = "<")),
                                                                      column(width = 1,
                                                                             helpText("x")),
                                                                      column(width = 1,
                                                                             selectInput(inputId = "benchmark_relationship_upper_4",
                                                                                         label = "",
                                                                                         choices = c("<", "<="),
                                                                                         selected = "<=")),
                                                                      column(width = 2,
                                                                             numericInput(inputId = "benchmark_range_limit_upper_4",
                                                                                          label = "",
                                                                                          value = 50,
                                                                                          min = -Inf,
                                                                                          max = Inf)),
                                                                      
                                                                      column(width = 3,
                                                                             textInput(inputId = "benchmark_category_4",
                                                                                       label = "",
                                                                                       placeholder = "Not Meeting",
                                                                                       value = "")),
                                                                      hr()
                                                             )),
                                            # Row 5
                                            conditionalPanel(condition = "input.range_count > 4",
                                                             fluidRow(column(width = 2,
                                                                             numericInput(inputId = "benchmark_range_limit_lower_5",
                                                                                          label = "",
                                                                                          value = 0,
                                                                                          min = -Inf,
                                                                                          max = Inf)),
                                                                      column(width = 1,
                                                                             selectInput(inputId = "benchmark_relationship_lower_5",
                                                                                         label = "",
                                                                                         choices = c("<", "<="),
                                                                                         selected = "<")),
                                                                      column(width = 1,
                                                                             helpText("x")),
                                                                      column(width = 1,
                                                                             selectInput(inputId = "benchmark_relationship_upper_5",
                                                                                         label = "",
                                                                                         choices = c("<", "<="),
                                                                                         selected = "<=")),
                                                                      column(width = 2,
                                                                             numericInput(inputId = "benchmark_range_limit_upper_5",
                                                                                          label = "",
                                                                                          value = 50,
                                                                                          min = -Inf,
                                                                                          max = Inf)),
                                                                      
                                                                      column(width = 3,
                                                                             textInput(inputId = "benchmark_category_5",
                                                                                       label = "",
                                                                                       placeholder = "Not Meeting",
                                                                                       value = "")),
                                                                      hr()
                                                             )),
                                            # Row 6
                                            conditionalPanel(condition = "input.range_count > 5",
                                                             fluidRow(column(width = 2,
                                                                             numericInput(inputId = "benchmark_range_limit_lower_6",
                                                                                          label = "",
                                                                                          value = 0,
                                                                                          min = -Inf,
                                                                                          max = Inf)),
                                                                      column(width = 1,
                                                                             selectInput(inputId = "benchmark_relationship_lower_6",
                                                                                         label = "",
                                                                                         choices = c("<", "<="),
                                                                                         selected = "<")),
                                                                      column(width = 1,
                                                                             helpText("x")),
                                                                      column(width = 1,
                                                                             selectInput(inputId = "benchmark_relationship_upper_6",
                                                                                         label = "",
                                                                                         choices = c("<", "<="),
                                                                                         selected = "<=")),
                                                                      column(width = 2,
                                                                             numericInput(inputId = "benchmark_range_limit_upper_6",
                                                                                          label = "",
                                                                                          value = 50,
                                                                                          min = -Inf,
                                                                                          max = Inf)),
                                                                      
                                                                      column(width = 3,
                                                                             textInput(inputId = "benchmark_category_6",
                                                                                       label = "",
                                                                                       placeholder = "Not Meeting",
                                                                                       value = "")),
                                                                      hr()
                                                             ))
                           ),
                           
                  ),
                  
                  tabPanel(title = "Results",
                           plotOutput("quantiles_plot"),
                           textOutput("quantile_caption"),
                           plotOutput("benchmark_plot"),
                           textOutput("benchmark_summary"),
                           plotOutput("timeseries_plot"),
                           textOutput("timeseries_summary")),
                  tabPanel(title = "Data",
                           dataTableOutput("data_table")))
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Create a workspace list to store objects and values in
  workspace <- reactiveValues(placeholder = TRUE,
                              temp_directory = tempdir(),
                              original_directory = getwd(),
                              quantiles = c(0.25, 0.5, 0.75),
                              # The color palette for the figures
                              palette = c("#f5bb57ff",
                                          "#f8674cff",
                                          "#4a8b9fff",
                                          "#685b7fff",
                                          "#c95294ff",
                                          "#f5a9c6ff",
                                          "#75c6c5ff",
                                          "#fd6794ff"))
  
  # Nor are there plots
  output$plot_files <- renderText("FALSE")
  
  #### When a CSV is uploaded, do this ####
  observeEvent(eventExpr = input$raw_data,
               handlerExpr = {
                 workspace[["raw_data"]] <- read.csv(input$raw_data$datapath,
                                                     stringsAsFactors = FALSE)
               })
  
  #### When someone fetches data from the Landscape Data Commons, do this ####
  observeEvent(eventExpr = input$fetch_data,
               handlerExpr = {
                 showNotification(ui = "Downloading data from the LDC. Please wait.",
                                  duration = NULL,
                                  closeButton = FALSE,
                                  id = "downloading",
                                  type = "message")
                 # output$ecosite_error <- renderText("")
                 # Only do anything if there's an ecosite ID
                 if (input$ecosite_id != "") {
                   # Make sure it's uppercase
                   ecosite_id <- toupper(input$ecosite_id)
                   
                   # Handle multiple requested ecosites at once!
                   ecosite_id_vector <- stringr::str_split(string = ecosite_id,
                                                           pattern = ",",
                                                           simplify = TRUE)
                   ecosite_id_vector <- trimws(ecosite_id_vector)
                   
                   query_results_list <- lapply(X = ecosite_id_vector,
                                                FUN = function(X){
                                                  # Build the query
                                                  query <- paste0("https://api.landscapedatacommons.org/api/",
                                                                  "geoindicators?",
                                                                  "EcologicalSiteId=",
                                                                  X)
                                                  
                                                  # Getting the data via curl
                                                  # connection <- curl::curl(query)
                                                  # results_raw <- readLines(connection)
                                                  # results <- jsonlite::fromJSON(results_raw)
                                                  message("Attempting to query EDIT")
                                                  message(query)
                                                  # Full query results for geoindicators based on ecosite
                                                  full_results <- httr::GET(query,
                                                                            config = httr::timeout(30))
                                                  # Grab only the data portion
                                                  results_raw <- full_results[["content"]]
                                                  # Convert from raw to character
                                                  results_character <- rawToChar(results_raw)
                                                  # Convert from character to data frame
                                                  results <- jsonlite::fromJSON(results_character)
                                                  message("Results converted from json to character")
                                                  
                                                  # THIS IS IMPORTANT!
                                                  # Remove rows without data, which apparently happens
                                                  # We'll just trust that the indicator names are going to follow the same pattern
                                                  # so we can check the first variable starting with "AH_" for NA values
                                                  if (length(results) > 0) {
                                                    results_var_names <- names(results)
                                                    indicator_var_indices <- grep(x = results_var_names,
                                                                                  pattern = "^AH_")
                                                    # So this assumes that if the any single indicator value for an observation is NA,
                                                    # then ALL indicator values for that observation will be NA
                                                    test_indicator_var <- results_var_names[indicator_var_indices[1]]
                                                    
                                                    indices_with_data <- !is.na(results[[test_indicator_var]])
                                                    
                                                    results[indices_with_data, ]
                                                  } else {
                                                    NULL
                                                  }
                                                  
                                                })
                   
                   results <- do.call(rbind,
                                      query_results_list)
                   message("results currently are:")
                   message(results)
                   # So we can tell the user later which actually got queried
                   if (is.null(results)) {
                     workspace$missing_ecosites <- ecosite_id_vector
                   } else {
                     workspace$queried_ecosites <- unique(results$EcologicalSiteId)
                     workspace$missing_ecosites <- ecosite_id_vector[!(ecosite_id_vector %in% workspace$queried_ecosites)]
                   }
                   
                   if (length(workspace$missing_ecosites) > 0) {
                     ecosite_error <- paste0("The following ecological site IDs are not associated with data in the LDC: ",
                                             paste(workspace$missing_ecosites,
                                                   collapse = ", "))
                     showNotification(ui = ecosite_error,
                                      duration = NULL,
                                      closeButton = TRUE,
                                      type = "warning",
                                      id = "ecosite_error")
                   }
                   
                   
                   # Only keep going if there are results!!!!
                   if (length(results) > 0) {
                     # Convert from character to numeric variables where possible
                     data_corrected <- lapply(X = names(results),
                                              data = results,
                                              FUN = function(X, data){
                                                # Get the current variable values as a vector
                                                vector <- data[[X]]
                                                # Try to coerce into numeric
                                                numeric_vector <- as.numeric(vector)
                                                # If that works without introducing NAs, return the numeric vector
                                                # Otherwise, return the original character vector
                                                if (all(!is.na(numeric_vector))) {
                                                  return(numeric_vector)
                                                } else {
                                                  return(vector)
                                                }
                                              })
                     
                     # From some reason co.call(cbind, data_corrected) was returning a list not a data frame
                     # so I'm resorting to using dplyr
                     data <- dplyr::bind_cols(data_corrected)
                     # Correct the names of the variables
                     names(data) <- names(results)
                     
                     # Put it in the workspace list
                     workspace$raw_data <- data
                   } else {
                     workspace$raw_data <- NULL
                   }
                 }
                 removeNotification(id = "downloading")
               })
  
  #### Raw data are updated, do this ####
  observeEvent(eventExpr = workspace$raw_data,
               handlerExpr = {
                 if (!is.null(workspace$raw_data)) {
                   # Get the variable names in the CSV
                   variable_names <- names(workspace$raw_data)
                   
                   # Look at each column and determine if it can be coerced into numeric
                   viable_variables <- apply(X = workspace$raw_data,
                                             MARGIN = 2,
                                             FUN = function(X){
                                               vector <- X
                                               numeric_vector <- as.numeric(vector)
                                               any(!is.na(numeric_vector))
                                             })
                   
                   # Update the dropdown options to include those variables
                   updateSelectInput(session = session,
                                     inputId = "id_variables",
                                     choices = c("", variable_names))
                   updateSelectInput(session = session,
                                     inputId = "variable",
                                     choices = c("", variable_names[viable_variables]))
                   updateSelectInput(session = session,
                                     inputId = "identifying_variable",
                                     choices = c("", variable_names))
                   updateSelectInput(session = session,
                                     inputId = "date_variable",
                                     choices = c("", variable_names))
                   
                   output$data_table <- renderDataTable(workspace$raw_data)
                   
                   updateTabsetPanel(session = session,
                                     inputId = "maintabs",
                                     selected = "Data")
                 }
               })
  
  #### When the indicator is updated, do this ####
  observeEvent(eventExpr = input$variable,
               handlerExpr = {
                 updateTextInput(session,
                                 inputId = "variable_name",
                                 value = input$variable)
               })
  
  #### When the uniquely identifying variable is updated, do this ####
  observeEvent(eventExpr = input$identifying_variable,
               handlerExpr = {
                 if (input$identifying_variable == "") {
                   updateSelectInput(session = session,
                                     inputId = "comparison_plot_id",
                                     choices = "")
                 } else {
                   if (length(unique(workspace$raw_data[[input$identifying_variable]])) == nrow(workspace$raw_data)) {
                     updateSelectInput(session = session,
                                       inputId = "comparison_plot_id",
                                       choices = workspace$raw_data[[input$identifying_variable]])
                     removeNotification(id = "uid_error")
                   } else {
                     uid_error <- paste0("The variable ", input$identifying_variable, " does not contain uniquely identifying keys. Please select a unique identifier.")
                     showNotification(ui = uid_error,
                                      duration = NULL,
                                      closeButton = TRUE,
                                      id = "uid_error",
                                      type = "error")
                     updateSelectInput(session = session,
                                       inputId = "comparison_plot_id",
                                       choices = "")
                   }
                 }
                 
               })
  
  
  #### When the identifying variables are updated, do this ####
  observeEvent(eventExpr = input$id_variables,
               handlerExpr = {
                 # Get the variable names in the CSV
                 variable_names <- names(workspace$raw_data)
                 
                 # Look at each column and determine if it can be coerced into numeric
                 viable_variables <- apply(X = workspace$raw_data,
                                           MARGIN = 2,
                                           FUN = function(X){
                                             vector <- X
                                             numeric_vector <- as.numeric(vector)
                                             any(!is.na(numeric_vector))
                                           })
                 
                 not_id_variable <- !(variable_names %in% input$id_variables)
                 
                 # Update!
                 updateSelectInput(session = session,
                                   inputId = "variable",
                                   choices = c("", variable_names[viable_variables & not_id_variable]))
               })
  
  ### When the quantiles are updated, do this ####
  observeEvent(eventExpr = input$quantiles,
               handlerExpr = {
                 # Split the string along the commas
                 quantiles_vector <- unlist(strsplit(input$quantiles,
                                                     split = ","))
                 # Remove any spaces
                 quantiles_vector <- trimws(quantiles_vector)
                 
                 # Convert to numeric
                 quantiles_vector <- as.numeric(quantiles_vector)
                 quantiles_vector <- unique(quantiles_vector)
                 
                 # IF ANY AREN'T NUMERIC, WE HAVE A PROBLEM
                 if (any(is.na(quantiles_vector))) {
                   showNotification(ui = "All quantile values must be numeric values between 0 and 100, separated by commas. Unless corrected, the default of 50% will be used.",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    type = "error",
                                    id = "quantile_error_nonnumeric")
                   # Default to just 50%
                   workspace[["quantiles"]] <- c(0.5)
                 } else {
                   removeNotification(id = "quantile_error_nonnumeric")
                   
                   # Convert to proportion for quantile()
                   quantiles_vector <- quantiles_vector / 100
                   
                   # If any of the percentages were < 0 or > 100, there's a problem
                   if (all(quantiles_vector >= 0) & all(quantiles_vector <= 1)) {
                     workspace[["quantiles"]] <- quantiles_vector
                   } else {
                     showNotification(ui = "All quantile values must be between 0 and 100, separated by commas. Unless corrected, the default of 50% will be used.",
                                      duration = NULL,
                                      closeButton = TRUE,
                                      type = "error",
                                      id = "quantile_error_valuerange")
                     # Default to just 50%
                     workspace[["quantiles"]] <- c(0.5)
                   }
                 }
                 
               })
  
  #### When the plot button is hit, do this ####
  observeEvent(eventExpr = input$plot_button,
               handlerExpr = {
                 showNotification(ui = "Drawing plots. Please wait.",
                                  duration = NULL,
                                  closeButton = FALSE,
                                  type = "message",
                                  id = "plotting")
                 # Get a copy of the data to manipulate for plotting
                 plotting_data <- workspace$raw_data

                 
                 # Only plot if there's data!!!!
                 if (!is.null(plotting_data)){
                   # Grab the variable name to work with
                   variable_name <- input$variable
                   
                   #! What are the quantile proportions?
                   quantile_proportions <- workspace$quantiles
                   # Make sure they're in order from smallest to largest
                   quantile_proportions <- quantile_proportions[order(quantile_proportions)]
                   
                   # Write that current variable to a new one so we can plot easily
                   # And make sure it's numeric, not integer or something
                   plotting_data[["current_variable"]] <- as.numeric(plotting_data[[variable_name]])
                   
                   # Get those values as a vector
                   current_data_vector <- plotting_data[[variable_name]]
                   
                   # Find the quantile breaks
                   quantiles <- quantile(current_data_vector,
                                         probs = quantile_proportions,
                                         na.rm = TRUE)
                   # Drop 0% and 100% if they're in there
                   quantile_indices <- !(names(quantiles) %in% c("0%", "100%"))
                   quantiles <- quantiles[quantile_indices]
                   
                   # Write in the quantile that the values belong in, starting with all of them in 100%
                   plotting_data[["Quantile"]] <- "100%"
                   # Then in order from largest to smallest, assign the rest of the quantiles
                   for (current_quantile in names(quantiles)[length(quantiles):1]) {
                     plotting_data[["Quantile"]][plotting_data[["current_variable"]] <= quantiles[current_quantile]] <- current_quantile
                   }
                   
                   # Correct the order of the legend items
                   quantile_names <- unique(c(names(quantiles), "100%"))
                   plotting_data[["Quantile"]] <- factor(plotting_data[["Quantile"]],
                                                         levels = quantile_names)
                   
                   
                   # Make the dang plot happen!
                   if (nrow(plotting_data) > 0) {
                     message("Plotting the quantiles plot")
                     
                     workspace$quantile_plot <- ggplot() +
                       geom_histogram(data = plotting_data,
                                      aes(y = current_variable,
                                          fill = Quantile),
                                      binwidth = 1) +
                       scale_fill_manual(values = workspace$palette) +
                       geom_hline(yintercept = quantiles,
                                  size = 1,
                                  color = "gray50") +
                       scale_y_continuous(expand = c(0, 0)) +
                       scale_x_continuous(expand = c(0, 0)) +
                       labs(x = "Count of data points",
                            y = input$variable_name) +
                       theme(panel.grid = element_blank(),
                             panel.background = element_rect(fill = "gray95")) +
                       coord_flip()
                     
                     ## BUT!!!! WHAT IF WE'RE COMPARING VALUES ON THESE PLOTS????
                     if (input$compare) {
                       if (input$comparison_type == "manual") {
                         comparison_vector <- input$manual_comparison_value
                       } else if (input$comparison_type == "plot_id") {
                         comparison_vector <- workspace$raw_data[[input$variable]][workspace$raw_data[[input$identifying_variable]] %in% input$comparison_plot_id]
                       }
                       
                       message("Adding comparison values to quantile plot:")
                       message(paste(round(comparison_vector,
                                           digits = 1),
                                     collapse = ", "))
                       
                       workspace$quantile_plot <- workspace$quantile_plot +
                         geom_hline(aes(yintercept = comparison_vector),
                                    linetype = "dashed",
                                    size = 1,
                                    color = "black")
                     }
                     
                     message("Rendering quantiles plot")
                     output$quantiles_plot <- renderPlot(workspace$quantile_plot)
                     
                     message("Saving quantiles plot")
                     ggsave(filename = paste0(workspace$temp_directory, "/fig1_quantile_plot.png"),
                            plot = workspace$quantile_plot,
                            device = "png",
                            width = 9,
                            height = 4,
                            units = "in")
                   }
                   
                   
                   # Create the caption for the quantile plot
                   quantile_plot_caption <- paste0("Figure 1: The distribution of values for the indicator across ", nrow(plotting_data), " data points, broken into ", length(quantiles) + 1, " quantiles. ",
                                                   paste0(paste0(paste0(names(quantiles), " of data points have a value <= "),
                                                                 round(quantiles, digits = 1),
                                                                 collapse = ", "),
                                                          ", and 100% of data points have a value <= ", round(max(current_data_vector, na.rm = TRUE), digits = 1)))
                   
                   # Okay, but we also need the captions to reflect the comparison values if they were requested!
                   if (input$compare) {
                     if (length(comparison_vector) == 1) {
                       comparison_caption_text <- paste0("The black dashed line marks the value ",
                                                         round(comparison_vector,
                                                               digits = 1),
                                                         ".")
                     } else if (length(comparison_vector) > 1) {
                       comparison_vector_string <- stringr::str_replace(paste(round(comparison_vector,
                                                                                    digits = 1),
                                                                              collapse = ", "),
                                                                        pattern = ", (?=\\d{1,100}\\.\\d{0,1}$)",
                                                                        replacement = ", and ")
                       comparison_caption_text <- paste0("The black dashed lines mark the values ",
                                                         comparison_vector_string,
                                                         ".")
                     } else {
                       comparison_caption_text <- NULL
                     }
                     
                     if (!is.null(comparison_caption_text)) {
                       quantile_plot_caption <- paste(quantile_plot_caption,
                                                      comparison_caption_text)
                     }
                   } else {
                     comparison_caption_text <- NULL
                   }
                   
                   message(paste0("Quantiles plot caption is: ",
                                  quantile_plot_caption))
                   
                   output$quantile_caption <- renderText(quantile_plot_caption)
                   
                   ##### Do the benchmarking ####
                   if (input$use_benchmarks) {
                     print("Benchmark time!")
                     # Now time to deal with benchmarks
                     # Make a data frame with all the benchmark ranges in it
                     benchmark_table <- data.frame(range_limit_lower = c(input$benchmark_range_limit_lower_1,
                                                                         input$benchmark_range_limit_lower_2,
                                                                         input$benchmark_range_limit_lower_3,
                                                                         input$benchmark_range_limit_lower_4,
                                                                         input$benchmark_range_limit_lower_5,
                                                                         input$benchmark_range_limit_lower_6),
                                                   range_relation_lower = c(input$benchmark_relationship_lower_1,
                                                                            input$benchmark_relationship_lower_2,
                                                                            input$benchmark_relationship_lower_3,
                                                                            input$benchmark_relationship_lower_4,
                                                                            input$benchmark_relationship_lower_5,
                                                                            input$benchmark_relationship_lower_6),
                                                   range_relation_upper = c(input$benchmark_relationship_upper_1,
                                                                            input$benchmark_relationship_upper_2,
                                                                            input$benchmark_relationship_upper_3,
                                                                            input$benchmark_relationship_upper_4,
                                                                            input$benchmark_relationship_upper_5,
                                                                            input$benchmark_relationship_upper_6),
                                                   range_limit_upper = c(input$benchmark_range_limit_upper_1,
                                                                         input$benchmark_range_limit_upper_2,
                                                                         input$benchmark_range_limit_upper_3,
                                                                         input$benchmark_range_limit_upper_4,
                                                                         input$benchmark_range_limit_upper_5,
                                                                         input$benchmark_range_limit_upper_6),
                                                   benchmark_category = c(input$benchmark_category_1,
                                                                          input$benchmark_category_2,
                                                                          input$benchmark_category_3,
                                                                          input$benchmark_category_4,
                                                                          input$benchmark_category_5,
                                                                          input$benchmark_category_6))
                     
                     # Limit that to just the first however many ranges the user has chosen to define for now
                     # This means that even if they populated more ranges then reduced the count they'll still
                     # only get the ones they can see in the app
                     benchmark_table <- benchmark_table[1:input$range_count, ]
                     # Also strip out undefined ranges
                     invalid_benchmark_indices <- benchmark_table$benchmark_category == ""
                     benchmark_table <- benchmark_table[!invalid_benchmark_indices, ]
                     
                     print(benchmark_table)
                     # First create the string vector we'll write into for each category
                     benchmark_results <- rep("Undefined",
                                              times = nrow(plotting_data))
                     # Then step through the ranges in a loop
                     # We loop because the user might've accidentally defined it so that ranges overlap
                     for (benchmark in 1:nrow(benchmark_table)) {
                       current_range_string_lower <- paste(benchmark_table$range_limit_lower[benchmark],
                                                           benchmark_table$range_relation_lower[benchmark])
                       current_range_string_upper <- paste(benchmark_table$range_relation_upper[benchmark],
                                                           benchmark_table$range_limit_upper[benchmark])
                       current_benchmark_category <- benchmark_table$benchmark_category[benchmark]
                       
                       # Convert the range inequalities into evaluatable statements by addint in the values
                       evaluation_strings_lower <- sapply(X = plotting_data$current_variable,
                                                          range_string = current_range_string_lower,
                                                          FUN = function(X, range_string){
                                                            paste(range_string, X)
                                                          })
                       evaluation_strings_upper <- sapply(X = plotting_data$current_variable,
                                                          range_string = current_range_string_upper,
                                                          FUN = function(X, range_string){
                                                            paste(X, range_string)
                                                          })
                       # Then parse and evaluate those strings
                       # This is the easiest way I've been able to come up with for evaluating benchmarks
                       # that are entered piecemeal and with a dropdown
                       lower_strings_evaluated <- sapply(X = evaluation_strings_lower,
                                                         FUN = function(X){
                                                           eval(parse(text = X))
                                                         })
                       upper_strings_evaluated <- sapply(X = evaluation_strings_upper,
                                                         FUN = function(X){
                                                           eval(parse(text = X))
                                                         })
                       
                       # Write in the benchmark category into our benchmark_results at the appropriate indices
                       correct_indices <- lower_strings_evaluated & upper_strings_evaluated
                       benchmark_results[correct_indices] <- current_benchmark_category
                     }
                     
                     
                     # Put the results into the data
                     plotting_data[["benchmark_results"]] <- benchmark_results
                     
                     # Calculate the percent of points meeting and not meeting
                     benchmark_results_summary <- table(benchmark_results)
                     
                     message("Benchmark results summary:")
                     print(benchmark_results_summary)
                     
                     missing_categories <- benchmark_table$benchmark_category[!(benchmark_table$benchmark_category %in% names(benchmark_results_summary))]
                     if (length(missing_categories) > 0) {
                       for (missing_category in missing_categories) {
                         benchmark_results_summary[[missing_category]] <- 0
                       }
                     }
                     
                     percent_by_category <- round(100 * benchmark_results_summary / sum(benchmark_results_summary),
                                                  digits = 1)
                     
                     # Plot the histogram with benchmark info!
                     workspace$benchmark_plot <- ggplot() +
                       geom_histogram(data = plotting_data,
                                      aes(y = current_variable,
                                          fill = benchmark_results),
                                      binwidth = 1) +
                       scale_fill_manual(values = workspace$palette) +
                       scale_y_continuous(expand = c(0, 0)) +
                       scale_x_continuous(expand = c(0, 0)) +
                       labs(x = "Count of data points",
                            y = input$variable_name,
                            fill = "Benchmark status") +
                       theme(panel.grid = element_blank(),
                             panel.background = element_rect(fill = "gray95")) +
                       coord_flip()
                     
                     ## BUT!!!! WHAT IF WE'RE COMPARING VALUES ON THESE PLOTS????
                     if (input$compare) {
                       if (input$comparison_type == "manual") {
                         comparison_vector <- input$manual_comparison_value
                       } else if (input$comparison_type == "plot_id") {
                         comparison_vector <- workspace$raw_data[[input$variable]][workspace$raw_data[[input$identifying_variable]] %in% input$comparison_plot_id]
                       }
                       
                       workspace$benchmark_plot <- workspace$benchmark_plot +
                         geom_hline(aes(yintercept = comparison_vector),
                                    linetype = "dashed",
                                    size = 1,
                                    color = "black")
                     }
                     
                     output$benchmark_plot <- renderPlot(workspace$benchmark_plot)
                     
                     
                     ggsave(filename = paste0(workspace$temp_directory, "/fig2_benchmark_plot.png"),
                            plot = workspace$benchmark_plot,
                            device = "png",
                            width = 9,
                            height = 4,
                            units = "in")
                     
                     # Make the statements about each condition category for the caption
                     category_statements <- paste0(benchmark_results_summary, " data points (", percent_by_category, "%) ",
                                                   "fall in the benchmark category ", names(benchmark_results_summary),
                                                   collapse = ", ")
                     
                     # Create the caption for the benchmark plot
                     benchmark_plot_caption <- paste0("Figure 2: The distribution of values for the indicator across ", sum(benchmark_results_summary),
                                                      " data points classified into benchmark categories. ",
                                                      "Of the ", sum(benchmark_results_summary), " data points, ",
                                                      category_statements, ".")
                     
                     if (!is.null(comparison_caption_text)) {
                       benchmark_plot_caption <- paste(benchmark_plot_caption,
                                                       comparison_caption_text)
                     }
                     
                     output$benchmark_summary <- renderText(benchmark_plot_caption)
                     
                     output_data <- plotting_data[, c(input$id_variables,
                                                      input$variable,
                                                      "benchmark_results")]
                     
                     # Create a .zip fle in case user wants the plots, which depends on a system call
                     # I'm not sure why we switch working directories for this, but I'm afraid to change it
                     message(paste0("Current temp directory is ", workspace$temp_directory))
                     setwd(workspace$temp_directory)
                     
                     # Write out the benchmarked data for download
                     write.csv(output_data,
                               file = "benchmarked_data.csv",
                               row.names = FALSE)
                   } else {
                     message("No benchmarking!")
                     message("Attempting to delete fig2_benchmark_plot.png")
                     if (file.exists(paste0(workspace$temp_directory, "/fig2_benchmark_plot.png"))) {
                       file.remove(paste0(workspace$temp_directory, "/fig2_benchmark_plot.png"))
                     }
                     if (file.exists(paste0(workspace$temp_directory, "/fig2_benchmark_plot.png"))) {
                       message("fig2_benchmark_plot.png does exists. :(")
                     } else {
                       message("fig2_benchmark_plot.png does not exist. :D")
                     }
                     message("Attempting to delete benchmarked_data.csv")
                     if (file.exists(paste0(workspace$temp_directory, "/benchmarked_data.csv"))) {
                       file.remove(paste0(workspace$temp_directory, "/benchmarked_data.csv"))
                     }
                     if (file.exists(paste0(workspace$temp_directory, "/benchmarked_data.csv"))) {
                       message("benchmarked_data.csv does exist :(")
                     } else {
                       message("benchmarked_data.csv does not exist :D")
                     }
                     
                     output$benchmark_plot <- NULL
                     benchmark_plot_caption <- NULL
                     output$benchmark_summary <- NULL
                   }
                   
                   ## Do the time series plot!
                   if (input$time_series) {
                     message("Plotting time series!")
                     timeseries_plotting_data <- workspace$raw_data
                     timeseries_plotting_data$value <- timeseries_plotting_data[[input$variable]]
                     
                     if (input$date_type == "iso8601") {
                       year_vector <- stringr::str_extract(timeseries_plotting_data[[input$date_variable]],
                                                           pattern = "^\\d{4}")
                       if (any(is.na(year_vector))) {
                         showNotification(ui = "Unable to extract years from the date variable for some or all observations. Please make sure that the date is formatted as YYYY-MM-DD in accordance with the ISO8601 standard, e.g., '2020-10-30', and that all data have dates.",
                                          duration = NULL,
                                          closeButton = TRUE,
                                          type = "warning",
                                          id = "year_error_iso8601")
                       }
                       
                       timeseries_plotting_data$year <- year_vector
                       
                     } else if (input$date_type == "month_day_year") {
                       year_vector <- stringr::str_extract(timeseries_plotting_data[[input$date_variable]],
                                                           pattern = "\\d{4}$")
                       if (any(is.na(year_vector))) {
                         showNotification(ui = "Unable to extract years from the date variable for some or all observations. Please make sure that the date is formatted as 'Month Day, Year', e.g., 'October 30, 2020', and that all data have dates.",
                                          duration = NULL,
                                          closeButton = TRUE,
                                          type = "warning",
                                          id = "year_error_monthdayyear")
                       }
                       
                       timeseries_plotting_data$year <- year_vector
                       
                     } else {
                       # If the variable is just years, roll with it
                       timeseries_plotting_data$year <- timeseries_plotting_data[[input$date_variable]]
                     }
                     
                     # MAKE SURE TO PUT YEARS IN ASCENDING ORDER WITH "ALL YEARS" AT THE END
                     years_vector_numeric <- as.numeric(unique(timeseries_plotting_data$year))
                     years_vector_numeric <- years_vector_numeric[order(years_vector_numeric,
                                                                        decreasing = FALSE)]
                     years_levels <- c(years_vector_numeric, "All years")
                     
                     # Bind on the data again, but with "All years" in the year variable for all observations
                     timeseries_duplicate_data <- timeseries_plotting_data
                     timeseries_duplicate_data$year <- "All years"
                     timeseries_plotting_data <- rbind(timeseries_plotting_data,
                                                       timeseries_duplicate_data)
                     
                     # Use the levels vector to set the factor order
                     timeseries_plotting_data$year <- factor(timeseries_plotting_data$year,
                                                             levels = years_levels)
                     
                     # Plot the figure
                     workspace$timeseries_plot <- ggplot() +
                       geom_boxplot(data = timeseries_plotting_data,
                                    aes(x = year,
                                        y = value)) +
                       labs(y = input$variable_name,
                            x = "Year") +
                       theme(panel.grid.minor.x = element_blank(),
                             panel.grid.major.x = element_blank(),
                             panel.background = element_rect(fill = "gray95"))
                     
                     # Render the figure
                     output$timeseries_plot <- renderPlot(workspace$timeseries_plot)
                     
                     # Write out the figure
                     ggsave(filename = paste0(workspace$temp_directory, "/fig3_timeseries_plot.png"),
                            plot = workspace$timeseries_plot,
                            device = "png",
                            width = 9,
                            height = 4,
                            units = "in")
                     
                     # Make the caption
                     timeseries_plot_caption <- "Figure 3: Distribution of indicator values by year."
                     output$timeseries_summary <- renderText(timeseries_plot_caption)
                     
                   } else {
                     message("No time series!")
                     message("Attempting to delete fig3_timeseries_plot.png")
                     if (file.exists(paste0(workspace$temp_directory, "/fig3_timeseries_plot.png"))) {
                       file.remove(paste0(workspace$temp_directory, "/fig3_timeseries_plot.png"))
                     }
                     if (file.exists(paste0(workspace$temp_directory, "/fig3_timeseries_plot.png"))) {
                       message("fig3_timeseries_plot.png does exists. :(")
                     } else {
                       message("fig3_timeseries_plot.png does not exist. :D")
                     }
                     
                     output$timeseries_plot <- NULL
                     timeseries_plot_caption <- NULL
                     output$timeseries_summary <- NULL
                   }
                   
                   
                   updateTabsetPanel(session,
                                     inputId = "maintabs",
                                     selected = "Results") 
                   
                   message(paste0("Setting work directory to ",
                                  workspace$temp_directory))
                   setwd(workspace$temp_directory)
                   
                   # Write out the captions as a text file for downloading
                   writeLines(text = paste(quantile_plot_caption,
                                           benchmark_plot_caption,
                                           timeseries_plot_caption,
                                           sep = "\n\n"),
                              con = "captions.txt")
                   
                   files_to_zip <- list.files(path = workspace$temp_directory,
                                              pattern = "\\.(png|txt|csv)$",
                                              ignore.case = TRUE,
                                              full.names = TRUE)
                   
                   message("Preparing to zip up the following files:")
                   message(paste0(files_to_zip,
                                  collapse = ", "))
                   
                   
                   switch(Sys.info()[["sysname"]],
                          Windows = {
                            system(paste0("cmd.exe /c \"C:\\Program Files\\7-Zip\\7z\".exe a -tzip plots.zip ",
                                          paste(files_to_zip,
                                                collapse = " ")))
                          },
                          Linux = {
                            system(paste("zip plots %s",
                                         paste(files_to_zip,
                                               collapse = " ")))
                          })
                   if (!any(grepl(x = list.files(workspace$temp_directory), pattern = "^plots\\.(zip)|(ZIP)"))) {
                     stop("No valid .zip file called 'plots' exists in the directory.")
                   } else {
                     message("plots.zip does exist in the temp directory")
                   }
                   setwd(workspace$original_directory)
                 }
                 
                 removeNotification(id = "plotting")
               })
  
  ##### Download handler for the .zip file created with plots ####
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("plots_",
             paste0(format(Sys.Date(), "%Y-%m-%d"), "_",
                    format(Sys.time(), "%H%M", tz = "GMT")),
             ".zip")
    },
    content = function(file) {
      file.copy(paste0(workspace$temp_directory, "/plots.zip"), file)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
