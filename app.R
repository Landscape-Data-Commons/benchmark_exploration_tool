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
  
  # Application title
  titlePanel("Plotting Indicator Values and Benchmarks"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "raw_data",
                label = "Exported TerrADat CSV",
                multiple = FALSE,
                accept = "CSV"),
      helpText("OR"),
      textInput(inputId = "ecosite_id",
                label = "Ecological Site ID",
                value = "",
                placeholder = "R042XB012NM"),
      textOutput(outputId = "ecosite_error"),
      actionButton(inputId = "fetch_data",
                   label = "Fetch data from the Landscape Data Commons"),
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
      fluidRow(column(width = 5,
                      selectInput(inputId = "benchmark_relationship",
                                  label = "Benchmark relationship",
                                  choices = c("<", "<=", ">", ">="),
                                  selected = ">=")),
               column(width = 5,
                      numericInput(inputId = "benchmark_value",
                                   label = "Benchmark cutoff value",
                                   value = 0,
                                   min = 0))),
      hr(),
      # Only show the plot button if data have been uploaded/downloaded
      conditionalPanel(condition = "input.variable != ''",
                       actionButton(inputId = "plot_button",
                                    label = "Plot indicator distribution!")),
      # Only show if there are plot images to download
      conditionalPanel(condition = "input.plot_button >= 1",
                       downloadButton(outputId = 'downloadData',
                                      label = 'Download results'))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "maintabs",
                  
                  tabPanel(title = "Instructions",
                           includeHTML("instructions.html")),
                  
                  tabPanel(title = "Results",
                           plotOutput("quantiles_plot"),
                           # plotlyOutput("quantiles_plot"),
                           textOutput("quantile_breaks"),
                           plotOutput("benchmark_plot"),
                           # plotlyOutput("benchmark_plot"),
                           textOutput("benchmark_summary")),
                  tabPanel(title = "Data",
                           tableOutput("data_table")))
      
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
                 output$ecosite_error <- renderText("")
                 # Only do anything if there's an ecosite ID
                 if (input$ecosite_id != "") {
                   # Make sure it's uppercase
                   ecosite_id <- toupper(input$ecosite_id)
                   
                   # Build the query
                   query <- paste0("http://api.landscapedatacommons.org/api/",
                                   "geoindicators?",
                                   "EcologicalSiteId=",
                                   ecosite_id)

                   # Getting the data via curl
                   # connection <- curl::curl(query)
                   # results_raw <- readLines(connection)
                   # results <- jsonlite::fromJSON(results_raw)
                   print("Attempting to query EDIT")
                   # Full query results for geoindicators based on ecosite
                   full_results <- httr::GET(query,
                                             config = httr::timeout(30))
                   # Grab only the data portion
                   results_raw <- full_results[["content"]]
                   # Convert from raw to character
                   results_character <- rawToChar(results_raw)
                   # Convert from character to data frame
                   results <- jsonlite::fromJSON(results_character)
                   
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
                     output$ecosite_error <- renderText(paste(ecosite_id, "is not a valid ecological site ID recognized by EDIT"))
                   }
                 }
               })
  
  #### When a CSV is read in, do this ####
  observeEvent(eventExpr = workspace$raw_data,
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
                 
                 # Update the dropdown options to include those variables
                 updateSelectInput(inputId = "id_variables",
                                   choices = c("", variable_names))
                 updateSelectInput(inputId = "variable",
                                   choices = c("", variable_names[viable_variables]))
                 
                 output$data_table <- renderTable(workspace$raw_data)
                 
                 updateTabsetPanel(session,
                                   inputId = "maintabs",
                                   selected = "Data")
               })
  
  #### When the indicator is updated, do this ####
  observeEvent(eventExpr = input$variable,
               handlerExpr = {
                 updateTextInput(session,
                                 inputId = "variable_name",
                                 value = input$variable)
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
                 updateSelectInput(inputId = "variable",
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
                 
                 # IF ANY AREN'T NUMERIC, WE HAVE A PROBLEM
                 if (any(is.na(quantiles_vector))) {
                   # INSERT ERROR MESSAGE HERE
                   # Default to just 50%
                   workspace[["quantiles"]] <- c(0.5)
                 } else {
                   # Convert to proportion for quantile()
                   quantiles_vector <- quantiles_vector / 100
                   
                   # If any of the percentages were < 0 or > 100, there's a problem
                   if (all(quantiles_vector >= 0) & all(quantiles_vector <= 1)) {
                     workspace[["quantiles"]] <- quantiles_vector
                   } else {
                     # INSERT ERROR MESSAGE HERE
                     # Default to just 50%
                     workspace[["quantiles"]] <- c(0.5)
                   }
                 }
                 
               })
  
  #### When the plot button is hit, do this ####
  observeEvent(eventExpr = input$plot_button,
               handlerExpr = {
                 # Get a copy of the data to manipulate for plotting
                 plotting_data <- workspace$raw_data
                 
                 message("plotting_data row one is:")
                 message(plotting_data[1,])
                 
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
                     workspace$quantile_plot <- ggplot(data = plotting_data) +
                       geom_histogram(aes(y = current_variable,
                                          fill = Quantile),
                                      binwidth = 1) +
                       scale_fill_manual(values = workspace$palette) +
                       geom_hline(yintercept = quantiles,
                                  size = 1.5,
                                  color = "gray50") +
                       labs(x = "Count of plots",
                            y = input$variable_name) +
                       theme(panel.grid = element_blank(),
                             panel.background = element_rect(fill = "gray95")) +
                       coord_flip()
                     
                     output$quantiles_plot <- renderPlot(workspace$quantile_plot)
                     # output$quantiles_plot <- renderPlotly(ggplotly(workspace$quantile_plot))
                     
                     ggsave(filename = paste0(workspace$temp_directory, "/fig1_quantile_plot.png"),
                            plot = workspace$quantile_plot,
                            device = "png",
                            width = 9,
                            height = 4,
                            units = "in")
                   }
                   
                   # Now time to do this with the theoretical benchmark
                   relationship <- input$benchmark_relationship
                   benchmark_value <- input$benchmark_value
                   benchmark_string <- paste(relationship, benchmark_value)
                   
                   # We're going to make strings with the inequalities to evaluate
                   evaluation_strings <- paste0(plotting_data$current_variable, benchmark_string)
                   # Then parse and evaluate those strings
                   # This is the easiest way I've been able to come up with for evaluating benchmarks
                   # that are entered piecemeal and with a dropdown
                   strings_evaluated <- sapply(X = evaluation_strings,
                                               FUN = function(X){
                                                 eval(parse(text = X))
                                               })
                   # Assume nothing is meeting!!!
                   benchmark_results <- rep("Not Meeting",
                                            times = length(strings_evaluated))
                   # Replace "Not meeting" with "Meeting" where they passed the benchmarking
                   benchmark_results[strings_evaluated] <- "Meeting"
                   plotting_data[["benchmark_results"]] <- benchmark_results
                   
                   # Calculate the percent of points meeting and not meeting
                   benchmark_results_summary <- table(benchmark_results)
                   percent_meeting <- round(100 * benchmark_results_summary["Meeting"] / sum(benchmark_results_summary),
                                            digits = 1)
                   percent_not_meeting = round(100 - percent_meeting,
                                               digits = 1)
                   
                   # In case all the plots fell into one category
                   meeting_count <- benchmark_results_summary["Meeting"]
                   if (is.null(meeting_count) | is.na(meeting_count)) {
                     meeting_count <- 0
                   }
                   not_meeting_count <- benchmark_results_summary["Not Meeting"]
                   if (is.null(not_meeting_count) | is.na(not_meeting_count)) {
                     not_meeting_count <- 0
                   }
                   
                   # Plot the histogram with benchmark info!
                   workspace$benchmark_plot <- ggplot(data = plotting_data) +
                     geom_histogram(aes(y = current_variable,
                                        fill = benchmark_results),
                                    binwidth = 1) +
                     scale_fill_manual(values = workspace$palette) +
                     geom_hline(yintercept = benchmark_value,
                                size = 1.5,
                                color = "gray50") +
                     labs(x = "Count of plots",
                          y = input$variable_name,
                          fill = "Benchmark status") +
                     theme(panel.grid = element_blank(),
                           panel.background = element_rect(fill = "gray95")) +
                     coord_flip()
                   
                   output$benchmark_plot <- renderPlot(workspace$benchmark_plot)
                   # output$benchmark_plot <- renderPlotly(plotly::ggplotly(workspace$benchmark_plot))
                   
                   ggsave(filename = paste0(workspace$temp_directory, "/fig2_benchmark_plot.png"),
                          plot = workspace$benchmark_plot,
                          device = "png",
                          width = 9,
                          height = 4,
                          units = "in")
                   
                   updateTabsetPanel(session,
                                     inputId = "maintabs",
                                     selected = "Results") 
                   
                   # Create the caption for the quantile plot
                   quantile_plot_caption <- paste0("Figure 1: The distribution of values for the indicator across ", sum(benchmark_results_summary), " plots, broken into ", length(quantiles) + 1, " quantiles. ",
                                                   paste0(paste0(paste0(names(quantiles), " of plots have a value <= "),
                                                                 round(quantiles, digits = 1),
                                                                 collapse = ", "),
                                                          ", and 100% of plots have a value <= ", round(max(current_data_vector, na.rm = TRUE), digits = 1)))
                   
                   output$quantile_breaks <- renderText(quantile_plot_caption)
                   
                   # Create the caption for the benchmark plot
                   benchmark_plot_caption <- paste0("Figure 2: The distribution of values for the indicator across ", sum(benchmark_results_summary),
                                                    " plots classified as meeting or not meeting the benchmark of ", benchmark_string, ". ",
                                                    "Of the ", sum(benchmark_results_summary), " plots, ", percent_meeting, "% (", meeting_count, " plots) met the benchmark and ",
                                                    percent_not_meeting, "% (", not_meeting_count, " plots) did not meet the benchmark.")
                   
                   output$benchmark_summary <- renderText(benchmark_plot_caption)
                   
                   output_data <- plotting_data[, c(input$id_variables,
                                                    input$variable,
                                                    "benchmark_results")]
                   
                   # Create a .zip fle in case user wants the plots, which depends on a system call
                   # I'm not sure why we switch working directories for this, but I'm afraid to change it
                   setwd(workspace$temp_directory)
                   
                   # Write out the benchmarked data for download
                   write.csv(output_data,
                             file = "benchmarked_data.csv",
                             row.names = FALSE)
                   
                   # Write out the captions as a text file for downloading
                   writeLines(text = paste(quantile_plot_caption,
                                           benchmark_plot_caption,
                                           sep = "\n\n"),
                              con = "captions.txt")
                   
                   files_to_zip <- list.files(pattern = "\\.(png|txt|csv)$",
                                              ignore.case = TRUE)
                   
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
                     stop("No valid .zip file called 'results' exists in the directory.")
                   }
                   setwd(workspace$original_directory)
                 }
               })
  
  ##### Download handler for the .zip file created with plots ####
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("plots_",
             paste0(format(Sys.Date(), "%Y-%m-%d"),
                    format(Sys.time(), "T%H%MZ", tz = "GMT")),
             ".zip")
    },
    content = function(file) {
      file.copy(paste0(workspace$temp_directory, "/plots.zip"), file)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
