library(shiny)
library(ggplot2)
library(dplyr)
library(httr)
library(stringr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(shinyjs)
library(zip)
source("functions.R")

# Define UI
ui <- fluidPage(
  title = "Benchmark Exploration Tool",
  useShinyjs(),
  tags$head(
    # Style things nicely!
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # A function that lets us create links to tabs since there's no
    # equivalent to updateTabsetPanel() like updateTabPanel() for some reason.
    # This lets us make links with a(onclick = "tabJump('Tab Name')")
    # This comes from StackOverflow, I think?
    tags$script(HTML('
        var tabJump = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      '))
  ),
  
  navbarPage(title = tags$div(class = "tool-title",
                              "Benchmark Exploration Tool"),
             id = "navbar-full",
             position = "static-top",
             footer = tags$div(class = "footer",
                               p(column(width = 4,
                                        p(a(href = 'mailto:nelson.stauffer@usda.gov',
                                            'Contact us with questions',
                                            target = "_blank"))),
                                 column(width = 8,
                                        align = "right",
                                        p(img(src = "combined_logos_hires.png",
                                              width = "95%"))))),
             
             tabPanel(title = "Start",
                      sidebarLayout(
                        sidebarPanel(
                          HTML("<div class = 'app-info'>
                                <h3>About</h3>
                                This application visualizes the distribution of values for ecological indicators to help determine value ranges for categorical benchmarks or to see the consequences of applying categorical benchmarks.
                                <br>
                                </div>"
                          ),
                          fluidRow(column(width = 10,
                                          radioButtons(inputId = "data_source",
                                                       label = "Data source",
                                                       choices = c("Query the Landscape Data Commons" = "ldc",
                                                                   "Upload tabular data file" = "upload"),
                                                       selected = character(0))),
                                   column(width = 1,
                                          actionButton(inputId = "data_source_info",
                                                       label = "",
                                                       class = "info-btn",
                                                       icon = icon("circle-question")))),
                          conditionalPanel(condition = "input.data_source == 'upload'",
                                           fluidRow(column(width = 10,
                                                           fileInput(inputId = "raw_data",
                                                                     label = "Data CSV",
                                                                     multiple = FALSE,
                                                                     accept = "CSV")),
                                                    column(width = 1))
                          ),
                          # If the data are going to come from the LDC, display options for querying
                          fluidRow(column(width = 10,
                                          uiOutput("query_method_ui")),
                                   column(width = 1,
                                          uiOutput("query_method_info_ui"))),
                          # Should the polygons be uploaded or drawn?
                          fluidRow(column(width = 10,
                                          uiOutput("polygon_source_ui"))),
                          # If the query will be key-based, show the text box input for keys
                          fluidRow(column(width = 10,
                                          uiOutput("keys_input_ui")),
                                   column(width = 1,
                                          uiOutput("keys_input_info_ui"))),
                          # If the query will be spatial, show the upload bar
                          fluidRow(column(width = 10,
                                          uiOutput("spatial_input_ui")),
                                   column(width = 1,
                                          uiOutput("spatial_input_info_ui"))),
                          # If there's an uploaded polygon file, show the options to select a feature
                          fluidRow(column(width = 10,
                                          uiOutput("select_polygon_ui")),
                                   column(width = 1,
                                          uiOutput("select_polygon_info_ui"))),
                          fluidRow(column(width = 11,
                                          uiOutput("polygon_draw_prompt"))),
                          # If there's an uploaded polygon file, show the option to repair the polygons
                          fluidRow(column(width = 10,
                                          uiOutput("repair_polygons_ui")),
                                   column(width = 1,
                                          uiOutput("repair_polygons_info_ui"))),
                          # If querying the LDC and the query criteria are selected, show
                          # the fetch button
                          # There are three because I have to render them separately depending
                          # on different conditions and I'm not allowed to have multiple
                          # situations render to the same output name. The UI elements
                          # both contain identical fetch buttons though since they can
                          # never coexist.
                          HTML("<center>"),
                          uiOutput("fetch_ui1"),
                          HTML("</center>"),
                          HTML("<center>"),
                          uiOutput("fetch_ui2"),
                          HTML("</center>"),
                          HTML("<center>"),
                          uiOutput("fetch_ui3"),
                          HTML("</center>"),
                          uiOutput("data_available_ui"),
                          conditionalPanel(
                            condition = "$('html').hasClass('shiny-busy')",
                            br(),
                            HTML(
                              "<div class = 'load-message'><img src = 'busy_icon_complex.svg' height = '40rem'>Working! Please wait.<img src = 'busy_icon_complex.svg' height = '40rem' transform = 'scaleX(-1)'></div>"
                            )
                          ),
                          br(),
                          HTML("<center>"),
                          actionButton(inputId = "reset_button",
                                       label = "Reset this tool"),
                          HTML("</center>"),
                        ),
                        mainPanel(
                          tags$div(class = "main-panel-body",
                                   uiOutput("drawing_map_ui"),
                                   uiOutput("main_map_ui")#,
                                   # includeHTML("instructions.html")
                          )
                        )
                      )
             ),
             tabPanel(title = "Data",
                      mainPanel(width = 11,
                                uiOutput(outputId = "data_tab_notyet_ui"),
                                tags$div(class = "data-table",
                                         fluidRow(
                                           column(width = 12,
                                                  DT::DTOutput("data_table"))
                                         )))),
             tabPanel(title = "Benchmarks",
                      sidebarLayout(sidebarPanel = sidebarPanel(
                        width = 8,
                        checkboxInput(inputId = "use_benchmarks",
                                      label = "Apply benchmarks",
                                      value = FALSE),
                        conditionalPanel(condition = "input.use_benchmarks",
                                         p(class = "data-prompt",
                                           "Once you've finished setting up your benchmark ranges, you can generate your plots in the",
                                           a("Figures tab.",
                                             onclick = "tabJump('Figures')")),
                                         fluidRow(column(width = 3,
                                                         selectInput(inputId = "range_count",
                                                                     label = "Number of benchmark ranges",
                                                                     choices = 2:6,
                                                                     selected = 2))),
                                         # Labels
                                         fluidRow(column(width = 2,
                                                         HTML("<div class = 'benchmark-ui-label'>
                                                              Lower limit
                                                              </div>")),
                                                  column(width = 1,
                                                         HTML("<div class = 'benchmark-ui-label'>
                                                              Lower limit relation
                                                              </div>")),
                                                  column(width = 1,
                                                         HTML("<div class = 'benchmark-ui-label'>
                                                              Indicator value
                                                              </div>")),
                                                  column(width = 1,
                                                         HTML("<div class = 'benchmark-ui-label'>
                                                              Upper limit relation
                                                              </div>")),
                                                  column(width = 2,
                                                         HTML("<div class = 'benchmark-ui-label'>
                                                              Upper limit
                                                              </div>")),
                                                  
                                                  column(width = 3,
                                                         HTML("<div class = 'benchmark-ui-label'>
                                                              Benchmark category
                                                              </div>")),
                                                  column(width = 1,
                                                         actionButton(inputId = "benchmark_ui_info",
                                                                      label = "",
                                                                      class = "info-btn",
                                                                      icon = icon("circle-question"))),
                                                  br()
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
                                                         HTML("<p style='text-align:center;'><br>X</p>")),
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
                                                  br()
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
                                                         HTML("<p style='text-align:center;'><br>X</p>")),
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
                                                  br()
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
                                                                          HTML("<p style='text-align:center;'><br>X</p>")),
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
                                                                   br()
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
                                                                          HTML("<p style='text-align:center;'><br>X</p>")),
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
                                                                   br()
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
                                                                          HTML("<p style='text-align:center;'><br>X</p>")),
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
                                                                   br()
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
                                                                          HTML("<p style='text-align:center;'><br>X</p>")),
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
                                                                   br()
                                                          ))
                        )
                      ),
                      mainPanel = mainPanel())),
             tabPanel(title = "Figures",
                      sidebarLayout(sidebarPanel = sidebarPanel(
                        selectInput(inputId = "id_variables",
                                    label = "Variable(s) with identifying, non-indicator information",
                                    multiple = TRUE,
                                    choices = c("")),
                        selectInput(inputId = "variable",
                                    label = "Indicator to plot",
                                    choices = c("")),
                        textInput(inputId = "variable_name",
                                  label = "Indicator label",
                                  value = ""),
                        numericInput(inputId = "value_min",
                                     label = "Minimum possible indicator value",
                                     value = 0),
                        numericInput(inputId = "value_max",
                                     label = "Maximum possible indicator value",
                                     value = 100),
                        radioButtons(inputId = "plot_type",
                                     label = "Plot type",
                                     choices = c("Box plot" = "boxplot",
                                                 "Histogram" = "histogram"),
                                     selected = "boxplot"),
                        conditionalPanel(condition = "input.plot_type == 'histogram'",
                                         textInput(inputId = "quantiles",
                                                   label = "Quantile break percentages, separated by commas",
                                                   value = "25, 50, 75")),
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
                                                                      # label = "Comparison unique ID(s)",
                                                                      # multiple = TRUE,
                                                                      label = "Comparison unique ID",
                                                                      multiple = FALSE,
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
                        # Only show the plot button if data have been uploaded/downloaded
                        conditionalPanel(condition = "input.variable != ''",
                                         br(),
                                         actionButton(inputId = "plot_button",
                                                      label = "Plot indicator distribution!")),
                        conditionalPanel(
                          condition = "$('html').hasClass('shiny-busy')",
                          br(),
                          HTML(
                            "<div class = 'load-message'><img src = 'busy_icon_complex.svg' height = '40rem'>Working! Please wait.<img src = 'busy_icon_complex.svg' height = '40rem' transform = 'scaleX(-1)'></div>"
                          )
                        ),
                        # Only show if there are plot images to download
                        conditionalPanel(condition = "input.plot_button >= 1",
                                         br(),
                                         downloadButton(outputId = 'downloadData',
                                                        label = 'Download results'))
                      ),
                      mainPanel = mainPanel(
                        tabPanel(title = "Figures",
                                 tags$div(class = "main-panel-body",
                                          plotOutput("quantiles_plot"),
                                          textOutput("quantile_caption"),
                                          plotOutput("benchmark_plot"),
                                          textOutput("benchmark_summary"),
                                          plotOutput("timeseries_plot"),
                                          textOutput("timeseries_summary")
                                 )
                        )
                      )))
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ##### Intialization #####
  # Allow for wonking big files
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  # This is dangerous, but I'm doing it anyway so that polygons work consistently
  sf::sf_use_s2(FALSE)
  
  # Create a workspace list to store objects and values in
  workspace <- reactiveValues(placeholder = TRUE,
                              temp_directory = tempdir(),
                              original_directory = getwd(),
                              quantiles = c(0.25, 0.5, 0.75),
                              mapping_header_sf = NULL,
                              mapping_polygons = NULL,
                              header_sf = NULL,
                              headers = NULL,
                              raw_data = NULL,
                              # The color palette for the figures
                              # palette = c("#f5bb57ff",
                              #             "#f8674cff",
                              #             "#4a8b9fff",
                              #             "#685b7fff",
                              #             "#c95294ff",
                              #             "#f5a9c6ff",
                              #             "#75c6c5ff",
                              #             "#fd6794ff"))
                              # # The IBM colorblind-friendly palette
                              # palette = c("#648fffff",
                              #             "#785ef0ff",
                              #             "#dc267fff",
                              #             "#fe6100ff",
                              #             "#ffb000ff"))
                              # The Tol colorblind-friendly palette
                              palette = c("#332288ff",
                                          "#117733ff",
                                          "#44AA99ff",
                                          "#88CCEEff",
                                          "#DDCC77ff",
                                          "#CC6677ff",
                                          "#AA4499ff",
                                          "#882255ff"))
  
  # Nor are there plots
  output$plot_files <- renderText("FALSE")
  
  
  ##### Conditional UI elements #####
  ###### Start Sidebar ######
  # Query method for when grabbing data from the LDC
  output$query_method_ui <- renderUI(expr = if (req(input$data_source) == "ldc") {
    message("data_source is 'ldc'. Rendering query_method UI element.")
    selectInput(inputId = "query_method",
                label = "Query method",
                choices = c("Spatial" = "spatial",
                            "By ecological site" = "EcologicalSiteID",
                            "By PrimaryKey" = "PrimaryKey",
                            "By ProjectKey" = "ProjectKey"),
                selected = "spatial")
  })
  output$query_method_info_ui <- renderUI(expr = if (req(input$data_source) == "ldc") {
    message("data_source is 'ldc'. Rendering query_method_info UI element.")
    actionButton(inputId = "query_method_info",
                 label = "",
                 class = "info-btn",
                 icon = icon("circle-question"))
  })
  
  output$polygon_source_ui <- renderUI(expr = if (req(input$query_method) == "spatial" & req(input$data_source) == "ldc") {
    radioButtons(inputId = "polygon_source",
                 label = "Polygon source",
                 choices = c("Uploaded" = "upload",
                             "Drawn" = "draw"),
                 inline = TRUE)
  })
  
  output$polygon_draw_prompt <- renderUI(expr = if(req(input$data_source == "ldc") & req(input$polygon_source == "draw")) {
      fluidRow(column(width = 1,
                      img(src = "polygon_tool_icons.png",
                          height = "60px",
                          display = "inline",
                          align = "left",
                          hspace = "5px",
                          vspace = "5px")),
               column(width = 9,
                      HTML(text = "Please use the buttons found on the left side of the map to draw your polygon boundary.")
                      ))
  })
  
  # Add a fetch button when grabbing data from the LDC and the query criteria
  # are available
  # Apparently since the tool will never have input$keys and input$polygons_layer
  # at the same time, I can't capture them both in a single conditional, but I
  # can do it in two separate ones rendering an identical element because I know
  # they'll never come into conflict
  output$fetch_ui1 <- renderUI(expr = if (req(input$query_method) %in% c("EcologicalSiteID", "PrimaryKey", "ProjectKey") & req(input$keys) != "") {
    tagList(br(),
            actionButton(inputId = "fetch_data",
                         label = "Fetch data"))
  })
  output$fetch_ui2 <- renderUI(expr = if (req(input$query_method) == "spatial" & (req(input$polygon_source) == "upload" & req(input$polygons_layer) != "")) {
    tagList(br(),
            actionButton(inputId = "fetch_data",
                         label = "Fetch data"))
  })
  output$fetch_ui3 <- renderUI(expr = if (req(input$query_method) == "spatial" & (req(input$polygon_source) == "draw" & !is.null(req(workspace$drawn_polygon_sf)))) {
    tagList(br(),
            actionButton(inputId = "fetch_data",
                         label = "Fetch data"))
  })
  
  # Building the links to other tabs!
  # Note that we have to do this with a() and an onclick argument that calls the
  # function defined way up at the top of all this. The a() is necessary because
  # we can't nest another layer of quotes in a string, being limited to "" and ''
  output$data_available_ui <- renderUI(if (!is.null(workspace$raw_data)) {
    tagList(br(),
            fluidRow(column(width = 10,
                            p(class = "data-prompt",
                              "You have data available in the",
                              a("Data tab!",
                                onclick = "tabJump('Data')"),
                              "The next step is to either configure benchmarks in the",
                              a("Benchmarks tab",
                                onclick = "tabJump('Benchmarks')"),
                              "or to go directly to making figures in the",
                              a("Figures tab.",
                                onclick = "tabJump('Figures')")))))
  })
  
  # Keys when grabbing data from the LDC by key values
  output$keys_input_ui <- renderUI(expr = if (req(input$query_method) %in% c("EcologicalSiteID", "PrimaryKey", "ProjectKey")) {
    message("query_method is in c('EcologicalSiteID', 'PrimaryKey', 'ProjectKey'). Rendering keys UI element.")
    # Use different placeholders for different key types!
    if (input$query_method == "EcologicalSiteID") {
      textInput(inputId = "keys",
                label = "Search key values",
                value = "",
                placeholder = "R042XB012NM")
    } else if (input$query_method == "PrimaryKey") {
      textInput(inputId = "keys",
                label = "Search key values",
                value = "")
    } else {
      textInput(inputId = "keys",
                label = "Search key values",
                value = "")
    }
  })
  
  output$keys_input_info_ui <- renderUI(expr = if (req(input$query_method) %in% c("EcologicalSiteID", "PrimaryKey", "ProjectKey")) {
    message("data_source is 'ldc'. Rendering keys_input_info UI element.")
    actionButton(inputId = "keys_input_info",
                 label = "",
                 class = "info-btn",
                 icon = icon("circle-question"))
  })
  
  # Uploading spatial data
  output$spatial_input_ui <- renderUI(expr = if (req(input$query_method) == "spatial" & req(input$data_source) == "ldc" & req(input$polygon_source) == "upload") {
    message("query_method is 'spatial'. Rendering spatial_input UI element.")
    fileInput(inputId = "polygons",
              label = "Polygons ZIP file",
              multiple = FALSE,
              accept = ".zip")
  })
  output$spatial_input_info_ui <- renderUI(expr = if (req(input$query_method) == "spatial" & req(input$data_source) == "ldc" & req(input$polygon_source) == "upload") {
    message("query_method is 'spatial'. Rendering spatial_input_info UI element.")
    actionButton(inputId = "spatial_input_info",
                 label = "",
                 class = "info-btn",
                 icon = icon("circle-question"))
  })
  # Only allow polygon selection if there's an uploaded polygon
  output$select_polygon_ui <- renderUI(expr = if (!is.null(req(input$polygons)) & req(input$query_method) == "spatial" & req(input$data_source) == "ldc" & req(input$polygon_source) == "upload") {
    message("There are polygons available to select from. Rendering polygons_layer UI element.")
    selectInput(inputId = "polygons_layer",
                label = "Polygons name",
                choices = c(""),
                selected = "")
  })
  output$select_polygon_info_ui <- renderUI(expr = if (!is.null(req(input$polygons)) & req(input$query_method) == "spatial" & req(input$data_source) == "ldc" & req(input$polygon_source) == "upload") {
    message("There are polygons available to select from. Rendering polygons_layer UI element.")
    actionButton(inputId = "select_polygons_info",
                 label = "",
                 class = "info-btn",
                 icon = icon("circle-question"))
  })
  # Only allow repair if there's an uploaded polygon
  output$repair_polygons_ui <- renderUI(expr = if (!is.null(req(input$polygons)) & req(input$query_method) == "spatial" & req(input$data_source) == "ldc" & req(input$polygon_source) == "upload") {
    message("There are polygons selected. Rendering repair_polygons UI element")
    checkboxInput(inputId = "repair_polygons",
                  label = "Repair polygons",
                  value = FALSE)
  })
  output$repair_polygons_info_ui <- renderUI(expr = if (!is.null(req(input$polygons)) & req(input$query_method) == "spatial" & req(input$data_source) == "ldc") {
    message("There are polygons selected. Rendering repair_polygons_info UI element")
    actionButton(inputId = "repair_polygons_info",
                 label = "",
                 class = "info-btn",
                 icon = icon("circle-question"))
  })
  ###### Start Main Panel ######################################################
  output$main_map_ui <- renderUI({
    if (req(input$query_method) != "spatial" & (!is.null(workspace$mapping_header_sf) | !is.null(workspace$mapping_polygons))) {
      message("Attempting to render main_map_ui")
      leafletOutput(outputId = "main_map",
                    height = "50vh")
    }
  })
  output$drawing_map_ui <- renderUI({
    if (req(input$query_method) == "spatial") {
      message("Attempting to render drawing_map_ui")
      leafletOutput(outputId = "drawing_map",
                    height = "50vh")
    }
  })
  
  ###### Data Main Panel #######################################################
  output$data_tab_notyet_ui <- renderUI({
    # if (is.null(workspace$raw_data)) {
    message("Rendering the no data warning")
    fluidRow(column(width = 10,
                    p(class = "data-prompt",
                      "If there are no data visible, please return to the",
                      a("Start tab",
                        onclick = "tabJump('Start')"),
                      "and make sure that you've either uploaded a CSV or queried the Landscape Data Commons.")))
    # }
  })
  
  ##### When the reset button is pressed #####
  observeEvent(eventExpr = req(input$reset_button),
               handlerExpr = {
                 message("Reset button pressed!")
                 shinyjs::refresh()
                 message("Executed shinyjs::refresh()")
               })
  
  observeEvent(eventExpr = req(input$clear_data),
               handlerExpr = {
                 message("Clear Data button pressed!")
                 workspace$raw_data <- NULL
                 output$data <- NULL
                 output$benchmark_plot <- NULL
                 output$benchmark_summary <- NULL
                 output$timeseries_plot <- NULL
                 output$timeseries_summary <- NULL
               })
  
  #### When a CSV is uploaded, do this ####
  observeEvent(eventExpr = input$raw_data,
               handlerExpr = {
                 workspace[["raw_data"]] <- read.csv(input$raw_data$datapath,
                                                     stringsAsFactors = FALSE)
               })
  
  ##### Polygon upload handling #####
  # When input$polygons updates, look at its filepath and read in the CSV
  observeEvent(eventExpr = input$polygons,
               handlerExpr = {
                 message("Polygons file uploaded")
                 # Because it looks like I can't enforce filetype in the upload
                 # selection dialogue, check it here
                 # I'm assuming that a file extension can be 1-5 characters long
                 # although the longest I've seen is 4, I think
                 polygon_upload_extension <- toupper(stringr::str_extract(string = input$polygons$datapath,
                                                                          pattern = "(?<=\\.).{1,5}$"))
                 polygons_are_zip <- polygon_upload_extension == "ZIP"
                 
                 if (!polygons_are_zip) {
                   showNotification(ui = "Polygons must be uploaded as a zipped shapefile or zipped geodatabase.",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    id = "polygons_zip_error",
                                    type = "error")
                 } else {
                   message("Attempting to unzip file")
                   # Unzip with an OS-specific system call
                   # Setting the working directory
                   setwd(dirname(input$polygons$datapath))
                   # Passing this to the OS
                   system(sprintf("cd %s", dirname(input$polygons$datapath)))
                   # Just checking for debugging
                   message(getwd())
                   # The unzipping argument to pass to the OS
                   system(sprintf("unzip -u %s", input$polygons$datapath))
                   # Set the working directory back
                   setwd(workspace$original_directory)
                   
                   message("File unzipped")
                   # Get the shapefile name
                   extracted_files <- list.files(dirname(input$polygons$datapath),
                                                 full.names = TRUE)
                   
                   # Look for extracted shapefiles
                   shp_indices <- grepl(extracted_files,
                                        pattern = "\\.shp$",
                                        ignore.case = TRUE)
                   message(paste0("Found ",
                                  sum(shp_indices),
                                  " shapefiles"))
                   upload_has_shp <- any(shp_indices)
                   
                   # Look for extracted geodatabases
                   gdb_indices <- grepl(extracted_files,
                                        pattern = "\\.gdb$",
                                        ignore.case = TRUE)
                   message(paste0("Found ",
                                  sum(gdb_indices),
                                  " geodatabases"))
                   upload_has_gdb <- any(gdb_indices)
                   
                   # Prioritize geodatabases
                   if (upload_has_gdb) {
                     workspace$polygon_filetype <- "gdb"
                     message("Working from extracted geodatabase")
                     # If there's more than one geodatabase, just use the first
                     # but warn the user
                     if (sum(gdb_indices) > 1) {
                       message("Multiple GDBs detected. Using 'first' one")
                       showNotification(ui = "More than one geodatabase found. Please upload one at a time.",
                                        duration = NULL,
                                        closeButton = TRUE,
                                        type = "warning",
                                        id = "multiple_gdb_warning")
                       current_gdb_path <- extracted_files[gdb_indices][1]
                     } else {
                       message("Only one GDB to work with")
                       current_gdb_path <- extracted_files[gdb_indices]
                     }
                     # So I can reference this when reading in layers later
                     workspace$gdb_filepath <- current_gdb_path
                     # Find which layers are available
                     available_polygons <- sf::st_layers(dsn = current_gdb_path)$name
                     message(paste0("Available layers in GDB are: ",
                                    paste(available_polygons,
                                          collapse = ", ")))
                     message("Updating selectInput(inputId = 'polygons_layer')")
                     # Update the selection options
                     updateSelectInput(session = session,
                                       inputId = "polygons_layer",
                                       choices = available_polygons,
                                       selected = available_polygons[1])
                   } else if (upload_has_shp) {
                     workspace$polygon_filetype <- "shp"
                     message("Working with extracted shapefile(s)")
                     # Which files end in .shp?
                     available_polygons <- extracted_files[shp_indices]
                     
                     message(paste0("Before checking for all files, the available shapefiles are: ",
                                    paste(basename(available_polygons),
                                          collapse = ", ")))
                     
                     # And which of those .shp files has associated
                     # .prj, .dbf, and .shx files?
                     has_all_files <- sapply(X = available_polygons,
                                             files = extracted_files,
                                             FUN = function(X, files) {
                                               required_filetypes <- c("dbf",
                                                                       "prj",
                                                                       "shp",
                                                                       "shx")
                                               file_pattern <- gsub(x = X,
                                                                    pattern = "shp$",
                                                                    replacement = "",
                                                                    ignore.case = TRUE)
                                               all(sapply(X = required_filetypes,
                                                          files = files,
                                                          file_pattern = file_pattern,
                                                          FUN = function(X, files, file_pattern) {
                                                            paste0(file_pattern,
                                                                   X) %in% files
                                                          }))
                                             })
                     # So then which shapefiles are really valid?
                     available_polygons <- available_polygons[has_all_files]
                     message(paste0("After checking for all files, the available shapefiles are: ",
                                    paste(basename(available_polygons),
                                          collapse = ", ")))
                     
                     # Update the selection options
                     # This makes a named vector of the shp filepaths
                     # so it's easy to read them in later but the options are
                     # human readable in the GUI
                     polygon_shp_options <- available_polygons
                     shp_filenames <- gsub(x = basename(available_polygons),
                                           pattern = "\\.shp$",
                                           replacement = "",
                                           ignore.case = TRUE)
                     names(available_polygons) <- shp_filenames
                     updateSelectInput(session = session,
                                       inputId = "polygons_layer",
                                       choices = available_polygons,
                                       selected = available_polygons[1])
                   } else {
                     showNotification(ui = "Uploaded file does not appear to contain either a valid shapefile or geodatabase.",
                                      duration = NULL,
                                      closeButton = TRUE,
                                      type = "error",
                                      id = "empty_upload_error")
                   }
                 }
               })
  
  #### Map time ####
  observeEvent(eventExpr = list(workspace$mapping_header_sf,
                                workspace$mapping_polygons),
               handlerExpr = {
                 message("Something changed for mapping purposes.")
                 # Initialize the map
                 map <- leaflet::leaflet()
                 
                 # Add some basic info
                 map <- leaflet::addTiles(map = map)
                 
                 # Add the polygons
                 message("Checking to see if !is.null(workspace$mapping_polygons)")
                 if (!is.null(workspace$mapping_polygons)) {
                   message("!is.null(workspace$mapping_polygons) was TRUE")
                   # Note that we have to manually remove Z dimensions with sf::st_zm()
                   # otherwise if there's a Z dimension this fails with an
                   # inscrutable error.
                   map <- leaflet::addPolygons(map = map,
                                               data = sf::st_transform(x = sf::st_zm(workspace$mapping_polygons),
                                                                       crs = "+proj=longlat +datum=WGS84"),
                                               fillColor = "coral",
                                               stroke = FALSE,
                                               fillOpacity = 0.5)
                 }
                 
                 # Add in the retrieved points
                 message("Checking to see if !is.null(workspace$mapping_header_sf)")
                 if (!is.null(workspace$mapping_header_sf)) {
                   message("!is.null(workspace$mapping_header_sf) was TRUE")
                   map <- leaflet::addCircleMarkers(map = map,
                                                    data = sf::st_transform(x = workspace$mapping_header_sf,
                                                                            crs = "+proj=longlat +datum=WGS84"),
                                                    stroke = TRUE,
                                                    opacity = 0.9,
                                                    color = "white",
                                                    weight = 1,
                                                    fillColor = "gray20",
                                                    fillOpacity = 1,
                                                    radius = 3)
                 }
                 
                 if (is.null(workspace$mapping_polygons) & is.null(workspace$mapping_header_sf)) {
                   # Set the framing
                   map <- setView(map = map,
                                  lng = -119,
                                  lat = 38.7,
                                  zoom = 4.25)
                   map <- setMaxBounds(map = map,
                                       lng1 = -125.5,
                                       lat1 = 25,
                                       lng2 = -66,
                                       lat2 = 49.5)
                 }
                 
                 # Add in the drawing controls
                 map_drawing <- addDrawToolbar(map = map,
                                               targetGroup = "draw",
                                               position = 'topleft',
                                               polylineOptions = FALSE,
                                               circleOptions = FALSE,
                                               markerOptions = FALSE,
                                               circleMarkerOptions = FALSE,
                                               singleFeature = TRUE)
                 
                 message("Rendering map")
                 output$drawing_map <- leaflet::renderLeaflet(map_drawing)
                 output$main_map <- leaflet::renderLeaflet(map)
                 message("Map rendered")
               })
  
  ###### Making a polygon sf object from the polygon drawn on the map ##########
  # This is adapted from the RAP Production Explorer
  # Character string of coordinates from drawn features on Leaflet map
  observeEvent(input$drawing_map_draw_new_feature,{
    message("There's a new polygon drawn on the map! Getting coordinates")
    # This builds a neat little [x, y] string for each vertex
    # Frankly, this is a little silly to do considering we're going to split them
    # into a vector, but I can't be bothered to refactor beyond changing it to
    # a sapply() because it works as-is and isn't hurting anyone
    coords <- sapply(X = input$drawing_map_draw_new_feature$geometry$coordinates[1][[1]],
                     FUN = function(X) {
                       paste0("[", X[1], ", ", X[2], "]")
                     })
    coord_string <- paste0(coords,
                           collapse = ", ")
    workspace$drawn_coordinates <- coord_string
    message("Coordinates saved to workspace$drawn_coordinates")
  })
  # Convert the coordinates of the vertices on the map into a polygon sf object!
  observeEvent(eventExpr = workspace$drawn_coordinates,
               handlerExpr = {
                 if (!is.null(workspace$drawn_coordinates)) {
                   coords <- workspace$drawn_coordinates
                   message("workspace$drawn_coordinates has updated! Attempting to create a polygon sf object using the coordinates as vertices")
                   print(coords)
                   message("Cleaning coordinates and creating vector")
                   coords_clean <- strsplit(x = gsub(x = coords,
                                                     pattern = "\\[|\\]",
                                                     replacement = ""),
                                            split = ',') 
                   print(coords_clean)
                   message("Converting vector to numeric")
                   coords_numeric <- as.numeric(coords_clean[[1]])
                   print(coords_numeric)
                   n_vertices <- length(coords_clean[[1]])/2
                   message("Creating a matrix from the coordinates")
                   coords_matrix <- matrix(coords_numeric, nrow = n_vertices, byrow = TRUE)
                   coords_list <- list(coords_matrix)
                   message("Making a polygon matrix thingy")
                   print(coords_list)
                   polygon <- sf::st_polygon(coords_list)
                   message("Making an sf object")
                   polygon <- st_sfc(polygon,
                                     crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                   workspace$drawn_polygon_sf <- sf::st_sf(polygon)
                   message("Polygon saved to workspace$drawn_polygon_sf")
                 }
               })
  
  #### Making freshly-uploaded polygons map-ready ####
  observeEvent(eventExpr = {input$polygons_layer},
               handlerExpr = {
                 message("input$polygons_layer has updated")
                 if (input$polygons_layer != "") {
                   message("Reading in polygons")
                   if (workspace$polygon_filetype == "gdb") {
                     workspace$mapping_polygons <- sf::st_read(dsn = workspace$gdb_filepath,
                                                               layer = input$polygons_layer)
                   } else if (workspace$polygon_filetype == "shp") {
                     workspace$mapping_polygons <- sf::st_read(dsn = input$polygons_layer)
                   }
                   message("Making sure the polygons are in NAD83")
                   workspace$mapping_polygons <- sf::st_transform(workspace$mapping_polygons,
                                                                  crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                   
                   # If they've asked us to "repair" polygons, buffer by 0
                   if (input$repair_polygons) {
                     message("Attempting to repair polygons by buffering by 0")
                     workspace$mapping_polygons <- sf::st_buffer(x = workspace$mapping_polygons,
                                                                 dist = 0)
                   }
                 }
               })
  
  #### When someone fetches data from the Landscape Data Commons, do this ####
  observeEvent(eventExpr = input$fetch_data,
               handlerExpr = {
                 # Set this variable so we can handle the data appropriately based on source
                 # Since there are from the LDC, we'll also be looking for header info
                 workspace$current_data_source <- "ldc"
                 message("Data source set to LDC")
                 
                 # Only do anything if there's at least one key
                 if (input$query_method == "spatial") {
                   message("input$query_method is 'spatial'")
                   message("Attempting to query spatially")
                   if (input$polygon_source == "upload") {
                     if (req(input$polygons_layer) == "") {
                       message("Currently expecting uploaded polygons but there are none selected.")
                       showNotification(ui = "Please upload and select polygons or drawn a polygon instead.",
                                        duration = NULL,
                                        closeButton = TRUE,
                                        type = "warning",
                                        id = "no_polygons_yet_warning")
                     } else {
                       message("Reading in polygons")
                       if (workspace$polygon_filetype == "gdb") {
                         workspace$polygons <- sf::st_read(dsn = workspace$gdb_filepath,
                                                           layer = input$polygons_layer)
                       } else if (workspace$polygon_filetype == "shp") {
                         workspace$polygons <- sf::st_read(dsn = input$polygons_layer)
                       }
                       
                       # If they've asked us to "repair" polygons, buffer by 0
                       if (input$repair_polygons) {
                         message("Attempting to repair polygons by buffering by 0")
                         message("Making sure the polygons are in NAD83 beforehand")
                         workspace$polygons <- sf::st_transform(workspace$polygons,
                                                                crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                         workspace$polygons <- sf::st_buffer(x = workspace$polygons,
                                                             dist = 0)
                         message("Polygons are buffered")
                       }
                     }
                   } else {
                     if (!is.null(req(workspace$drawn_polygon_sf))) {
                       message("Using drawn polygon")
                       workspace$polygons <- workspace$drawn_polygon_sf
                     }
                   }
                   message("Making sure the polygons are in NAD83")
                   workspace$polygons <- sf::st_transform(workspace$polygons,
                                                          crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                   
                   message(paste0("Number of individual polygons in workspace$polygons is ",
                                  nrow(workspace$polygons)))
                   message("Adding unique_id variable to workspace$polygons")
                   workspace$polygons[["unique_id"]] <- 1:nrow(workspace$polygons)
                   
                   # For mapping purposes
                   message("Updating workspace$mapping_polygons")
                   workspace$mapping_polygons <- workspace$polygons
                   
                   if (is.null(workspace$headers)) {
                     message("Retrieving headers")
                     workspace$headers <- tryCatch(fetch_ldc(keys = NULL,
                                                             key_type = NULL,
                                                             data_type = "header",
                                                             verbose = TRUE),
                                                   error = function(error){
                                                     gsub(x = error,
                                                          pattern = "^Error.+[ ]:[ ]",
                                                          replacement = "")
                                                   })
                     message(paste0("class(workspace$headers) is ",
                                    paste(class(workspace$headers),
                                          collapse = ", ")))
                   }
                   
                   current_headers <- workspace$headers
                   
                   
                   # If there was an API error, display that
                   if ("character" %in% class(current_headers)) {
                     results <- NULL
                     showNotification(ui = paste0("API error retrieving headers for spatial query: ",
                                                  current_headers),
                                      duration = NULL,
                                      closeButton = TRUE,
                                      id = "headers_for_sf_error",
                                      type = "error")
                   } else {
                     # If there was no error, proceed
                     message("Converting header info to sf object")
                     current_headers_sf <- sf::st_as_sf(x = current_headers,
                                                        coords = c("Longitude_NAD83",
                                                                   "Latitude_NAD83"),
                                                        crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                     
                     # This'll be useful so I can make a map, if that feature is added
                     workspace$header_sf <- current_headers_sf
                     workspace$mapping_header_sf <- current_headers_sf
                     
                     message("Performing sf_intersection()")
                     points_polygons_intersection <- tryCatch(sf::st_intersection(x = current_headers_sf[, "PrimaryKey"],
                                                                                  y = sf::st_transform(workspace$polygons[, "unique_id"],
                                                                                                       crs = sf::st_crs(current_headers_sf))),
                                                              error = function(error){"There was a geoprocessing error. Please try using the 'repair polygons' option."})
                     
                     if ("character" %in% class(points_polygons_intersection)) {
                       showNotification(ui = points_polygons_intersection,
                                        duration = NULL,
                                        closeButton = TRUE,
                                        type = "error",
                                        id = "intersection_error")
                     } else {
                       current_primary_keys <- unique(points_polygons_intersection$PrimaryKey)
                       
                       if (length(current_primary_keys) < 1) {
                         message("No data were found")
                         showNotification(ui = paste0("No data were found within your polygons."),
                                          duration = NULL,
                                          closeButton = TRUE,
                                          id = "no_overlap",
                                          type = "warning")
                         results <- NULL
                       } else {
                         message(paste0(length(current_primary_keys), " primary keys found. Querying now."))
                         
                         message("Retrieving data using PrimaryKey values from spatial intersection")
                         results <- tryCatch(fetch_ldc(keys = current_primary_keys,
                                                       key_type = "PrimaryKey",
                                                       data_type = "indicators",
                                                       key_chunk_size = 100,
                                                       verbose = TRUE),
                                             error = function(error){
                                               gsub(x = error,
                                                    pattern = "^Error.+[ ]:[ ]",
                                                    replacement = "")
                                             })
                         message("Querying by primary key complete.")
                         message(paste0("Number of records retrieved: ",
                                        length(results)))
                       }
                       
                       # Only keep going if there are results!!!!
                       if (length(results) > 0 & "data.frame" %in% class(results)) {
                         message("Making workspace$mapping_header_sf")
                         workspace$mapping_header_sf <- current_headers_sf[current_headers_sf$PrimaryKey %in% results$PrimaryKey,]
                         
                         message("Coercing variables to numeric.")
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
                         message("Setting data_fresh to TRUE because we just downloaded it")
                         workspace$data_fresh <- TRUE
                         workspace$raw_data <- data
                       } else if (length(results) == 0) {
                         message("No records found for those PrimaryKeys")
                         if (length(current_primary_keys) > 0) {
                           no_data_spatial_error_message <- paste0("Although sampling locations were found within your polygons, they did not have associated data of the type requested.")
                         } else {
                           no_data_spatial_error_message <- paste0("No sampling locations were found within your polygons.")
                         }
                         showNotification(ui = paste0(no_data_spatial_error_message,
                                                      results),
                                          duration = NULL,
                                          closeButton = TRUE,
                                          id = "no_data_spatial_error",
                                          type = "error")
                         workspace$raw_data <- NULL
                       } else {
                         showNotification(ui = paste0("API error retrieving data based on spatial query: ",
                                                      results),
                                          duration = NULL,
                                          closeButton = TRUE,
                                          id = "primarykey_spatial_error",
                                          type = "error")
                         workspace$raw_data <- NULL
                       }
                     }
                   }
                 } else if (req(input$keys) != "") {
                   message("Querying by keys")
                   # Handle multiple requested ecosites at once!
                   current_key_vector <- stringr::str_split(string = input$keys,
                                                            pattern = ",",
                                                            simplify = TRUE)
                   
                   # This will make it easy to check to see if any of these values
                   # weren't associated with data
                   current_key_vector <- trimws(current_key_vector)
                   
                   # fetch_ldc() can take a vector (slow, retrieves one at a time)
                   # or a string of values separated by commas (fast, retrieves all at once)
                   current_key_string <- paste(current_key_vector,
                                               collapse = ",")
                   
                   
                   # The API queryable tables don't include ecosite, so we grab
                   # the header table and get primary keys from that
                   if (input$query_method == "EcologicalSiteID") {
                     workspace$mapping_polygons <- NULL
                     message("query_method is EcologicalSiteID")
                     message("Retrieving headers")
                     current_headers <- tryCatch(fetch_ldc(keys = current_key_string,
                                                           key_type = input$query_method,
                                                           data_type = "header",
                                                           verbose = TRUE),
                                                 error = function(error){
                                                   gsub(x = error,
                                                        pattern = "^Error.+[ ]:[ ]",
                                                        replacement = "")
                                                 })
                     message(paste0("class(current_headers) is ",
                                    paste(class(current_headers),
                                          collapse = ", ")))
                     if ("character" %in% class(current_headers)) {
                       showNotification(ui = current_headers,
                                        duration = NULL,
                                        closeButton = TRUE,
                                        type = "error",
                                        id = "api_headers_error")
                       results <- NULL
                     } else {
                       message("Converting header info to sf object")
                       current_headers_sf <- sf::st_as_sf(x = current_headers,
                                                          coords = c("Longitude_NAD83",
                                                                     "Latitude_NAD83"),
                                                          crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                       
                       # This'll be useful so I can make a map, if that feature is added
                       workspace$mapping_header_sf <- current_headers_sf
                       
                       current_primary_keys <- current_headers$PrimaryKey
                       
                       current_key_chunk_count <- ceiling(length(current_primary_keys) / 100)
                       
                       current_primary_keys <- sapply(X = 1:current_key_chunk_count,
                                                      keys_vector = current_primary_keys,
                                                      key_chunk_size = 100,
                                                      key_count = length(current_primary_keys),
                                                      FUN = function(X, keys_vector, key_chunk_size, key_count) {
                                                        min_index <- max(c(1, (X - 1) * key_chunk_size + 1))
                                                        max_index <- min(c(key_count, X * key_chunk_size))
                                                        indices <- min_index:max_index
                                                        paste(keys_vector[indices],
                                                              collapse = ",")
                                                      })
                       message(paste(current_primary_keys,
                                     collapse = ", "))
                       message("Retrieving data using PrimaryKey values from headers")
                       results <- tryCatch(fetch_ldc(keys = current_primary_keys,
                                                     key_type = "PrimaryKey",
                                                     data_type = "indicators",
                                                     verbose = TRUE),
                                           error = function(error){
                                             gsub(x = error,
                                                  pattern = "^Error.+[ ]:[ ]",
                                                  replacement = "")
                                           })
                     }
                   } else if (input$query_method != "spatial") {
                     workspace$mapping_polygons <- NULL
                     message("query_method is not EcologicalSiteID or spatial")
                     message("Retrieving data using provided keys")
                     current_key_chunk_count <- ceiling(length(current_key_vector) / 100)
                     
                     current_keys_chunks <- sapply(X = 1:current_key_chunk_count,
                                                   keys_vector = current_key_vector,
                                                   key_chunk_size = 100,
                                                   key_count = length(current_key_vector),
                                                   FUN = function(X, keys_vector, key_chunk_size, key_count) {
                                                     min_index <- max(c(1, (X - 1) * key_chunk_size + 1))
                                                     max_index <- min(c(key_count, X * key_chunk_size))
                                                     indices <- min_index:max_index
                                                     paste(keys_vector[indices],
                                                           collapse = ",")
                                                   })
                     
                     results <- tryCatch(fetch_ldc(keys = current_keys_chunks,
                                                   key_type = input$query_method,
                                                   data_type = "indicators",
                                                   verbose = TRUE),
                                         error = function(error){
                                           gsub(x = error,
                                                pattern = "^Error.+[ ]:[ ]",
                                                replacement = "")
                                         })
                   }
                   
                   
                   # So we can tell the user later which actually got queried
                   if (is.null(results)) {
                     message("No data from LDC!")
                     workspace$missing_keys <- current_key_vector
                     showNotification(ui = "No data were found associated with your keys.",
                                      duration = NULL,
                                      closeButton = TRUE,
                                      type = "warning",
                                      id = "no_data_returned_warning")
                   } else if ("character" %in% class(results)) {
                     # If results is actually an error message, display it
                     showNotification(ui = results,
                                      duration = NULL,
                                      closeButton = TRUE,
                                      type = "error",
                                      id = "api_error")
                     workspace$missing_keys <- NULL
                   } else {
                     message("There are results!")
                     message("Determining if keys are missing.")
                     message(paste0("input$query_method is: ",
                                    paste(input$query_method,
                                          collapse = ", ")))
                     # Because ecosites were two-stage, we check in with headers
                     if (input$query_method %in% c("EcologicalSiteID")) {
                       workspace$queried_keys <- unique(current_headers[[input$query_method]])
                     } else if (input$query_method != "spatial") {
                       workspace$queried_keys <- unique(results[[input$query_method]])
                     }
                     
                     workspace$missing_keys <- current_key_vector[!(current_key_vector %in% workspace$queried_keys)]
                   }
                   
                   message("Determining if workspace$missing_keys has length > 0")
                   if (length(workspace$missing_keys) > 0) {
                     message(paste0("The following key values are missing: ",
                                    paste(workspace$missing_keys,
                                          collapse = ", ")))
                     key_error <- paste0("The following keys did not have data associated with them: ",
                                         paste(workspace$missing_keys,
                                               collapse = ", "))
                     showNotification(ui = key_error,
                                      duration = NULL,
                                      closeButton = TRUE,
                                      type = "warning",
                                      id = "key_error")
                   } else {
                     message("No missing keys!")
                   }
                   
                   
                   # Only keep going if there are results!!!!
                   if (length(results) > 0 & "data.frame" %in% class(results)) {
                     message("Coercing variables to numeric.")
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
                     message("Setting data_fresh to TRUE because we just downloaded it")
                     workspace$data_fresh <- TRUE
                     workspace$raw_data <- data
                     
                     message("Attempting to make workspace$header_sf")
                     if (is.null(workspace$headers)) {
                       message("Retrieving headers")
                       workspace$headers <- tryCatch(fetch_ldc(keys = NULL,
                                                               key_type = NULL,
                                                               data_type = "header",
                                                               verbose = TRUE),
                                                     error = function(error){
                                                       gsub(x = error,
                                                            pattern = "^Error.+[ ]:[ ]",
                                                            replacement = "")
                                                     })
                       message(paste0("class(workspace$headers) is ",
                                      paste(class(workspace$headers),
                                            collapse = ", ")))
                     }
                     
                     message("Converting header info to sf object")
                     message(paste("workspace$headers variables are: ",
                                   paste0(names(workspace$headers),
                                          collapse = ", ")))
                     current_headers_sf <- sf::st_as_sf(x = workspace$headers[workspace$headers[["PrimaryKey"]] %in% workspace$raw_data[["PrimaryKey"]], ],
                                                        coords = c("Longitude_NAD83",
                                                                   "Latitude_NAD83"),
                                                        crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                     
                     # This'll be useful so I can make a map, if that feature is added
                     workspace$mapping_header_sf <- current_headers_sf
                   } else {
                     workspace$raw_data <- NULL
                   }
                   
                 } else {
                   showNotification(ui = "Please provide key values to search with.",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    id = "no_keys_warning",
                                    type = "warning")
                 }
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
                   
                   message("Rendering data table")
                   output$data_table <- DT::renderDT(workspace$raw_data,
                                                     rownames = FALSE,
                                                     options = list(pageLength = 10,
                                                                    fixedHeader = TRUE,
                                                                    scrollX = TRUE), 
                                                     extensions = "FixedHeader")
                   message("Data table rendered")
                 }
               })
  
  #### When the indicator is updated, do this ####
  observeEvent(eventExpr = input$variable,
               handlerExpr = {
                 updateTextInput(session,
                                 inputId = "variable_name",
                                 value = input$variable)
                 message("Updating minimum value to", min(workspace$raw_data[[input$variable]],
                                                          na.rm = TRUE))
                 updateNumericInput(session,
                                    inputId = "value_min",
                                    value = min(0,
                                                min(workspace$raw_data[[input$variable]],
                                                    na.rm = TRUE)))
                 message("Updating maximum value to", max(workspace$raw_data[[input$variable]],
                                                          na.rm = TRUE))
                 updateNumericInput(session,
                                    inputId = "value_max",
                                    value = max(100,
                                                max(workspace$raw_data[[input$variable]],
                                                    na.rm = TRUE)))
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
                 # Clean out the temp directory just to be safe
                 current_files_in_temp_dir <- list.files(path = workspace$temp_directory,
                                                         full.names = TRUE)
                 for (file in current_files_in_temp_dir) {
                   file.remove(file)
                 }
                 
                 # Get a copy of the data to manipulate for plotting
                 plotting_data <- workspace$raw_data
                 
                 
                 # Only plot if there's data!!!!
                 if (!is.null(plotting_data)){
                   # Grab the variable name to work with
                   variable_name <- input$variable
                   
                   #! What are the quantile proportions?
                   # Use 25%, 50%, and 75% for boxplots
                   if (input$plot_type == "boxplot") {
                     quantile_proportions <- c(0.25, 0.5, 0.75)
                   } else {
                     quantile_proportions <- workspace$quantiles
                   }
                   
                   # Make sure they're in order from smallest to largest
                   quantile_proportions <- quantile_proportions[order(quantile_proportions)]
                   
                   # Write that current variable to a new one so we can plot easily
                   # And make sure it's numeric, not integer or something
                   plotting_data[["current_variable"]] <- as.numeric(plotting_data[[variable_name]])
                   
                   # Get those values as a vector
                   current_data_vector <- plotting_data[[variable_name]]
                   
                   # Get the mean and standard deviation for the current data
                   current_data_mean <- mean(current_data_vector,
                                             na.rm = TRUE)
                   current_data_stddev <- sd(current_data_vector,
                                             na.rm = TRUE)
                   
                   # Find the quantile breaks
                   quantiles <- quantile(current_data_vector,
                                         probs = quantile_proportions,
                                         na.rm = TRUE)
                   # Drop 0% and 100% if they're in there
                   quantile_indices <- !(names(quantiles) %in% c("0%", "100%"))
                   quantiles <- quantiles[quantile_indices]
                   
                   # Make these names work for the figure, e.g. convert from "75%" to "50% to 75%"
                   original_quantile_names <- names(quantiles)
                   updated_quantile_names <- original_quantile_names
                   for (quantile_index in seq_len(length(original_quantile_names))) {
                     if (quantile_index == 1) {
                       updated_quantile_names[quantile_index] <- paste0("0% to ", original_quantile_names[quantile_index])
                     } else {
                       updated_quantile_names[quantile_index] <- paste0(original_quantile_names[quantile_index - 1], " to ", original_quantile_names[quantile_index])
                     }
                   }
                   names(quantiles) <- updated_quantile_names
                   
                   # Write in the quantile that the values belong in, starting with all of them in 100%
                   plotting_data[["Quantile"]] <- paste0(original_quantile_names[length(original_quantile_names)], " to 100%")
                   
                   # Then in order from largest to smallest, assign the rest of the quantiles
                   for (current_quantile in names(quantiles)[length(quantiles):1]) {
                     plotting_data[["Quantile"]][plotting_data[["current_variable"]] <= quantiles[current_quantile]] <- current_quantile
                   }
                   
                   # Correct the order of the legend items
                   quantile_names <- unique(c(names(quantiles),
                                              paste0(original_quantile_names[length(original_quantile_names)], " to 100%")))
                   plotting_data[["Quantile"]] <- factor(plotting_data[["Quantile"]],
                                                         levels = quantile_names)
                   
                   # Add in a variable for the variable name
                   plotting_data$variable_name <- input$variable_name
                   
                   # Make the dang plot happen!
                   if (nrow(plotting_data) > 0) {
                     message("Plotting the quantiles plot")
                     
                     if (input$plot_type == "histogram") {
                       workspace$quantile_plot <- ggplot() +
                         geom_histogram(data = plotting_data,
                                        aes(y = current_variable,
                                            fill = Quantile),
                                        binwidth = 1) +
                         scale_fill_manual(values = workspace$palette) +
                         geom_hline(yintercept = quantiles,
                                    size = 1,
                                    color = "gray50") +
                         scale_y_continuous(limits = c(input$value_min, input$value_max),
                                            expand = expansion(mult = c(0, 0))) +
                         scale_x_continuous(expand = c(0, 0)) +
                         labs(x = "Count of data points",
                              y = input$variable_name) +
                         theme(panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               panel.background = element_rect(fill = "gray95")) +
                         coord_flip()
                     } else if (input$plot_type == "boxplot") {
                       # With jittered points
                       workspace$quantile_plot <- ggplot(data = plotting_data,
                                                         aes(y = current_variable,
                                                             x = variable_name)) +
                         geom_jitter(aes(color = Quantile,
                                         fill = Quantile,
                                         shape = Quantile),
                                     alpha = 0.45,
                                     width = 0.125,
                                     height = 0) +
                         geom_boxplot(width = 0.25,
                                      outlier.shape = NA,
                                      size = 1,
                                      fill = NA) +
                         scale_color_manual(values = workspace$palette) +
                         scale_fill_manual(values = workspace$palette) +
                         scale_shape_manual(values = c(22, 21, 24, 23)) +
                         scale_y_continuous(limits = c(input$value_min, input$value_max),
                                            expand = expansion(mult = c(0, 0))) +
                         labs(x = NULL,
                              y = "Indicator value") +
                         theme(panel.grid.major.y = element_blank(),
                               panel.grid.minor.y = element_blank(),
                               panel.background = element_rect(fill = "gray95"),
                               axis.text.y = element_text(angle = 90,
                                                          hjust = 0.5)) +
                         coord_flip()
                       
                       # # With a dotplot
                       # workspace$quantile_plot <- ggplot(data = plotting_data,
                       #                                   aes(y = current_variable,
                       #                                       x = variable_name)) +
                       #   geom_dotplot(aes(color = Quantile,
                       #                    fill = Quantile),
                       #                binaxis = "y",
                       #                stackdir = "center",
                       #                dotsize = 0.1) +
                       #   geom_boxplot(width = 0.15,
                       #                outlier.shape = NA,
                       #                size = 1,
                       #                alpha = 0.5) +
                       #   scale_color_manual(values = workspace$palette) +
                       #   scale_fill_manual(values = workspace$palette) +
                       #   scale_y_continuous(expand = c(0, 0)) +
                       #   labs(x = NULL,
                       #        y = "Indicator value") +
                       #   theme(panel.grid.major.y = element_blank(),
                       #         panel.grid.minor.y = element_blank(),
                       #         panel.background = element_rect(fill = "gray95")) +
                       #   coord_flip()
                       
                       # # With violin plots
                       # workspace$quantile_plot <- ggplot()
                       # 
                       # for (quantile in quantile_names) {
                       #   workspace$quantile_plot <- workspace$quantile_plot +
                       #     geom_violin(data = plotting_data[plotting_data$Quantile == quantile, ],
                       #                 aes(y = current_variable,
                       #                     x = variable_name,
                       #                     color = Quantile,
                       #                     fill = Quantile))
                       #   
                       # }
                       # 
                       # workspace$quantile_plot <- workspace$quantile_plot +
                       #   geom_boxplot(data = plotting_data,
                       #                aes(y = current_variable,
                       #                    x = variable_name),
                       #                width = 0.1,
                       #                outlier.shape = NA,
                       #                size = 1,
                       #                alpha = 0.5) +
                       #   scale_color_manual(values = workspace$palette) +
                       #   scale_fill_manual(values = workspace$palette) +
                       #   scale_y_continuous(expand = c(0, 0)) +
                       #   labs(x = NULL,
                       #        y = "Indicator value") +
                       #   theme(panel.grid.major.y = element_blank(),
                       #         panel.grid.minor.y = element_blank(),
                       #         panel.background = element_rect(fill = "gray95")) +
                       #   coord_flip()
                     }
                     
                     
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
                                                   paste0(paste0(paste0(original_quantile_names, " of data points have a value <= "),
                                                                 round(quantiles, digits = 1),
                                                                 collapse = ", "),
                                                          ", and 100% of data points have a value <= ", round(max(current_data_vector, na.rm = TRUE), digits = 1)))
                   
                   if (input$plot_type == "boxplot") {
                     quantile_plot_caption <- paste0(quantile_plot_caption,
                                                     " Whiskers are drawn with a length equal to 1.5 times the interquartile range or to the most extreme data point, whichever is shortest.")
                   }
                   
                   quantile_plot_caption <- paste0(quantile_plot_caption,
                                                   " The mean value is ", round(current_data_mean, digits = 1),
                                                   " and the standard deviation is ", round(current_data_stddev, digits = 1), ".")
                   
                   # Okay, but we also need the captions to reflect the comparison values if they were requested!
                   if (input$compare) {
                     if (length(comparison_vector) == 1) {
                       comparison_caption_text <- paste0("The black dashed line marks the value ",
                                                         round(comparison_vector,
                                                               digits = 1),
                                                         ".")
                     } else if (length(comparison_vector) == 2) {
                       comparison_vector_string <- paste(round(comparison_vector,
                                                               digits = 1),
                                                         collapse = " and ")
                       comparison_caption_text <- paste0("The black dashed lines mark the values ",
                                                         comparison_vector_string,
                                                         ".")
                     } else if (length(comparison_vector) > 2) {
                       comparison_vector_string <- stringr::str_replace(paste(round(comparison_vector,
                                                                                    digits = 1),
                                                                              collapse = ", "),
                                                                        pattern = ", (?=\\d{1,100}\\.{0,1}\\d{0,1}$)",
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
                     
                     if (input$plot_type == "histogram") {
                       # Plot the histogram with benchmark info!
                       workspace$benchmark_plot <- ggplot() +
                         geom_histogram(data = plotting_data,
                                        aes(y = current_variable,
                                            fill = benchmark_results),
                                        binwidth = 1) +
                         scale_fill_manual(values = workspace$palette) +
                         scale_y_continuous(limits = c(input$value_min, input$value_max),
                                            expand = expansion(mult = c(0, 0))) +
                         scale_x_continuous(expand = c(0, 0)) +
                         labs(x = "Count of data points",
                              y = input$variable_name,
                              fill = "Benchmark status") +
                         theme(panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               panel.background = element_rect(fill = "gray95")) +
                         coord_flip()
                     } else if (input$plot_type == "boxplot") {
                       workspace$benchmark_plot <- ggplot(data = plotting_data,
                                                          aes(y = current_variable,
                                                              x = variable_name)) +
                         geom_jitter(aes(color = benchmark_results,
                                         fill = benchmark_results,
                                         shape = benchmark_results),
                                     alpha = 0.45,
                                     width = 0.125,
                                     height = 0) +
                         geom_boxplot(width = 0.25,
                                      outlier.shape = NA,
                                      size = 1,
                                      fill = NA) +
                         scale_color_manual(values = workspace$palette) +
                         scale_fill_manual(values = workspace$palette) +
                         scale_shape_manual(values = c(22, 21, 24, 23)) +
                         scale_y_continuous(limits = c(input$value_min, input$value_max),
                                            expand = expansion(mult = c(0, 0))) +
                         labs(x = NULL,
                              y = "Indicator value",
                              fill = "Benchmark status",
                              color = "Benchmark status",
                              shape = "Benchmark status") +
                         theme(panel.grid.major.y = element_blank(),
                               panel.grid.minor.y = element_blank(),
                               panel.background = element_rect(fill = "gray95"),
                               axis.text.y = element_text(angle = 90,
                                                          hjust = 0.5)) +
                         coord_flip()
                     }
                     
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
                     
                     if (input$plot_type == "boxplot") {
                       benchmark_plot_caption <- paste0(benchmark_plot_caption,
                                                        " Whiskers are drawn with a length equal to 1.5 times the interquartile range or to the most extreme data point, whichever is shortest.")
                     }
                     
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
                     workspace$timeseries_plot <- ggplot(data = timeseries_plotting_data,
                                                         aes(x = year,
                                                             y = value)) +
                       geom_jitter(aes(color = year),
                                   alpha = 0.45,
                                   # width = 0.125,
                                   height = 0) +
                       geom_boxplot(fill = NA,
                                    outlier.shape = NA) +
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
                                     selected = "Figures") 
                   
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
                                              full.names = FALSE)
                   
                   message("Preparing to zip up the following files:")
                   message(paste0(files_to_zip,
                                  collapse = ", "))
                   
                   
                   switch(Sys.info()[["sysname"]],
                          Windows = {
                            message("This is a Windows system. Using 7zip.")
                            system(paste0("cmd.exe /c \"C:\\Program Files\\7-Zip\\7z\".exe a -tzip plots.zip ",
                                          paste(files_to_zip,
                                                collapse = " ")))
                          },
                          Linux = {
                            message("This is a Unix system. Using zip.")
                            system(paste("zip -D plots %s",
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
  
  ##### When mapping elements update #####
  observeEvent(eventExpr = list(workspace$mapping_header_sf,
                                workspace$mapping_polygons),
               handlerExpr = {
                 message("Initializing a fresh map")
                 # Initialize the map
                 map <- leaflet::leaflet()
                 
                 # Add some basic info
                 map <- leaflet::addTiles(map = map)
                 
                 # Add the polygons
                 message("Checking to see if !is.null(workspace$mapping_polygons)")
                 if (!is.null(workspace$mapping_polygons)) {
                   message("!is.null(workspace$mapping_polygons) was TRUE")
                   # Note that we have to manually remove Z dimensions with sf::st_zm()
                   # otherwise if there's a Z dimension this fails with an
                   # inscrutable error.
                   map <- leaflet::addPolygons(map = map,
                                               data = sf::st_transform(x = sf::st_zm(workspace$mapping_polygons),
                                                                       crs = "+proj=longlat +datum=WGS84"),
                                               fillColor = "coral",
                                               stroke = FALSE,
                                               fillOpacity = 0.5)
                 }
                 
                 # Add in the retrieved points
                 message("Checking to see if !is.null(workspace$mapping_header_sf)")
                 if (!is.null(workspace$mapping_header_sf)) {
                   message("!is.null(workspace$mapping_header_sf) was TRUE")
                   map <- leaflet::addCircleMarkers(map = map,
                                                    data = sf::st_transform(x = workspace$mapping_header_sf,
                                                                            crs = "+proj=longlat +datum=WGS84"),
                                                    stroke = TRUE,
                                                    opacity = 0.9,
                                                    color = "white",
                                                    weight = 1,
                                                    fillColor = "gray20",
                                                    fillOpacity = 1,
                                                    radius = 3)
                 }
                 
                 message("Rendering map")
                 output$map <- leaflet::renderLeaflet(map)
                 message("Map rendered")
               })
}

# Run the application 
shinyApp(ui = ui, server = server)
