library(shiny)
library(plotly)
library(shinyhelper)

# Retrieve the date of the last file change (date of the last data point)
current_date = file.info("www/scraped_data.txt")$mtime

fluidPage(tags$head(HTML("<title>COVID-19 Forecast</title>")),
          shinyjs::useShinyjs(),
          navbarPage(title = div(img(src="coronaLogo.png", 
                                 height = 29, width = 34),
                             "COVID-19 Forecast"), inverse=TRUE,
                   tabPanel("Home", style='width: 1300px;',
                            fluidRow(align = "right",
                                     tags$img(src="logo_TPH.png", width = 97.8, height = 28.114),
                                     tags$img(src="logo_UniBas.jpg", width = 93.8, height = 30.8,
                                              style="margin-left: 25px;")),
                            titlePanel(h3("Live time-series analysis to monitor and forecast the COVID-19 outbreak in Switzerland")), 
                            p("This resource provides real-time analysis of time series data describing the COVID-19 outbreak in Switzerland
                               using statistical models to interpret trends and forecast short-term development."),
                            tags$br(),
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(
                                # Sidebar panel for inputs ----
                                sidebarPanel(width = 2, height = 10, 
                                    helper(tags$br(), icon = "question-circle",
                                           colour = "black",
                                           type = "markdown",
                                           content = "ActionTabHelp"),
                                    h5("Visualization settings:"),
                                    hr(style="background-color: black; height: 1px; border: 0;"),
                                    # wellPanel()
                                    checkboxGroupInput("check_source", label = "Data sources",
                                                       choices = list("FOPH" = 1, 
                                                                      "corona-data.ch" = 2),
                                                       selected = c(1, 2)),
                                    checkboxGroupInput("check_ts", label = "Displayed data outputs",
                                                           choices = list("New cases" = 1, 
                                                                          "New hospitalizations" = 2,
                                                                          "Current hospitalizations" = 3,
                                                                          "New fatalities" = 4),
                                                           selected = c(1,4)),
                                    radioButtons("radio_disp", label = "Data smoothing",
                                                     choices = list("Raw data" = 1, 
                                                                    "Smoothed" = 2), 
                                                     selected = 1),
                                    h5(strong("Time series model:")),
                                    checkboxInput("check_model", label = "Show model", value = TRUE),
                                    checkboxInput("check_conf", label = "Show model uncertainty", value = TRUE),
                                    dateInput("date_model_train", label = h5("Training until date:"), value = current_date),
                                    hr(),
                                    downloadButton("save_ts", label = "Download data")
                                ),
                                
                                # Main panel for displaying outputs ----
                                mainPanel(width = 8,
                                    tabsetPanel(id = "analysis_tabs",
                                                tabPanel('Monitoring and forecasting',
                                                    helper(tags$br(), icon = "question-circle",
                                                        colour = "black",
                                                        type = "markdown",
                                                        content = "PlotTabHelp_arima"),
                                                    p("The figures below display the results of time series analyses
                                                      performed on data provided by the ", 
                                                      span(textOutput("FOPH_text", inline = TRUE), 
                                                           style = "color:#fb6a4a"), 
                                                        "and data individually released by the Swiss cantons 
                                                            aggregated by",
                                                      span(textOutput("CORONA_text", inline = TRUE), 
                                                           style = "color:#42B3D5"), "."),
                                                    p(paste("Last data update:", current_date)),
                                                    plotlyOutput(outputId = "ARIMA_analysis")
                                                ),
                                                tabPanel('Effect delays',
                                                      helper(tags$br(), icon = "question-circle",
                                                                colour = "black",
                                                                type = "markdown",
                                                                content = "PlotTabHelp_lags"),
                                                      p("The figures below display the results of time series analyses
                                                      performed on data provided by the ", 
                                                           span(textOutput("FOPH_text2", inline = TRUE), 
                                                                style = "color:#fb6a4a"), 
                                                           "and data individually released by the Swiss cantons 
                                                           aggregated by",
                                                           span(textOutput("CORONA_text2", inline = TRUE), 
                                                                style = "color:#42B3D5"), "."),
                                                         p(paste("Last data update:", current_date)),
                                                      
                                                     plotlyOutput(outputId = "lag_analysis"),
                                                     
                                                     p("Estimated delays in the effects of measures:"),
                                                     p("14 - 16 days before we observe effects on confirmed cases"),
                                                     p("23 - 25 days before we observe effects on numbers of deaths")
                                                )
                                    )
                                )
                            )
                   ),
                   # tabPanel("Help", style='width: 1300px;',
                   #          fluidRow(align = "right",
                   #                   tags$img(src="logo_TPH.png", width = 97.8, height = 28.114),
                   #                   tags$img(src="logo_UniBas.jpg", width = 93.8, height = 30.8,
                   #                            style="margin-left: 25px;"))
                   # ),
                   tabPanel("Methods", style='width: 1300px;',
                            fluidRow(align = "right",
                                     tags$img(src="logo_TPH.png", width = 97.8, height = 28.114),
                                     tags$img(src="logo_UniBas.jpg", width = 93.8, height = 30.8,
                                              style="margin-left: 25px;")),
                            titlePanel(h3("COVID-19 Dashboard: Live time-series analysis 
                                          to monitor and forecast the COVID-19 outbreak in Switzerland")), 
                            p("Description of data sources, 
                              statistical methods and displayed results", style = "color:grey"),
                            hr(),
                            withMathJax(includeMarkdown("www/Methods.Rmd"))
                   ),
                   tabPanel("About", style='width: 1300px;',
                            fluidRow(align = "right",
                                     tags$img(src="logo_TPH.png", width = 97.8, height = 28.114),
                                     tags$img(src="logo_UniBas.jpg", width = 93.8, height = 30.8,
                                              style="margin-left: 25px;")),
                            titlePanel(h3("COVID-19 Dashboard: Live time-series analysis 
                                          to monitor and forecast the COVID-19 outbreak in Switzerland")), 
                            p("Authors", style = "color:grey"),
                            hr(),
                            includeMarkdown("www/About.Rmd")
                   )
))