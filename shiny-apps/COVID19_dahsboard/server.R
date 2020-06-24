library(shiny)
library(zoo)
library(plotly)
library(shinyjs)
library(plyr)

source("scripts/plotting_scripts.R")
source("scripts/define_constants.R")
source("scripts/ts_scripts.R")
source("scripts/lag_analysis.R")

# Import the data scraped from BAG and corona-data.ch
scraped_data = read.table(file = "www/scraped_data.txt", sep = "\t", 
                          header = TRUE, stringsAsFactors = FALSE)
scraped_data$day_reported = ymd(scraped_data$day_reported)

# Define server logic
shinyServer(function(input, output, session) {
    
    plotheight = reactive({
        length(input$check_ts)
    })
    
    arima_res = reactive({
        run_arima_all_date(scraped_data, as.Date(input$date_model_train), 
                                       7, TRUE, TRUE)
    })
    
    lag_res = reactive({
        run_lag_analysis(arima_res()) 
    })
    
    # Plot the monitoring and forecasting plots
    output$ARIMA_analysis <- renderPlotly({
        # Render plots for the selected time series and data sources 
        if(!is.null(input$check_source) & !is.null(input$check_ts)) {
            figure = render_plots_ts(arima_res(), input$check_source, 
                                     input$check_ts, input$radio_disp,
                                     input$check_model, input$check_conf)
        } else {
            figure = NULL
        }
        # figure
        p3 = ggplotly(figure) 
        p4 = p3 %>% style(p3, showlegend = FALSE)
        p4
    })
    
    # Deactivate model uncertainty display option if no model is shown
    observeEvent(input$radio_model, {
        if(input$radio_model == "1"){
            updateCheckboxInput(session, "check_conf", value = FALSE)
            disable("check_conf")
        } else {
            enable("check_conf")
        }
    })
    
    observe_helpers(session = shiny::getDefaultReactiveDomain())
                    # help_dir = "helpfiles", withMathJax = FALSE)
    
    # Download data tables
    output$save_ts = downloadHandler(
        # arima_res = run_arima_all_date(scraped_data, as.Date(input$date_model_train),
        #                                7, TRUE, TRUE),
        filename = function() {paste("COVID-19_Dashboard_Data.csv")},
        content = function(file) {
            write.csv(arima_res(), file, row.names = FALSE)
        }
    )
    
    output$FOPH_text = renderText({"Swiss Federal Office of Public Health (FOPH)"})
    output$FOPH_text2 = renderText({"Swiss Federal Office of Public Health (FOPH)"})
    
    output$CORONA_text = renderText({"corona-data.ch"})
    output$CORONA_text2 = renderText({"corona-data.ch"})
    
    # Plot the monitoring and forecasting plots
    output$lag_analysis <- renderPlotly({
        # Render plots for the selected time series and data sources 
        if(!is.null(input$check_source) & !is.null(input$check_ts)) {
            figure = render_plot_breakpoints(data_table = arima_res(), 
                                             breakpoints_table = lag_res(), 
                                             o_data_disp = input$radio_disp,
                                             o_data_source = input$check_source, 
                                             o_ts = input$check_ts)
        } else {
            figure = NULL
        }
        # figure
        p3 = ggplotly(figure) 
        p4 = p3 %>% style(p3, showlegend = FALSE)
        p4
    })
    
})