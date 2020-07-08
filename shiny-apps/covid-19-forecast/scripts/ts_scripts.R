#########################
# Scripts for time series analysis:
# Trends and forecasting with ARIMA models
# Chanegpoint analysis with F statistics
#
# monica.golumbeanu@unibas.ch
#########################
library(scales)
library(tidyverse)
library(tseries)
library(forecast)
library(lubridate)

# Apply a 7-day-window smoothing to the time series data
ma = function(x, n = 7){
    ma_vec = stats::filter(x, rep(1 / n, n), sides = 1)
    return(ma_vec)
}

# Apply a 7-day-window smoothing to the time series data
ma_middle = function(x, n = 7){
    ma_vec = stats::filter(x, rep(1 / n, n), sides = 2)
    return(ma_vec)
}

# Apply smoothing to specified columns in a data frame
apply_ma = function(tab_df, col_names) {
    for (col_name in col_names) {
        tab_df[, col_name] = c(ma(tab_df[, col_name]))
    }
    return(as.data.frame(tab_df))
}

# Select best model
select_model = function(data_to_fit) {
    data_to_fit[which(data_to_fit <= 0)] = NA
    diff_order = 2
    comp_df = NULL
    for (p in 0:5) {
        for (q in 0:5) {
            arima_fit = Arima(data_to_fit, order = c(p, diff_order, q), 
                              optim.control = list(maxit=5000), lambda = 0,
                              biasadj=TRUE, method="ML")
            comp_df = rbind(comp_df, c(p, diff_order, q, arima_fit$bic, arima_fit$aic, 
                                       arima_fit$aicc, arima_fit$loglik))
        }
    }
    colnames(comp_df) = c("p", "d", "q", "BIC", "AIC", "AICc", "LL")
    comp_df = as.data.frame(comp_df)
    plot_df = comp_df[, c("p", "q", "AICc")]
    best_model_i = plot_df[which(plot_df$AICc == min(plot_df$AICc)),]
    best_model = Arima(data_to_fit, order = c(best_model_i$p, 2, best_model_i$q), 
                       method="ML", lambda = 0, biasadj=TRUE)
    return(best_model)
}

#####################################
# Fit an ARIMA model to a time series
# INPUTS: 
# ts_data = data frame where the first column is the time/date (in Date format) and the second
#               column is the corresponding measure (number of cases, hospitalisations, etc.)
# forecast_days = number of days to be forecasted, default is 1
# moving_avg = specify whether a moving average transformation should be applied or not, default is TRUE
# remove_last = specify whether the last data point should be removed or not, default is TRUE
# OUTPUTS:
# data = input time series data
# arima_model = fitted arima model
# arima_fits = model fits including uncertainty
# arima_forecast = forecasted time series
#####################################
fit_arima_model = function(ts_data, forecast_days = 1, moving_avg = TRUE, 
                           remove_last = TRUE, aa_opt = TRUE, acf_opt = FALSE, cumul = FALSE) {
    
    # Retain raw data
    raw_data = ts_data
    measure_name = colnames(raw_data)[2]
    print(measure_name)
    colnames(raw_data) = c("day_reported", measure_name)
    colnames(ts_data) = c("day_reported", measure_name)
    
    # Remove last entry (replace it with previous value) if specified
    if (remove_last) {
        ts_data[nrow(ts_data), 2] = ts_data[(nrow(ts_data)-1), 2]
    }
    
    # Apply a moving average if specified
    if(moving_avg) {
        ts_data[,2] = ma_middle(ts_data[,2])
        # Remove end dates with NA entries due to smoothing
        ts_data = ts_data[-c((nrow(ts_data)-2):nrow(ts_data)),]
        colnames(ts_data) = c("day_reported", "smoothed_value")
    }
    
    # Remove NA entries at the beginning
    ts_data = ts_data[which(ts_data[,2]>0),]
    # ts_data[which(is.na(ts_data[,2])),2] = 0
    
    # Calculate the day of the last prediction
    last_data = ts_data[nrow(ts_data), 1]
    last_prediction = last_data + forecast_days
    
    # Transform to additive time series
    # data_to_fit = log10(ts_data[,2])
    data_to_fit = ts_data[,2]
    
    # Check differencing order
    if(acf_opt) {
        d1_data_to_fit = diff(data_to_fit, differences = 1)
        acf(na.omit(d1_data_to_fit))
        print(adf.test(na.omit(d1_data_to_fit)))
    }
    
    # Fit an ARIMA model to the time series
    data_to_fit[which(!is.finite(data_to_fit))] = 0
    if(aa_opt) {
        # data_to_fit[which(data_to_fit <= 0)] = 1
        arima_model = auto.arima(data_to_fit, d = 2, stepwise=FALSE, 
                                 approximation = FALSE) #lambda = "auto"
    } else {
        arima_model = select_model(data_to_fit)
    }
    # print(arima_model)
    
    # fitted_all_df = cbind(ts_data[,1],
    #                       c(10^fitted(arima_model)) * c(ts_data[,2] > 0),
    #                       c(10^(fitted(arima_model) - sqrt(arima_model$sigma2))) * c(ts_data[,2] > 0),
    #                       c(10^(fitted(arima_model) + sqrt(arima_model$sigma2))) * c(ts_data[,2] > 0)) 
    # 
    if (aa_opt) {
        fitted_all_df = cbind(ts_data[,1],
                              fitted(arima_model),
                              pmax(0, fitted(arima_model) - 2*sqrt(arima_model$sigma2)),
                              fitted(arima_model) + 2*sqrt(arima_model$sigma2)) 
        
        colnames(fitted_all_df) = c("day_reported", "Value", "Low", "High")
    } else {
        fitted_all_df = cbind(ts_data[,1],
                              fitted(arima_model),
                              pmax(0, fitted(arima_model) - 2*sqrt(arima_model$sigma2)),
                              fitted(arima_model) + 2*sqrt(arima_model$sigma2)) 
        
        colnames(fitted_all_df) = c("day_reported", "Value", "Low", "High")
    }
    
    # Forecast
    pred_arima_all = as.data.frame(forecast(arima_model, h = forecast_days))
    forecast_times = seq.Date(as.Date(last_data) + 1, as.Date(last_prediction), by = "day")
    forecast_arima = cbind(forecast_times, pred_arima_all)#10.^
    colnames(forecast_arima) = c("day_reported", "Value", "Low", "High", "Low2", "High2")
    forecast_arima = forecast_arima[, c("day_reported", "Value", "Low", "High")]
    
    if (cumul) {
        forecast_arima$Low = pmax(0, pred_arima_all$`Point Forecast` - 2*sqrt(arima_model$sigma2))
    }
    
    # sim <- matrix(NA, ncol=forecast_days,nrow=1000)
    # for(i in 1:1000)
    #     sim[i,] <- simulate(arima_model, forecast_days)
    # se = apply(sim,2,sd)/sqrt(1000)
    # forecast_arima$High <- 10^(pred_arima_all$`Point Forecast` + 1.96*se)
    # forecast_arima$Low <- 10^(pred_arima_all$`Point Forecast` - 1.96*se)
    
    # forecast_arima$Low = 10^(pred_arima_all$`Point Forecast` - 1.96*sqrt(arima_model$sigma2))
    # forecast_arima$High = 10^(pred_arima_all$`Point Forecast` + 1.96*sqrt(arima_model$sigma2))
    # 
    
    # if (aa_opt) {
    #     forecast_arima$Low = pred_arima_all$`Point Forecast` - 2*sqrt(arima_model$sigma2)
    #     forecast_arima$High = pred_arima_all$`Point Forecast` + 2*sqrt(arima_model$sigma2)
    # } else { # TO DO: check exponentiation
    #     forecast_arima$Low = pmax(0, pred_arima_all$`Point Forecast` - 2*sqrt(arima_model$sigma2))
    #     forecast_arima$High = pmax(0, pred_arima_all$`Point Forecast` + 2*sqrt(arima_model$sigma2))
    # }

    # Fits and forecast
    all_data = rbind(fitted_all_df, forecast_arima)
    all_data$day_reported = c(ts_data[,1], forecast_arima$day_reported)
    
    # Results data table
    results_tab = Reduce(function(...) merge(..., by = colnames(ts_data)[1], all = TRUE), 
                         list(all_data, raw_data, ts_data))
    results_tab = cbind.data.frame(results_tab, measure_name) 
    colnames(results_tab) = c("day_reported", "model_fit", "model_fit_low", 
                              "model_fit_high", "data_value", "smoothed_value", "variable")
    
    return(list(arima_model = arima_model, 
                results_table = results_tab))
}

#####################################
# Fit an ARIMA model to a time series
# INPUTS: 
# ts_data = data frame where the first column is the time/date (in Date format) and the second
#               column is the corresponding measure (number of cases, hospitalisations, etc.)
# forecast_days = number of days to be forecasted, default is 1
# moving_avg = specify whether a moving average transformation should be applied or not, default is TRUE
# remove_last = specify whether the last data point should be removed or not, default is TRUE
# OUTPUTS:
# data = input time series data
# arima_model = fitted arima model
# arima_fits = model fits including uncertainty
# arima_forecast = forecasted time series
#####################################
fit_arima_model_date = function(ts_data, until_date, forecast_days = 1, moving_avg = TRUE, 
                                remove_last = TRUE) {
    
    # Retain raw data
    raw_data = ts_data
    measure_name = colnames(raw_data)[2]
    
    # Remove last entry (replace it with previous value) if specified
    if (remove_last) {
        ts_data[nrow(ts_data), 2] = ts_data[(nrow(ts_data)-1), 2]
    }
    
    # Apply a moving average if specified
    if(moving_avg) {
        ts_data[,2] = ma_middle(ts_data[,2])
        # Remove end dates with NA entries due to smoothing
        ts_data = ts_data[-c((nrow(ts_data)-2):nrow(ts_data)),]
        colnames(ts_data) = c("day_reported", "smoothed_value")
    }
    
    # Extract data to fit until the specified date and log transform to additive 
    last_ts_data = ts_data[nrow(ts_data), 1]
    if(until_date < last_ts_data) {
        ext_ts_data = ts_data[which(ts_data[,1] <= until_date),]
        data_to_fit = log10(ext_ts_data[,2])
    } else {
        ext_ts_data = ts_data
        data_to_fit = log10(ext_ts_data[,2])
    }
    
    # Calculate the day of the last prediction
    last_data = ext_ts_data[nrow(ext_ts_data), 1]
    print(last_data)
    last_prediction = last_data + forecast_days
    
    
    # Fit an ARIMA model to the time series
    data_to_fit[which(!is.finite(data_to_fit))] = 0
    arima_model = auto.arima(data_to_fit)
    # print(colnames(ts_data))
    # print(arima_model)
    fitted_all_df = cbind(ext_ts_data[,1],
                          c(10^fitted(arima_model)) * c(ext_ts_data[,2] > 0),
                          c(10^(fitted(arima_model) - 2*arima_model$sigma2))* c(ext_ts_data[,2] > 0),
                          c(10^(fitted(arima_model) + 2*arima_model$sigma2)) * c(ext_ts_data[,2] > 0)) 
    
    colnames(fitted_all_df) = c("day_reported", "Value", "Low", "High")
    
    # Forecast
    pred_arima_all = as.data.frame(forecast(arima_model, h = forecast_days))
    forecast_times = seq.Date(as.Date(last_data) + 1, as.Date(last_prediction), by = "day")
    forecast_arima = cbind(forecast_times, 10.^pred_arima_all)
    colnames(forecast_arima) = c("day_reported", "Value", "Low", "High", "Low2", "High2")
    forecast_arima = forecast_arima[, c("day_reported", "Value", "Low", "High")]
    
    # Fits and forecast
    all_data = rbind(fitted_all_df, forecast_arima)
    all_data$day_reported = c(ext_ts_data[,1], forecast_arima$day_reported)
    
    # Results data table, obtained by merging: model fits and forecasts, raw data, smoothed data
    results_tab = Reduce(function(...) merge(..., by = "day_reported", all = TRUE), 
                         list(all_data, raw_data, ts_data))
    results_tab = cbind.data.frame(results_tab, measure_name) 
    colnames(results_tab) = c("day_reported", "model_fit", "model_fit_low", 
                              "model_fit_high", "data_value", "smoothed_value", "variable")
    
    return(list(arima_model = arima_model, 
                results_table = results_tab))
}

###########################
# Run the ARIMA analysis for each time series (column) in a data frame
# and return the models
# INPUT
# ts_df = data frame where the first column is the time/date (in Date format) and 
#           each following column is a time series; the name of each time series is
#           specified in the column name
# n_days = number of days to do the forecast for
# OUTPUT
# list of all the model objects
###########################
run_arima_all = function(ts_df, n_days, ma_opt, remove_last, aa_opt, acf_opt, cumul) {
    # Remove the data source column if exists
    if (!is.null(ts_df$data_source)) {
        tab_data_source = unique(ts_df$data_source)
        ts_df = ts_df[, !(colnames(ts_df) == "data_source")]
    }
    
    # Run the arima analysis for each column
    result_list = vector('list', ncol(ts_df)-1)
    final_tab = NULL
    for (i in c(2:ncol(ts_df))){
        extracted_ts = ts_df[, c(1,i)]
        if (sum(extracted_ts[,2], na.rm = TRUE) > 0) {
            arima_analysis = fit_arima_model(extracted_ts, forecast_days = n_days, 
                                             moving_avg = ma_opt, 
                                             remove_last = remove_last,
                                             aa_opt = aa_opt, acf_opt = acf_opt, cumul = cumul) 
            res_tab = arima_analysis$results_table
            # res_tab$data_source = tab_data_source
            final_tab = rbind.data.frame(final_tab, res_tab)
        } 
    }
    return(final_tab)
}

#############################################
# Run the ARIMA analysis for each time series (column) in a data frame
# and return the models. The models are trained on data until a specified date
#############################################
run_arima_all_date = function(ts_df, last_date, n_days, ma_opt, remove_last) {
    # Run the arima analysis for each output measure and data source
    final_tab = NULL
    output_vars = unique(ts_df$variable)
    data_source_vars = unique(ts_df$data_source)
    for (o_var in output_vars) {
        for (ds_var in data_source_vars) {
            extracted_ts = ts_df[which(ts_df$variable == o_var &
                                           ts_df$data_source == ds_var), c("day_reported", "value")]
            if(nrow(extracted_ts) >0 ) {
                colnames(extracted_ts) = c("day_reported", o_var)
                arima_analysis = fit_arima_model_date(extracted_ts, until_date = last_date, 
                                                      forecast_days = n_days, moving_avg = ma_opt, 
                                                      remove_last = remove_last) 
                res_tab = arima_analysis$results_table
                res_tab$data_source = ds_var
                final_tab = rbind.data.frame(final_tab, res_tab)
            }
        }
    }
    return(final_tab)
}

######################
# Calculate error between the fitted model and true datavalues
#
#######################
calculate_model_error = function(ts_tab, s_date, h_p) {
    plot_df = NULL
    # Calculate moving average
    ts_tab[,2] = ma_middle(ts_tab[,2])
    ts_tab = ts_tab[-c((nrow(ts_tab)-2):nrow(ts_tab)),] 
    # Extract last date
    end_date = ts_tab[nrow(ts_tab)-1, 1]
    while (s_date <= (end_date-1)) {
        # Forecast day h_p
        # h_p = 1
        # Extract "training set"
        ts_tab_d = ts_tab[which(ts_tab$day_reported < (s_date - h_p + 1)), ]
        # Fit model
        arima_model = auto.arima(log10(ts_tab_d[, 2]))
        
        arima_forecast = forecast(arima_model, h = h_p)
        true_value = ts_tab[which(ts_tab[,1] == s_date - h_p + 1), 2]
        error_dif = abs(as.double(10^arima_forecast$mean[h_p]) - true_value)/true_value
        plot_df = c(plot_df, error_dif)
        s_date = s_date + 1
        print(s_date)
    }
    
    return(plot_df)
}
