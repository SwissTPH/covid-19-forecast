########################
# Analysis of structural changes in time series to identify the lag between 
# measures and effects
#
# monica.golumbeanu@unibas.ch
########################

library(strucchange)

source("~/Documents/GitRepos/covid19_time_series/shinyApp/scripts/ts_scripts.R")

# Estimate the structural changes
estimate_breaks = function(data_table, measure_name) {
    ts_vec = (ma_middle(data_table[, measure_name]))
    bp = breakpoints(ts_vec ~ 1, breaks = 5) 
    ci_bp = as.data.frame(confint(bp)$confint)
    colnames(ci_bp) = c("low", "value", "high")
    return(list(bp = bp, ci_bp = ci_bp))
}

# Estimate the structural changes in a given time series
# INPUT: time series vector with the data values
# OUTPUT: data frame with the breakpoint positions and confidence intervals
estimate_breaks_ts = function(ts_vec, apply_ma = FALSE) {
    if(apply_ma) {
        ts_vec = (ma_middle(ts_vec))
    }
    
    bp = breakpoints(ts_vec ~ 1) 
    ci_bp = as.data.frame(confint(bp)$confint)
    colnames(ci_bp) = c("low", "value", "high")
    return(ci_bp)
}

# Estimate the structural changes for all time series vectors provided in a
# data frame
run_lag_analysis = function(ts_df) {
    # Run the arima analysis for each output measure and data source
    final_tab = NULL
    output_vars = unique(ts_df$variable)
    data_source_vars = unique(ts_df$data_source)
    for (o_var in output_vars) {
        for (ds_var in data_source_vars) {
            extracted_ts = ts_df[which(ts_df$variable == o_var &
                                           ts_df$data_source == ds_var), ]
            if(nrow(extracted_ts) >0 ) {
                breaks_res = estimate_breaks_ts(extracted_ts$smoothed_value)
                breaks_res$value = extracted_ts[breaks_res$value, "day_reported"]
                breaks_res$low = extracted_ts[breaks_res$low, "day_reported"]
                breaks_res$high = extracted_ts[breaks_res$high, "day_reported"]
                breaks_res$variable = o_var
                breaks_res$data_source = ds_var
                
                # Select breakpoints to show
                if (o_var == "curr_hosp") {
                    breaks_res = breaks_res[which(breaks_res$value > as.Date("2020-04-01") & 
                                                      breaks_res$value < as.Date("2020-04-15")),]
                } else if (o_var == "cases") {
                    breaks_res = breaks_res[which(breaks_res$value > as.Date("2020-03-17") & 
                                                      breaks_res$value < as.Date("2020-04-05")),]
                } else if (o_var == "deaths") {
                    breaks_res = breaks_res[which(breaks_res$value > as.Date("2020-04-01") & 
                                                      breaks_res$value < as.Date("2020-04-15")),]
                } else if (o_var == "hospitalizations") {
                    breaks_res = breaks_res[which(breaks_res$value > as.Date("2020-03-20") & 
                                                      breaks_res$value < as.Date("2020-04-01")),]
                }
                
                final_tab = rbind.data.frame(final_tab, breaks_res)
            }
        }
    }
    return(final_tab)
}

plot_breakpoints = function(data_table, measure_name, date_bp,
                            break_points, plot_title) {
    
    bp_vec = break_points$bp$breakpoints
    select_tp = which(data_tab[bp_vec, "day_reported"] > date_bp)
    trans_data = cbind.data.frame(data_table$day_reported, ma_middle(data_table[, measure_name]))
    colnames(trans_data) = c("day_reported", measure_name)
    p = ggplot() + 
        geom_rect(aes(xmin=data_tab[break_points$ci_bp$low[select_tp], "day_reported"], 
                                      xmax=data_tab[break_points$ci_bp$high[select_tp], "day_reported"], 
                                      ymin=-Inf, ymax=Inf), fill="#0868ac", alpha=0.3) +
        geom_point(data = data_table, aes_string(x = "day_reported", y = measure_name), color = "grey") +
        geom_line(data = trans_data, aes_string(x = "day_reported", y = measure_name), color = "grey", lwd = 0.5) +
        geom_vline(xintercept = data_tab[bp_vec[select_tp], "day_reported"], lty = 2) +
        # geom_vline(xintercept = data_tab[break_points$ci_bp$low[select_tp], "day_reported"], lty = 2, color = "#0868ac") +
        # geom_vline(xintercept = data_tab[break_points$ci_bp$high[select_tp], "day_reported"], lty = 2, color = "#0868ac") +
        geom_vline(xintercept = as.Date("2020-03-17"), lty = 1, color = "#fb6a4a") + theme_bw() +
        labs( x = "Time", y = "", title = plot_title)  
    return(p)
}

# # Analysis with the corona-data.ch
# data_tab = get_CORONA_table()
# # For cases
# est_bp = estimate_breaks(data_tab, "cases")
# p_cases = plot_breakpoints(data_tab, "cases", as.Date("2020-03-25"),
#                             est_bp, "Daily reported cases")
# # For deaths
# est_bp = estimate_breaks(data_tab, "deaths")
# p_deaths = plot_breakpoints(data_tab, "deaths", as.Date("2020-03-25"),
#                            est_bp, "Daily reported deaths")
# fig_corona = ggarrange(p_cases, p_deaths, ncol = 2, nrow = 1)
# ggsave("../output_figures/lag_corona.pdf", width = 6, height = 3)
# 
# # Analysis with the BAG data
# data_tab = get_BAG_table()
# # For cases
# est_bp = estimate_breaks(data_tab, "cases")
# p_cases = plot_breakpoints(data_tab, "cases", as.Date("2020-03-25"),
#                            est_bp, "Daily reported cases")
# # For deaths
# est_bp = estimate_breaks(data_tab, "deaths")
# p_deaths = plot_breakpoints(data_tab, "deaths", as.Date("2020-03-25"),
#                             est_bp, "Daily reported deaths")
# fig_bag = ggarrange(p_cases, p_deaths, ncol = 2, nrow = 1)
# ggsave("../output_figures/lag_bag.pdf", width = 6, height = 3)
# 
