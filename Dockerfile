FROM rocker/shiny:3.6.1
# install some system dependencies required by the extra R libraries
# this might require some trial and error depending on the R package
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev

# install some extra R libraries required by our shiny app
RUN install2.r shinyjs ggplot2 DT png httr shiny zoo plotly plyr scales tidyverse tseries forecast lubridate strucchange shinyhelper

# this Dockerfile extends 
# https://github.com/rocker-org/shiny/blob/master/Dockerfile
