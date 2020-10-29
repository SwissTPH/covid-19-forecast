FROM rocker/tidyverse:3.6.1
# install some system dependencies required by the extra R libraries
# this might require some trial and error depending on the R package
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    cron 

# install some extra R libraries required by our shiny app
RUN install2.r readxl lubridate reshape2

RUN touch /var/log/cron.log && mkdir /root/scripts && cmmod o+rwx /root/scripts

ADD sync_data /etc/cron.d/sync_data

RUN chmod 0644 /etc/cron.d/sync_data

CMD cron && tail -f /var/log/cron.log
