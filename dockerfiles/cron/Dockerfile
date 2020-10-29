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
# Create the required files/folders for cron to work
RUN touch /var/log/cron.log && mkdir /root/scripts && chmod o+rwx /root/scripts
# Import the cron file
ADD sync_data /etc/cron.d/sync_data
# give the correct permissions so cron can run it
RUN chmod 0644 /etc/cron.d/sync_data
#
CMD cron && tail -f /var/log/cron.log
