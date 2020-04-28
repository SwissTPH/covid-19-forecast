## docker-letsencrypt-example

Example of a docker-compose config to deploy nginx reverse proxy with letsencrypt SSL certificates.

This repo includes two docker-compose.yml files:

### docker-compose-prod.yml

This is the compose file you would use in your production web server. The domain defined as `VIRTUAL_HOST` and `LETSENCRYPT_HOST` in this compose file  (`sosci-devel.scicore.unibas.ch`
in this example) should point to your production web server. It's also required that ports 80 and 443 in your web server are accessible from the internet. This
is mandatory so letsencrypt can connect to the webserver and validate the ssl certificates.

Once you have defined the vars `VIRTUAL_HOST` , `LETSENCRYPT_HOST` and `LETSENCRYPT_EMAIL` in the compose file you only need to run
```
docker-compose -f docker-compose-prod.yml up
```

or add the `-d` flag to leave it running in the background....

```
docker-compose -f docker-compose-prod.yml up -d
```

### docker-compose-devel.yml

This compose file won't try to request ssl certificates from letsencrypt. It will only boot a nginx reverse proxy and a webapp. You can use
this one if you only want to develop or test in your laptop or in a server that is not accessible from the internet (so letencrypt certs cannot
be requested). To use it:

```
docker-compose -f docker-compose-devel.yml up
```

### Dockerfile

This is the file that defines your R environment and required packages. During the build of the container you might have to tweak the required system packages.
```
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev
    # system packages go here
```

To add more R packages you can add them in the final RUN statement:

```
RUN install2.r shinyjs ggplot2 DT png httr 
```


## Official docs

For more details and configuration tweaks refer to the official docs of these containers:
  * [https://hub.docker.com/r/rocker/r-ver](https://hub.docker.com/r/rocker/r-ver)
