FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages FIRST before copying app
RUN R -e "pkgs <- c('shiny','vroom','dplyr','janitor','bslib','shinyjs','plotly','waiter','tidyr','purrr','DT'); install.packages(pkgs, repos='http://cran.rstudio.com/')" && \
    rm -rf /tmp/downloaded_packages

# Copy app after packages are installed
COPY . /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]

