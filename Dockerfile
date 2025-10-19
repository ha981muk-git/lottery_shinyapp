FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'vroom', 'dplyr', 'janitor', 'bslib', 'shinyjs', 'plotly', 'waiter', 'tidyr', 'purrr', 'DT'), repos='http://cran.rstudio.com/')"

# Copy your app to the container
COPY . /srv/shiny-server/

# Expose Shiny port
EXPOSE 3838

# Start Shiny server
CMD ["/usr/bin/shiny-server"]
