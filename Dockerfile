FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy your app and install.R
COPY . /srv/shiny-server/
COPY install.R /install.R

# Install R packages
RUN Rscript /install.R

# Expose Shiny port
EXPOSE 3838

# Start Shiny server
CMD ["/usr/bin/shiny-server"]

