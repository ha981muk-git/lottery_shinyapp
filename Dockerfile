# Use an official R Shiny image
FROM rocker/shiny:latest

# System dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy app files
WORKDIR /srv/shiny-server/
COPY . .

# Install R dependencies
RUN Rscript requirements.R

# Expose port
# Expose port
EXPOSE 3838
# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]



# Note: The PORT environment variable is set to 10000 by default if not provided.


