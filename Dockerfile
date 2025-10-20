# Use an official R Shiny image
FROM rocker/shiny:latest

# System dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /srv/shiny-server/

# Copy app files
COPY . .

# Install R dependencies
RUN Rscript requirements.R

# Change ownership to the shiny user
RUN chown -R shiny:shiny /srv/shiny-server

# Expose the port (Render will override via PORT environment variable)
EXPOSE 3838

# Switch to the shiny user
USER shiny

# Run the Shiny app using the PORT environment variable
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '3838')))"]
