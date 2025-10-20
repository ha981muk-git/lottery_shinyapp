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

# Copy renv files first (for better caching)
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
# COPY renv/settings.json renv/settings.json

# Repair and restore R dependencies
RUN R -e "renv::repair()" && \
    R -e "renv::restore(prompt = FALSE)" && \
    R -e "renv::status()"

# Copy app files
COPY lottery_shinyapp/ .
COPY . .

# Change ownership to the shiny user BEFORE switching users
RUN chown -R shiny:shiny /srv/shiny-server

# Expose the port
EXPOSE 3838

# Switch to the shiny user
USER shiny

# Run as single app (better for debugging and Render)
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=3838)"]