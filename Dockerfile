# Use an official R Shiny image (includes R + Shiny Server)
FROM rocker/shiny:latest

# Install system dependencies needed by many R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    libjpeg-dev \
    libfontconfig1-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /srv/shiny-server/

# Copy renv configuration files first (for Docker layer caching)
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/ renv/

# Force renv to install all packages fresh (no cache)
RUN R -e "options(renv.config.cache.enabled = FALSE); renv::restore(prompt = FALSE)"

# As a safety net: ensure shiny + yaml are installed (in case renv.lock doesn’t include them)
RUN R -e "if (!requireNamespace('shiny', quietly = TRUE)) install.packages('shiny', repos='https://cloud.r-project.org'); \
          if (!requireNamespace('yaml', quietly = TRUE)) install.packages('yaml', repos='https://cloud.r-project.org')"

# Copy the rest of the app files
COPY . .

# Fix permissions for the 'shiny' user
RUN chown -R shiny:shiny /srv/shiny-server

# Expose port for Shiny apps
EXPOSE 3838

# Switch to the 'shiny' user (for security)
USER shiny

# Launch the app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=3838)"]
