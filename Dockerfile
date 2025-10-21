# Use an official R Shiny image (includes R + Shiny Server)
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    libjpeg-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libtiff5-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /srv/shiny-server/

# Copy renv files first (to use Docker caching)
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/ renv/


# Just remove options(renv.config.cache.enabled = FALSE) or options(renv.config.cache.enabled = TRUE) after first deploy. Next time you deploy:
# Docker reuses the installed packages from the cached layer.
# Only your app files are copied → next deploy is almost instant.

# Restore dependencies (from lockfile)
RUN R -e "if (file.exists('renv.lock')) { \
            options(renv.config.cache.enabled = TRUE); \
            renv::restore(prompt = FALSE); \
          }"

# Install fallback packages if missing
RUN R -e "pkgs <- c('shiny', 'vroom', 'dplyr', 'DT', 'janitor', 'plotly', 'purrr', 'shinyjs', 'tidyr', 'waiter', 'stringr', 'zoo', 'bslib', 'data.table', 'ggplot2', 'readr', 'lubridate', 'yaml', 'rsconnect'); \
          for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos='https://cloud.r-project.org');"

# Copy all app files
COPY . .

# Fix permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose port
EXPOSE 3838

# Switch user
USER shiny

# Run the app
CMD ["R", "-e", "options(shiny.maxRequestSize=30*1024^2); shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=3838)"]
