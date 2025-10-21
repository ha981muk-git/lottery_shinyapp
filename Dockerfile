# Use an official R Shiny image
FROM rocker/shiny:latest

# System dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server/

# Copy renv files
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/ renv/

# FORCE renv to install without cache - inline override
RUN R -e "options(renv.config.cache.enabled = FALSE); renv::restore(prompt = FALSE)"

# Copy app files
COPY . .

# Fix permissions
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838
USER shiny

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=3838)"]