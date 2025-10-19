# Use Rocker Shiny image
FROM rocker/shiny:latest

# Install any system dependencies you need (example: libcurl)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Copy your app to the container
COPY . /srv/shiny-server/

# Expose Shiny port
EXPOSE 3838

# Start Shiny server
CMD ["/usr/bin/shiny-server"]

