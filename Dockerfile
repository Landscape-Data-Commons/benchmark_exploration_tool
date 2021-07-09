# This Dockerfile is for the Benchmark Histograms tool
# Get image with R and Shiny preinstalled
FROM rocker/shiny:latest

# General system libraries that might be used
# I genuinely don't know which of these are necessary
# But they do show up in example Dockerfiles so they're here
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 

# Install required R packages
# These are just the app dependencies that aren't in the base install of R
# and aren't, to my knowledge, included in the image above
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('httr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('stringr', repos='http://cran.rstudio.com/')"

# Copy the app to the image
# There are only two files needed and we'll put them in /srv/benchmark-histograms:
# COPY app.R /srv/benchmark-histograms/
# COPY instructions.html /srv/benchmark-histograms/

# Select port so we can actually talk to the app
EXPOSE 3838
# This is an attempt to let the app make API calls over HTTPS
EXPOSE 443

# Allow permission
# No idea why this is here, it was inherited from an example
# It doesn't appear to be necessary but is here in case it needs to be reenabled later
# RUN sudo chown -R shiny:shiny /srv/benchmark-histograms

# Run app
# This makes sure that the app runs when the container is instanced
CMD ["R", "-e", "shiny::runApp('/srv/benchmark-histograms', host = '0.0.0.0', port = 3838)"]