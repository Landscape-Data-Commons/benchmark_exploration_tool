FROM landscapedatacommons/shinyproxy:4.0.2
COPY . /srv/shiny-server
CMD R -e "shiny::runApp('/srv/shiny-server/benchmark_histograms', host = '0.0.0.0', port = 3838)"
#CMD ["tail","-f", "/dev/null"]