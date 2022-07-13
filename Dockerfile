FROM landscapedatacommons/r-base:4.2.1
LABEL maintainer='Ken Ramsey <kramsey@jornada-vmail.nmsu.edu>'
# create Rprofile.site file in container
RUN echo "local({options(shiny.port = 3838, shiny.host = '0.0.0.0')})" > /usr/lib/R/etc/Rprofile.site
# change user
USER docker
# make app folder
RUN mkdir /home/docker/benchmark-exploration-tool
# copy app to image
COPY . /home/docker/benchmark-exploration-tool
# select port
EXPOSE 3838
# run app
#CMD ["R", "-e", "shiny::runApp('/benchmark-exploration-tool', host = '0.0.0.0', port = 3838)"]
CMD ["R", "-e", "shiny::runApp('/home/docker/benchmark-exploration-tool')"]
#CMD ["tail","-f", "/dev/null"]