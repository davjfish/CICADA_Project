FROM rocker/geospatial:latest

COPY . /srv/shiny-server/cicada
WORKDIR /srv/shiny-server/cicada

RUN apt-get update && apt-get install -y

RUN Rscript -e 'remotes::install_version("abind", upgrade = "never", version = "1.4-5")'
RUN Rscript -e 'remotes::install_version("data.table", upgrade = "never", version = "1.17.0")'
RUN Rscript -e 'remotes::install_version("DT", upgrade = "never", version = "0.29")'
RUN Rscript -e 'remotes::install_version("htmltools", upgrade = "never", version = "0.5.8.1")'
RUN Rscript -e 'remotes::install_version("leafem", upgrade = "never", version = "0.2.3")'
RUN Rscript -e 'remotes::install_version("leaflet", upgrade = "never", version = "2.2.0")'
RUN Rscript -e 'remotes::install_version("readxl", upgrade = "never", version = "1.4.3")'
RUN Rscript -e 'remotes::install_version("sf", upgrade = "never", version = "1.0-12")'
RUN Rscript -e 'remotes::install_version("shiny", upgrade = "never", version = "1.7.5")'
RUN Rscript -e 'remotes::install_version("shinycssloaders", upgrade = "never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("shinyjs", upgrade = "never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinythemes", upgrade = "never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("stars", upgrade = "never", version = "0.6-1")'
RUN Rscript -e 'remotes::install_version("stringr", upgrade = "never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("tidyverse", upgrade = "never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("viridisLite", upgrade = "never", version = "0.4.2")'



# prep data directory; when deploy in k8s this folder will be backended to a NAS containing all fst files
RUN mkdir /data
RUN chmod 0777 /data
RUN ln -s /data /srv/shiny-server/cicada/data

# expose container port 3838
EXPOSE 3838

# serve the application on startup
CMD ["Rscript", "-e", "options('shiny.port' = 3838, shiny.host = '0.0.0.0');shiny::runApp('App.R');"]
