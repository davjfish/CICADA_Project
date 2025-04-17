FROM rocker/geospatial:latest

COPY . /srv/shiny-server/cicada
WORKDIR /srv/shiny-server/cicada

RUN apt-get update && apt-get install -y

# Install R libraries
RUN Rscript -e 'remotes::install_version("bslib", upgrade = "never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("config", upgrade = "never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("DT", upgrade = "never", version = "0.31")'
RUN Rscript -e 'remotes::install_version("ggh4x", upgrade = "never", version = "0.2.8")'
RUN Rscript -e 'remotes::install_version("mapedit", upgrade = "never", version = "0.6.0")'
RUN Rscript -e 'remotes::install_version("patchwork", upgrade = "never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("shinyjs", upgrade = "never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinythemes", upgrade = "never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("shinycssloaders", upgrade = "never", version = "1.1.0")'

# prep data directory; when deploy in k8s this folder will be backended to a NAS containing all fst files
RUN mkdir /data
RUN chmod 0777 /data
RUN ln -s /data /srv/shiny-server/cicada/data

# expose container port 3838
EXPOSE 3838

# serve the application on startup
CMD ["Rscript", "-e", "options('shiny.port' = 3838, shiny.host = '0.0.0.0');shiny::runApp('App.R');"]
