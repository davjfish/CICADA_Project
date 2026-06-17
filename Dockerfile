FROM rocker/geospatial:4.4.3

WORKDIR /srv/shiny-server/cicada

# 2. Install system dependencies required by R packages (e.g., libcurl, openssl)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'install.packages("renv")'

# 4. Copy ONLY renv configuration files first to leverage Docker cache
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# 5. Change renv cache location to a local directory inside the container
ENV RENV_PATHS_CACHE=/srv/shiny-server/cicada/renv/cache

# prep data directory; when deploy in k8s this folder will be backended to a NAS containing all fst files
RUN mkdir /data
RUN chmod 0777 /data
RUN ln -s /data /srv/shiny-server/cicada/data

# 6. Restore the R environment (installs packages from renv.lock)
RUN R -e "renv::restore()"

# 7. Copy the actual Shiny app source code (changes frequently)
COPY . .

# expose container port 3838
EXPOSE 3838

# serve the application on startup
CMD ["Rscript", "-e", "options('shiny.port' = 3838, shiny.host = '0.0.0.0');shiny::runApp('App.R');"]
