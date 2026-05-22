# Base: R in a Debian Linux environment
FROM rocker/geospatial:4.6.0

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    make \
    pandoc \
    cmake \
    libuv1-dev \
    gdal-bin \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libsqlite3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libv8-dev \
    libpng-dev \
    libjpeg-dev \
    libabsl-dev \
    zlib1g-dev \
    librdf0-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /usr/src/app

RUN R -e 'install.packages("renv", repos="https://cloud.r-project.org")'
# Install renv and restore packages

# Copy renv.lock and renv folder first for caching
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
COPY .Rprofile .Rprofile

# Restore packages
RUN R -e "renv::restore(lockfile='renv.lock', prompt=FALSE, clean=TRUE)"

# Copy all R files
COPY app/ ./app/
COPY data/ ./data/
COPY app.R dependencies.R rhino.yml config.yml ./
# Expose Shiny port
EXPOSE 3838

# Default command: run the Shiny app

CMD ["R", "-e", "options(shiny.host='0.0.0.0', shiny.port=3838); shiny::runApp('/usr/src/app')"]