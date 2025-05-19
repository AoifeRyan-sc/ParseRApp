# Base image
FROM --platform=linux/amd64 rocker/shiny

# Install system libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libglpk-dev \
    libssl-dev && \
    apt-get clean
    
## app folder
COPY /app ./app
COPY renv.lock /renv.lock 

ARG PARSER_AUTH_KEY
ENV PARSER_AUTH_KEY=$PARSER_AUTH_KEY

# Install renv
RUN Rscript -e 'install.packages("renv")' &&\
    Rscript -e 'renv::restore(lockfile = "/renv.lock")' &&\
    Rscript -e 'install.packages("remotes")' &&\
    Rscript -e 'remotes::install_github("Avery-Island/ParseR", auth_token = Sys.getenv("PARSER_AUTH_KEY"))'

EXPOSE 3838

# Use the script as entrypoint
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]