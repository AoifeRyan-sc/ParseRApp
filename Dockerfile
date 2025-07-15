# Base image
FROM rocker/r-ver:4.4.3

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
COPY deploy.R deploy.R
COPY renv.lock /renv.lock 

ARG PARSER_AUTH_KEY
ENV PARSER_AUTH_KEY=$PARSER_AUTH_KEY

# Install renv
RUN Rscript -e 'install.packages("renv")' &&\
    Rscript -e 'install.packages("rsconnect")' &&\
    Rscript -e 'renv::restore(lockfile = "/renv.lock")' &&\
    Rscript -e 'install.packages("remotes")' &&\
    Rscript -e 'remotes::install_github("Avery-Island/ParseR", auth_token = Sys.getenv("PARSER_AUTH_KEY"))'

# Use the script as entrypoint
CMD Rscript deploy.R