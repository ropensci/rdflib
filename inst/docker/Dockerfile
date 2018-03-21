FROM rocker/tidyverse:latest

## consider removing build deps
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get -yq install \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    git \
    automake \
    libtool \
    gtk-doc-tools \
    bison \
    flex \
    libgmp-dev  \
    libmhash-dev \
    libgcrypt20-dev \
    libpcre3-dev \
    libv8-dev \
    libjq-dev \
    libpq-dev \
    libdb-dev \
    libsqlite3-dev \
    libmariadbclient-dev \
    librdf-storage-virtuoso \
	virtuoso-server \
	unixodbc-dev

RUN git clone git://github.com/dajobe/raptor.git && \
    cd raptor && \
    ./autogen.sh && \
    make && \
    make install && \
    cd .. && \
    git clone git://github.com/dajobe/rasqal.git && \
    cd rasqal && \
    ./autogen.sh && \
    make && \
    make install && \
    cd .. && \
    git clone git://github.com/dajobe/librdf.git && \
    cd librdf && \
    ./autogen.sh && \
    make && \
    make install

RUN R -e "install.packages('remotes')" && \
    R -e "remotes::install_github('ropensci/redland-bindings/R/redland')" && \
    R -e "remotes::install_github('ropensci/rdflib', dep=TRUE)"

RUN echo '[Local Virtuoso]\
          \nDriver=/usr/lib/x86_64-linux-gnu/odbc/virtodbc_r.so\
          \nAddress=localhost:1111' >> /etc/odbc.ini

RUN mkdir -p /etc/services.d/virtuoso \
  && echo '#!/usr/bin/with-contenv bash \
           \n exec virtuoso-t -c /etc/virtuoso-opensource-6.1/virtuoso.ini' \
           > /etc/services.d/virtuoso/run


