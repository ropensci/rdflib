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
    libiodbc2-dev \
    libsqlite3-dev \
    libmariadbclient-dev \
    librdf-storage-virtuoso \
	virtuoso-server

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
          \nDriver=/usr/lib/x86_64-linux-gnu/odbc/virtodbc.so\
          \nAddress=localhost:1111' >> /etc/odbc.ini


