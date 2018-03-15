FROM rocker/r-ver:latest

RUN apt-get update && apt-get -y install libpq-dev libdb-dev libxml2-dev libcurl4-openssl-dev libsqlite0-dev git automake libtool gtk-doc-tools bison flex glib-2.0 libgmp-dev  libmhash-dev libgcrypt20-dev

# libuuid mhash gcrypt pcre gmp mpfr 
#

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
    git clone git://github.com/dajobe/librdf.git && \
    cd librdf && \
    ./autogen.sh && \
    make && \
    make install

