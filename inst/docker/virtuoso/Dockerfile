FROM debian:stretch

## consider removing build deps
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get -yq install \
    virtuoso-server

## This needs to be on the app container
## (Both the driver library and the config file)
RUN echo '[Local Virtuoso]\
          \nDriver=/usr/lib/x86_64-linux-gnu/odbc/virtodbc_r.so\
          \nAddress=localhost:1111' >> /etc/odbc.ini
EXPOSE 1111
CMD ["virtuoso-t", "-c", "/etc/virtuoso-opensource-6.1/virtuoso.ini", "-f"]

