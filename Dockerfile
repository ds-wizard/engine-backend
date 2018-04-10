FROM ubuntu:14.04

RUN apt-get update && apt-get update && apt-get -qq -y install libmemcached-dev

ADD .stack-work/install/x86_64-linux/lts-9.11/8.0.2/bin/dsw-server /dsw-server

CMD /dsw-server
