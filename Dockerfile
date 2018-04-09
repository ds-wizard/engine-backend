FROM ubuntu:14.04

RUN apt-get update && apt-get update && apt-get -qq -y install libmemcached-dev

ADD .stack-work/install/x86_64-linux/lts-11.4/8.2.2/bin/dsw-server /my-exe

CMD /dsw-server
