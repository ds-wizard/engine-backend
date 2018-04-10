FROM ubuntu:14.04

# Install necessary libraries
RUN apt-get update && apt-get update && apt-get -qq -y install libmemcached-dev

# Add built exectutable binary
ADD .stack-work/install/x86_64-linux/lts-9.11/8.0.2/bin/dsw-server /dsw/dsw-server

# Add configs
ADD config/app-config.cfg /dsw/config/app-config.cfg
ADD config/app-config-test.cfg /dsw/config/app-config-test.cfg
ADD config/build-info.cfg /dsw/config/build-info.cfg
ADD config/build-info-test.cfg /dsw/config/build-info-test.cfg

CMD /dsw/dsw-server
