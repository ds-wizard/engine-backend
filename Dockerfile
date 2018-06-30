FROM ubuntu:14.04

WORKDIR /dsw

# Install necessary libraries
RUN apt-get update && apt-get update && apt-get -qq -y install libmemcached-dev ca-certificates wkhtmltopdf

# Add built exectutable binary
ADD .stack-work/install/x86_64-linux/lts-9.11/8.0.2/bin/dsw-server /dsw/dsw-server

# Add configs
ADD config/app-config.cfg.example /dsw/config/app-config.cfg
ADD config/build-info.cfg /dsw/config/build-info.cfg

CMD ["./dsw-server"]
