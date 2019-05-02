FROM ubuntu:16.04

WORKDIR /dsw

HEALTHCHECK --interval=5m --timeout=10s \
  CMD curl -f http://localhost:3000/ || exit 1

# Install necessary libraries
RUN apt-get update && apt-get -qq -y install libmemcached-dev ca-certificates netbase wget gdebi-core curl
RUN wget https://github.com/wkhtmltopdf/wkhtmltopdf/releases/download/0.12.5/wkhtmltox_0.12.5-1.trusty_amd64.deb && gdebi -n wkhtmltox_0.12.5-1.trusty_amd64.deb
RUN wget https://github.com/jgm/pandoc/releases/download/2.7.2/pandoc-2.7.2-1-amd64.deb && gdebi -n pandoc-2.7.2-1-amd64.deb

# Add built exectutable binary
ADD .stack-work/install/x86_64-linux/lts-13.12/8.6.4/bin/dsw-server /dsw/dsw-server

# Add templates
ADD templates /dsw/templates

# Add configs
ADD config/application.yml /dsw/config/application.yml
ADD config/integration.yml /dsw/config/integration.yml
ADD config/build-info.yml /dsw/config/build-info.yml

CMD ["./dsw-server"]
