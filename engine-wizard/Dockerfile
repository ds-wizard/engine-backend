FROM ubuntu:16.04

WORKDIR /application

HEALTHCHECK --interval=5m --timeout=10s \
  CMD curl -f http://localhost:3000/ || exit 1

# Install necessary libraries
RUN apt-get update && apt-get -qq -y install libmemcached-dev ca-certificates netbase wget gdebi-core curl
RUN wget https://github.com/wkhtmltopdf/wkhtmltopdf/releases/download/0.12.5/wkhtmltox_0.12.5-1.trusty_amd64.deb && gdebi -n wkhtmltox_0.12.5-1.trusty_amd64.deb
RUN wget https://github.com/jgm/pandoc/releases/download/2.7.2/pandoc-2.7.2-1-amd64.deb && gdebi -n pandoc-2.7.2-1-amd64.deb

# Add built exectutable binary
ADD engine-wizard-bin /application/engine-wizard-bin

# Create folder for configs & customization
RUN mkdir /application/engine-wizard

# Add templates
ADD templates /application/engine-wizard/templates

# Add configs
ADD config/application.yml /application/engine-wizard/config/application.yml
ADD config/integration.yml /application/engine-wizard/config/integration.yml
ADD config/build-info.yml /application/engine-wizard/config/build-info.yml
ADD config/localization.json /application/engine-wizard/config/localization.json

CMD ["./engine-wizard-bin"]
