FROM ubuntu:22.04

WORKDIR /application

HEALTHCHECK --interval=5m --timeout=10s \
  CMD curl -f http://localhost:3000/ || exit 1

# Install necessary libraries
RUN apt-get update && apt-get -qq -y install libmemcached-dev ca-certificates netbase wget gdebi-core curl libpq5 libpq-dev

# Add built exectutable binary
ADD engine-registry-bin /application/

# Create folder for configs & customization
RUN mkdir /application/engine-registry

# Add configs
ADD config/application.yml /application/engine-registry/config/application.yml
ADD config/build-info.yml /application/engine-registry/config/build-info.yml
ADD config/localization.json /application/engine-registry/config/localization.json

# Setup non-root user
RUN groupadd -r user && useradd -r -g user user
USER user

# Set up env
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8

# Run the run script (preparations and engine-wizard-bin)
CMD ["/application/engine-registry-bin"]
