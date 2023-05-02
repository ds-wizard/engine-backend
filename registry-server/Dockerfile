FROM ubuntu:22.04

WORKDIR /application

HEALTHCHECK --interval=5m --timeout=10s \
  CMD curl -f http://localhost:3000/ || exit 1

# Install necessary libraries
RUN apt-get update && apt-get -qq -y install libmemcached-dev ca-certificates netbase wget gdebi-core curl libpq5 libpq-dev

# Add built exectutable binary
ADD registry-server-bin /application/

# Create folder for configs & customization
RUN mkdir /application/registry-server

# Add configs
ADD config/application.yml /application/registry-server/config/application.yml
ADD config/build-info.yml /application/registry-server/config/build-info.yml

# Setup non-root user
RUN groupadd -r user && useradd -r -g user user
USER user

# Set up env
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
ENV APPLICATION_CONFIG_PATH=/app/config/application.yml
ENV BUILD_INFO_PATH=/app/config/build-info.yml

# Run the run script (preparations and wizard-server-bin)
CMD ["/application/registry-server-bin"]
