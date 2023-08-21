FROM ubuntu:22.04

WORKDIR /app

HEALTHCHECK --interval=5m --timeout=10s \
  CMD curl -f http://localhost:3000/ || exit 1

# Install necessary libraries
RUN apt-get update && apt-get -qq -y install libmemcached-dev ca-certificates netbase wget gdebi-core curl libpq5 libpq-dev

# Add built exectutable binary
ADD registry-server-bin /app/

# Create folder for configs & customization
RUN mkdir /app/registry-server

# Add configs
ADD config/application.yml config/build-info.yml /app/config/

# Setup non-root user
RUN groupadd -r user && useradd -r -g user user
USER user

# Set up env
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
ENV APPLICATION_CONFIG_PATH=/app/config/application.yml
ENV BUILD_INFO_PATH=/app/config/build-info.yml

# Run the run script (preparations and wizard-server-bin)
CMD ["/app/registry-server-bin"]
