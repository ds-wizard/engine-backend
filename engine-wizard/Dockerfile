FROM ubuntu:20.04

WORKDIR /application

HEALTHCHECK --interval=5m --timeout=10s \
  CMD curl -f http://localhost:3000/ || exit 1

# Install necessary libraries
RUN apt-get update && apt-get -qq -y install libmemcached-dev ca-certificates netbase wget curl libpq5 libpq-dev

# Add built exectutable binary and run script
ADD engine-wizard-bin ./scripts/docker-run.sh /application/

# Add templates
ADD templates /application/engine-wizard/templates

# Add configs
ADD config/application.yml config/integration.yml config/build-info.yml config/localization.json /application/engine-wizard/config/

# Run the run script (preparations and engine-wizard-bin)
CMD ["bash", "./docker-run.sh"]
