FROM ubuntu:22.04

WORKDIR /app

HEALTHCHECK --interval=5m --timeout=10s \
  CMD curl -f http://localhost:3000/ || exit 1

# Install necessary libraries
RUN apt-get update \
 && apt-get -qq -y install libmemcached-dev ca-certificates netbase curl libpq5 libpq-dev

# Add built exectutable binary and run script
ADD wizard-server-bin /app/

# Add configs
ADD config/application.yml config/integration.yml config/build-info.yml /app/config/

# Setup non-root user
RUN groupadd --system --gid 10001 user \
 && useradd  --system --uid 10000 -g user user
USER user

# Set up env
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
ENV APPLICATION_CONFIG_PATH=/app/config/application.yml
ENV INTEGRATION_CONFIG_PATH=/app/config/integration.yml
ENV BUILD_INFO_PATH=/app/config/build-info.yml

# Run the run script (preparations and wizard-server-bin)
CMD ["/app/wizard-server-bin"]
