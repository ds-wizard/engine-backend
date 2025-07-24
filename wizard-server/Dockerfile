FROM ubuntu:22.04

WORKDIR /app

HEALTHCHECK --interval=5m --timeout=10s \
  CMD curl -f http://localhost:3000/ || exit 1

# Install necessary libraries
RUN apt-get update \
 && apt-get -qq -y install libmemcached-dev ca-certificates netbase curl libpq5 libpq-dev python3-dev \
 && rm -rf /var/lib/apt/lists/*

# Set up env
ENV LC_ALL=C.UTF-8 \
    LANG=C.UTF-8 \
    APPLICATION_CONFIG_PATH=/app/config/application.yml \
    INTEGRATION_CONFIG_PATH=/app/config/integration.yml \
    BUILD_INFO_PATH=/app/config/build-info.yml \
    PYTHONPATH="/app/lib:$PYTHONPATH" \
    LD_LIBRARY_PATH="/app/lib:$LD_LIBRARY_PATH"

# Add dist folder with libraries, executable, and configs
ADD dist /app/

# Setup non-root user
RUN groupadd --system --gid 10001 user \
 && useradd  --system --uid 10000 -g user user
USER user

# Run the run script (preparations and wizard-server-bin)
CMD ["/app/wizard-server-bin"]
