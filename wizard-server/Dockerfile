FROM ubuntu:22.04

WORKDIR /application

HEALTHCHECK --interval=5m --timeout=10s \
  CMD curl -f http://localhost:3000/ || exit 1

# Install necessary libraries
RUN apt-get update && apt-get -qq -y install libmemcached-dev ca-certificates netbase wget curl libpq5 libpq-dev

# Add built exectutable binary and run script
ADD wizard-server-bin /application/

# Add configs
ADD config/application.yml config/integration.yml config/build-info.yml /application/wizard-server/config/

# Setup non-root user
RUN groupadd -r user && useradd -r -g user user
USER user

# Set up env
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8

# Run the run script (preparations and wizard-server-bin)
CMD ["/application/wizard-server-bin"]
