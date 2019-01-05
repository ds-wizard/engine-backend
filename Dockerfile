FROM ubuntu:14.04

WORKDIR /dsw

# Install necessary libraries
RUN apt-get update && apt-get -qq -y install libmemcached-dev ca-certificates wget gdebi-core
RUN wget https://github.com/wkhtmltopdf/wkhtmltopdf/releases/download/0.12.5/wkhtmltox_0.12.5-1.trusty_amd64.deb && gdebi -n wkhtmltox_0.12.5-1.trusty_amd64.deb
RUN wget https://github.com/jgm/pandoc/releases/download/2.2.1/pandoc-2.2.1-1-amd64.deb && gdebi -n pandoc-2.2.1-1-amd64.deb

# Add built exectutable binary
ADD .stack-work/install/x86_64-linux/lts-12.0/8.4.3/bin/dsw-server /dsw/dsw-server

# Add configs
ADD config/app-config.cfg.example /dsw/config/app-config.cfg
ADD config/build-info.cfg /dsw/config/build-info.cfg
ADD template/root.css /dsw/template/root.css
ADD template/root.html.jinja /dsw/template/root.html.jinja

CMD ["./dsw-server"]
