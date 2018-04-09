FROM ccmi-elixir.cesnet.cz:5000/elixir/stack-hpack

WORKDIR /sources

ADD ./app /sources/app
ADD ./config /sources/config
ADD ./scripts /sources/scripts
ADD ./lib /sources/lib
ADD ./test /sources/test
ADD ./dsw-server.cabal /sources/dsw-server.cabal
ADD ./package.yaml /sources/package.yaml
ADD ./Setup.hs /sources/Setup.hs
ADD ./LICENSE.md /sources/LICENSE.md
ADD ./stack.yaml /sources/stack.yaml

RUN mv ./config/app-config.cfg.example /sources/config/app-config.cfg

RUN hpack
RUN stack --system-ghc  build
RUN stack --system-ghc install

ENTRYPOINT stack --system-ghc exec dsw-server
