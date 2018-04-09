FROM ccmi-elixir.cesnet.cz:5000/elixir/stack-hpack

WORKDIR /sources

ADD ./package.yaml /sources/package.yaml
ADD ./dsw-server.cabal /sources/dsw-server.cabal
ADD ./stack.yaml /sources/stack.yaml

RUN hpack
RUN stack --system-ghc build --only-dependencies

ADD ./app /sources/app
ADD ./config /sources/config
ADD ./scripts /sources/scripts
ADD ./lib /sources/lib
ADD ./test /sources/test
ADD ./Setup.hs /sources/Setup.hs
ADD ./LICENSE.md /sources/LICENSE.md

RUN mv ./config/app-config.cfg.example /sources/config/app-config.cfg

RUN stack --system-ghc build

ENTRYPOINT stack --system-ghc exec dsw-server
