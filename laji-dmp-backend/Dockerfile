FROM haskell:9.10.1-slim-bullseye

WORKDIR /app/dmp

COPY . .

RUN apt-get update
RUN apt-get install -y liblzma-dev liblzma5 pkg-config curl ca-certificates libpq-dev
RUN install -d /usr/share/postgresql-common/pgdg
RUN curl -o /usr/share/postgresql-common/pgdg/apt.postgresql.org.asc --fail https://www.postgresql.org/media/keys/ACCC4CF8.asc
RUN . /etc/os-release
RUN sh -c "echo 'deb [signed-by=/usr/share/postgresql-common/pgdg/apt.postgresql.org.asc] https://apt.postgresql.org/pub/repos/apt bullseye-pgdg main' > /etc/apt/sources.list.d/pgdg.list"

RUN apt-get update
RUN apt-get install -y postgresql-14

ENV CABAL_DIR=/cabal

RUN cabal update
RUN cabal build

RUN chgrp -R 0 /app/dmp && chmod -R g+rwX /app/dmp

CMD ["cabal", "run"]