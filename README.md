# A PostgreSQL sandbox

A [Nix](https://github.com/NixOS/nix)-based sandbox to play with the [Pagila](https://github.com/devrimgunduz/pagila) [example database](https://dev.mysql.com/doc/sakila/en/sakila-structure.html) using [`psql`](https://www.postgresql.org/docs/current/app-psql.html) and [`ghci`](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html).

The database will be created and initialized the first time we enter [`nix develop`](https://nix.dev/manual/nix/2.18/command-ref/new-cli/nix3-develop):

    postgresql-playground$ nix develop
    waiting for server to start.... done
    server started
    (dev) postgresql-playground$ 

Once within `nix develop`, we can connect using [`psql`](https://www.postgresql.org/docs/current/app-psql.html):

    (dev) postgresql-playground$ psql
    psql (16.6)
    Type "help" for help.

    pagila=# \d author

## The Cabal project

The repo contains a Haskell [Cabal](./postgresql-playground.cabal) [project](./cabal.project) to experiment with the [rel8 1.6.0.0](https://hackage.haskell.org/package/rel8) and [persistent](https://hackage.haskell.org/package/persistent) / [esqueleto 3.5.14.0](https://hackage.haskell.org/package/esqueleto-3.5.14.0) SQL DSLs.

The Cabal project must be built separately *while inside `nix develop`*:

    (dev) postgresql-playground$ cabal update
    (dev) postgresql-playground$ cabal build

### Playing with rel8

Start a `ghci` repl with module [PagilaRel8](./lib-rel8/PagilaRel8.hs) in scope:

    (dev) postgresql-playground$ cabal repl lib:pagila-rel8
    ghci> HasqlRun {hasqlRun, release'} <- acquire'
    ghci> paymentsByCustomer & select & Rel8.run & hasqlRun

You can also run the [postgresql-playground-rel8](https://github.com/danidiaz/postgresql-playground/blob/main/app-rel8/PagilaRel8Main.hs) executable that performs some example queries:

    (dev) postgresql-playground$ cabal run postgresql-playground-rel8

### Playing with esqueleto

Start a `ghci` repl with module [PagilaEsqueleto](./lib-esqueleto/PagilaEsqueleto.hs) in scope:

    (dev) postgresql-playground$ cabal repl lib:pagila-esqueleto
    ghci> selectSomeAddresses & run

You can also run the [postgresql-playground-esqueleto](https://github.com/danidiaz/postgresql-playground/blob/main/app-esqueleto/PagilaEsqueletoMain.hs) executable that performs some example queries:

    (dev) postgresql-playground$ cabal run postgresql-playground-esqueleto

## To delete the database

Exit `nix develop`, then delete the folders `.pg/` and `pg_sockets/`: 

    (dev) postgresql-playground$ exit
    $ rm -rf .pg/ .pg_sockets/

## Links

### Pagila

- [github](https://github.com/devrimgunduz/pagila)
- [Structure of the Salika database](https://dev.mysql.com/doc/sakila/en/sakila-structure.html). [tables](https://dev.mysql.com/doc/sakila/en/sakila-structure-tables.html). [views](https://dev.mysql.com/doc/sakila/en/sakila-structure-views.html).

### Haskell SQL DSLs

- [Haskell SQL DSLs, why do you use them?](https://www.reddit.com/r/haskell/comments/1ezj3il/haskell_sql_dsls_why_do_you_use_them/)
- [Haskell + SQLite - `SQLite.Simple`, or `esqueleto`, or something else?](https://www.reddit.com/r/haskell/comments/s4dnp7/haskell_sqlite_sqlitesimple_or_esqueleto_or/)
- [Zelenya—How to use PostgreSQL with Haskell](https://dev.to/zelenya/series/24889)

### Rel8

- [Rel8](https://github.com/circuithub/rel8)
- [official documentation](https://rel8.readthedocs.io/en/latest/)
- [on Hackage](https://hackage.haskell.org/package/rel8)

### Persistent / Esqueleto 

- [persistent](https://hackage.haskell.org/package/persistent) 
- [persistent-postgreql](https://hackage.haskell.org/package/persistent-postgresql) 
- [The Persistent tutorial in the Yesod book](https://www.yesodweb.com/book/persistent)
- [the Persistent entity syntax](https://hackage.haskell.org/package/persistent-2.14.6.3/docs/Database-Persist-Quasi.html)
- [esqueleto](https://hackage.haskell.org/package/esqueleto)
    - [Database.Esqueleto.Experimental](https://hackage.haskell.org/package/esqueleto-3.5.14.0/docs/Database-Esqueleto-Experimental.html)
- [Hachyderm post](https://hachyderm.io/@DiazCarrete/113810714496179726)

### Other stuff

- [Using PostgreSQL in a nix-shell](https://mgdm.net/weblog/postgresql-in-a-nix-shell/). 
- [Postgresql/Postgis inside nix-shell with sqitch and default postgres user](https://gist.github.com/gusmacaulay/9dc5793439750912458f3c6a8945de7d). 
- [Nix Recipe: Setup Postgresql](https://zeroes.dev/p/nix-recipe-for-postgresql/). 
- [Unable to setup postgres in nix-shell](https://discourse.nixos.org/t/unable-to-setup-postgres-in-nix-shell/14813/2). 
- [trap](https://www.ludovicocaldara.net/dba/bash-tips-7-cleanup-on-exit/)
- [About glibc-2.40](https://www.phoronix.com/news/GNU-C-Library-Glibc-2.40).

### Some psql commands

    explain (verbose true, format json) select actor_id, first_name from actor where actor_id = 1;
    prepare foostmt (integer) as select actor_id, first_name from actor where actor_id = $1;
    explain (verbose true, format json) execute foostmt(1);
    deallocate foostmt;
    
Interesting that it catches type errors in the prepared statement's parameters:

    foodb=# prepare foostmt2 (bytea) as select actor_id, first_name from actor where actor_id = $1;
    ERROR:  operator does not exist: integer = bytea
    LINE 1: ... select actor_id, first_name from actor where actor_id = $1;
                                                                    ^
    HINT:  No operator matches the given name and argument types. You might need to add explicit type casts.
    
### Explaining and describing queries

- [PREPARE](https://www.postgresql.org/docs/current/sql-prepare.html) and [DEALLOCATE](https://www.postgresql.org/docs/current/sql-deallocate.html)

- [EXPLAIN](https://www.postgresql.org/docs/current/sql-explain.html)

- [PQdescribePrepared](https://www.postgresql.org/docs/9.5/libpq-exec.html#LIBPQ-EXEC-SELECT-INFO)

  > Submits a request to obtain information about the specified prepared statement

  > On success, a PGresult with status PGRES_COMMAND_OK is returned. The functions PQnparams and PQparamtype can be applied to this PGresult to obtain information about the parameters of the prepared statement, and the functions PQnfields, PQfname, PQftype, etc provide information about the result columns (if any) of the statement.