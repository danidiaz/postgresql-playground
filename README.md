# A PostgreSQL sandbox

A Nix-based sandbox to play with the [Pagila](https://github.com/devrimgunduz/pagila) [example database](https://dev.mysql.com/doc/sakila/en/sakila-structure.html) using [`psql`](https://www.postgresql.org/docs/current/app-psql.html) and [`ghci`](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html).

The database will be created and initialized the first time we enter [`nix-shell`](https://nixos.org/manual/nix/stable/command-ref/nix-shell.html):

    $ nix-shell
    [nix-shell]$

Once within `nix-shell`, we can connect using [`psql`](https://www.postgresql.org/docs/current/app-psql.html):

    [nix-shell]$ psql
    psql (14.6)
    Type "help" for help.

    pagila=#

Or using [`ghci`](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html) and the [`Rel8`](https://hackage.haskell.org/package/rel8) client library:

    [nix-shell]$ ghci Rel8Main.hs
    ghci> Right conn <- acquire ""
    ghci> each actorSchema & select & statement () & flip run conn

## To delete the database

Exit `nix-shell`, then delete the folders `.pg/` and `pg_sockets/`: 

    [nix-shell]$ exit
    $ rm -rf .pg .pg_sockets/

## Links

- [Using PostgreSQL in a nix-shell](https://mgdm.net/weblog/postgresql-in-a-nix-shell/). 
- [Postgresql/Postgis inside nix-shell with sqitch and default postgres user](https://gist.github.com/gusmacaulay/9dc5793439750912458f3c6a8945de7d). 
- [Nix Recipe: Setup Postgresql](https://zeroes.dev/p/nix-recipe-for-postgresql/). 
- [Unable to setup postgres in nix-shell](https://discourse.nixos.org/t/unable-to-setup-postgres-in-nix-shell/14813/2). 
- [trap](https://www.ludovicocaldara.net/dba/bash-tips-7-cleanup-on-exit/)

### Pagila

- [github](https://github.com/devrimgunduz/pagila)

- [Structure of the Salika database](https://dev.mysql.com/doc/sakila/en/sakila-structure.html). [tables](https://dev.mysql.com/doc/sakila/en/sakila-structure-tables.html). [views](https://dev.mysql.com/doc/sakila/en/sakila-structure-views.html).

### Rel8

- [Rel8](https://github.com/circuithub/rel8)
- [official documentation](https://rel8.readthedocs.io/en/latest/)
- [on Hackage](https://hackage.haskell.org/package/rel8)

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