# PostgreSQL & nix-shell

- [Using PostgreSQL in a nix-shell](https://mgdm.net/weblog/postgresql-in-a-nix-shell/). 
- [Postgresql/Postgis inside nix-shell with sqitch and default postgres user](https://gist.github.com/gusmacaulay/9dc5793439750912458f3c6a8945de7d). 
- [Nix Recipe: Setup Postgresql](https://zeroes.dev/p/nix-recipe-for-postgresql/). 
- [Unable to setup postgres in nix-shell](https://discourse.nixos.org/t/unable-to-setup-postgres-in-nix-shell/14813/2). 
- [trap](https://www.ludovicocaldara.net/dba/bash-tips-7-cleanup-on-exit/)

## To connect

    nix-shell
    psql
    
## Some psql commands

    explain (verbose true, format json) select author_id, name, url from author where author_id = 1;
    
    prepare foostmt (integer) as select author_id, name, url from author where author_id = $1;
    explain (verbose true, format json) execute foostmt(1);
    deallocate foostmt;
    
Interesting that it catches type errors in the prepared statement's parameters:

    foodb=# prepare foostmt2 (bytea) as select author_id, name, url from author where author_id = $1;
    ERROR:  operator does not exist: integer = bytea
    LINE 1: ...elect author_id, name, url from author where author_id = $1;
                                                                  ^
    HINT:  No operator matches the given name and argument types. You might need to add explicit type casts.
    
## Links

- [PREPARE](https://www.postgresql.org/docs/current/sql-prepare.html) and [DEALLOCATE](https://www.postgresql.org/docs/current/sql-deallocate.html)

- [EXPLAIN](https://www.postgresql.org/docs/current/sql-explain.html)

- [DESCRIBE](https://www.postgresql.org/docs/current/ecpg-sql-describe.html)

