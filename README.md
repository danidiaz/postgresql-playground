# PostgreSQL & nix-shell

- [Using PostgreSQL in a nix-shell](https://mgdm.net/weblog/postgresql-in-a-nix-shell/). 
- [Postgresql/Postgis inside nix-shell with sqitch and default postgres user](https://gist.github.com/gusmacaulay/9dc5793439750912458f3c6a8945de7d). 
- [Nix Recipe: Setup Postgresql](https://zeroes.dev/p/nix-recipe-for-postgresql/). 
- [Unable to setup postgres in nix-shell](https://discourse.nixos.org/t/unable-to-setup-postgres-in-nix-shell/14813/2). 
- [trap](https://www.ludovicocaldara.net/dba/bash-tips-7-cleanup-on-exit/)

## To connect

    nix-shell
    psql

## Rel8

- [Rel8](https://github.com/circuithub/rel8)
- [official documentation](https://rel8.readthedocs.io/en/latest/)
- [on Hackage](https://hackage.haskell.org/package/rel8)

```
[nix-shell]$ psql -f tables.sql
[nix-shell]$ ghci Main.hs
ghci> Right conn <- acquire ""
ghci> select (each projectSchema) & statement () & flip run conn
Right [Project {projectAuthorId = 1, projectName = "Some project"},Project {projectAuthorId = 2, projectName = "Another project"}]
```