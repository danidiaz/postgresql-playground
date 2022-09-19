{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
    packages = [
        pkgs.glibcLocales
        (pkgs.postgresql.withPackages (p: []))
        pkgs.pgcli
    ];
    shellHook = ''
        StartPG(){
            pg_ctl -w -l $PGDATA/log start
        }

        StopPG(){
            pg_ctl stop
        }

        export PGDATA="$PWD/.pg"
        export PGHOST="$PWD/.pg_sockets"
        export PGDATABASE="foodb"

        if [ ! -d $PGDATA ]; then
            initdb &> /dev/null
            mkdir -p $PGHOST
            echo "unix_socket_directories = '$PGHOST'" >> $PGDATA/postgresql.conf
            CREATE=true
        fi

        StartPG
        trap StopPG EXIT # ~ trapping HUP/EXIT

        if [[ $CREATE ]]; then
            createuser -s postgres &> /dev/null
            createdb $PGDATABASE
            # set a stable crappy password for local/dev
        fi
    '';
}