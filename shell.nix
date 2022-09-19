{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
    packages = [
        pkgs.glibcLocales
        (pkgs.postgresql.withPackages (p: []))
        pkgs.pgcli
    ];
    shellHook = ''
        StartPG(){
            pg_ctl -w -l $PGDATA/log start &> /dev/null
        }

        StopPG(){
            pg_ctl stop &> /dev/null
        }

        export PGDATA="$PWD/.pg"

        if [ ! -d $PGDATA ]; then
            initdb &> /dev/null
            CREATE=true
        fi

        StartPG
        trap StopPG EXIT # ~ trapping HUP/EXIT

        if [[ $CREATE ]]; then
            createuser -s postgres &> /dev/null
            # set a stable crappy password for local/dev
            # create db
        fi
    '';
}