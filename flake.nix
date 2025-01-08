{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "nixpkgs/release-24.05";
  };

  outputs = { self, nixpkgs }: 
  let system = "x86_64-linux" ;
  in {
        devShells.${system}.default = 
        let pkgs = nixpkgs.legacyPackages.${system};
        in 
            pkgs.mkShell {
                packages = [
                    pkgs.glibcLocales
                    (pkgs.postgresql.withPackages (p: []))
                    pkgs.pgcli
                    (pkgs.haskellPackages.ghcWithPackages (hpkgs : 
                        [
                            hpkgs.lens 
                            hpkgs.optics
                        ]))
                    pkgs.haskellPackages.cabal-install
                    pkgs.haskellPackages.haskell-language-server
                    pkgs.haskellPackages.ormolu
                ];
                shellHook = 
                    let mypagila = builtins.fetchGit { 
                            url = "https://github.com/devrimgunduz/pagila.git" ;
                            rev = "fef9675714cfba1756df4719b5e36075a7ddf90e" ;
                            shallow = true;
                            name = "mypagila"; } ;
                    in 
                    ''
                    StartPG(){
                        pg_ctl -w -l $PGDATA/log start
                    }

                    StopPG(){
                        pg_ctl stop
                    }

                    export PGDATA="$PWD/.pg"
                    export PGHOST="$PWD/.pg_sockets"
                    export PGDATABASE="pagila"

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
                        cat ${ mypagila }/pagila-schema.sql | psql -d $PGDATABASE
                        cat ${ mypagila }/pagila-data.sql | psql -d $PGDATABASE
                    fi
                '';
            };
    };
}
