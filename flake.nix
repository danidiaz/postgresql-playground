{
  description = "A very basic flake";

  # https://search.nixos.org/packages
  inputs = {
    nixpkgs.url = "nixpkgs/release-24.11";
  };

  outputs = { self, nixpkgs }: 
  let system = "x86_64-linux" ;
  in {
        devShells.${system}.default = 
        let pkgs = nixpkgs.legacyPackages.${system};
        in 
            # https://ryantm.github.io/nixpkgs/builders/special/mkshell/
            pkgs.mkShell {
                # nativeBuildInputs = [
                # ];
                packages = [
                    pkgs.glibcLocales
                    pkgs.pgcli
                    # https://discourse.nixos.org/t/ruby-cannot-find-libpq-and-neither-can-i/46739/2
                    pkgs.postgresql
                    # https://discourse.haskell.org/t/nixos-specifying-ghc-version/6478/2
                    (pkgs.haskellPackages.ghcWithPackages (hpkgs : 
                        [
                        ]))
                    pkgs.haskellPackages.cabal-install
                    # pkgs.haskellPackages.haskell-language-server
                    pkgs.haskellPackages.ormolu
                ];
                # https://www.reddit.com/r/NixOS/comments/110xqki/comment/j8k996e/
                shellHook = 
                    let mypagila = pkgs.fetchFromGitHub { 
                            owner =  "devrimgunduz" ;
                            repo =  "pagila" ;
                            rev = "5ba5a57aeb159f75f02aca2432d3c262186d13d3" ;
                            hash = "sha256-3Jt0EqeU2NgJt+y78RkbV30JpGmYWlr+zlVvGPW7YFM=";
                            } ;
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

                    PS1='\e[4;32m(dev) \W\$\e[m '
                '';
            };
    };
}
