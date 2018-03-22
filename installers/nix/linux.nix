{ stdenv, runCommand, writeText, writeScriptBin, writeTextFile, fetchurl, fetchFromGitHub, openssl, electron,
coreutils, utillinux, procps, cluster,
rawapp, master_config, cardanoPkgs, cardanoProgs,
ghc, installer-generator
}:

let
  slimOpenssl = runCommand "openssl" {} ''
    mkdir -pv $out/bin/
    cp ${openssl}/bin/openssl $out/bin/
  '';

  daedalus = writeScriptBin "daedalus" ''
    #!${stdenv.shell}

    set -xe

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export CLUSTER=${cluster}
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"

    mkdir -p "''${DAEDALUS_DIR}/${cluster}/"{Logs/pub,Secrets}
    cd "''${DAEDALUS_DIR}/${cluster}/"

    if [ ! -d tls ]; then
      mkdir -p tls/{server,ca}
      ${slimOpenssl}/bin/openssl req -x509 -newkey rsa:2048 -keyout tls/server/server.key -out tls/server/server.crt -days 3650 -nodes -subj "/CN=localhost"
      cp tls/server/server.crt tls/ca/ca.crt
    fi
    exec ${cardanoProgs}/bin/cardano-launcher \
      --config ${rawapp}/launcher-config.yaml
  '';
in daedalus
