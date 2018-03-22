{ lib, pkgs, stdenv, nodejs-8_x, python, api, cluster, nukeReferences, version, fetchzip, installer-generator, cardanoPkgs, cardanoProgs, electron }:
let
  nodejs = nodejs-8_x;
  yarn2nix = import (fetchzip {
    url = "https://github.com/moretea/yarn2nix/archive/v1.0.0.tar.gz";
    sha256 = "02bzr9j83i1064r1r34cn74z7ccb84qb5iaivwdplaykyyydl1k8";
  }) { inherit pkgs nodejs; };
  networkMap = {
    mainnet = "mainnet";
    staging = "testnet";
  };
in
yarn2nix.mkYarnPackage {
  name = "daedalus-js";
  src = if 0 <= builtins.compareVersions builtins.nixVersion "1.12" then builtins.fetchGit ./. else lib.cleanSource ./.;
  API = api;
  NETWORK = networkMap.${cluster};
  DAEDALUS_VERSION = "${version}";
  NODE_ENV = "production";
  installPhase = ''
    mkdir -p $out/bin $out/share/daedalus
    cp -vi ${cardanoPkgs.cardano-sl.src + "/configuration.yaml"}                            $out/configuration.yaml
    cp -vi ${cardanoPkgs.cardano-sl.src + "/mainnet-genesis-dryrun-with-stakeholders.json"} $out/mainnet-genesis-dryrun-with-stakeholders.json
    cp -vi ${cardanoPkgs.cardano-sl.src + "/mainnet-genesis.json"}                          $out/mainnet-genesis.json
    cp -vi ${cardanoPkgs.cardano-sl.src + "/../log-configs/daedalus.yaml"}                  $out/daedalus.yaml
    ${installer-generator}/bin/make-installer config          \
       --cardano           "${cardanoProgs}"                  \
       --config-files      "$out"                             \
       --daedalus-frontend "$out"                             \
       --dhall-root        "${installer-generator.src}/dhall"
    cp -vi "launcher-config.yaml"                                                           $out
    cp -vi "wallet-topology.yaml"                                                           $out
    npm run build
    cp -R dist/* $out/share/daedalus
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/main/index.js.map
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/renderer/index.js.map

    # closure size TODO list
    # electron depends on cups, which depends on avahi
    cat >$out/bin/daedalus <<EOF
    #!${stdenv.shell}

    test -z "''\\''$XDG_DATA_HOME" && { XDG_DATA_HOME="''\\''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''\\''${XDG_DATA_HOME}/Daedalus"

    cd "''\\''${DAEDALUS_DIR}/${cluster}/"

    exec ${electron}/bin/electron $out/share/daedalus/main/
    EOF
    chmod +x $out/bin/daedalus
  '';
  yarnPreBuild = ''
    mkdir -p $HOME/.node-gyp/${nodejs.version}
    echo 9 > $HOME/.node-gyp/${nodejs.version}/installVersion
    ln -sfv ${nodejs}/include $HOME/.node-gyp/${nodejs.version}
  '';
  pkgConfig = {
    node-sass = {
      buildInputs = [ python ];
      postInstall = ''
        npm run build
      '';
    };
  };
  # work around some purity problems in nix
  yarnLock = ./yarn.lock;
  packageJSON = ./package.json;
}
