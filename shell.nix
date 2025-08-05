with import <nixpkgs> {};
mkShell {
    buildInputs = [
        (with ocaml-ng.ocamlPackages; [
            benchmark
            ocaml
            ocp-indent
        ])
        shellcheck
        xxd
    ];
    shellHook = ''
        export BENCHMARK_CMXA_PATH=$(
            find ${ocaml-ng.ocamlPackages.benchmark} \
                -name "benchmark.cmxa" \
                -printf "%h"
        )
        . .shellhook
    '';
}
