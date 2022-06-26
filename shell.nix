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
        export BENCHMARK_CMA_PATH=$(
            find ${ocaml-ng.ocamlPackages.benchmark} \
                -name "benchmark.cma" \
                -printf "%h"
        )
        . .shellhook
    '';
}
