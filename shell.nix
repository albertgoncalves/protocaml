with import <nixpkgs> {};
mkShell {
    buildInputs = [
        (with ocaml-ng.ocamlPackages_4_12; [
            benchmark
            ocaml
            ocp-indent
        ])
        shellcheck
        xxd
    ];
    shellHook = ''
        export BENCHMARK_CMA_PATH=$(
            find ${ocaml-ng.ocamlPackages_4_12.benchmark} \
                -name "benchmark.cma" \
                -printf '%h\n'
        )
        . .shellhook
    '';
}
