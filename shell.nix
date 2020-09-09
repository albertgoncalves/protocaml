with import <nixpkgs> {};
pkgsMusl.mkShell {
    buildInputs = [
        (with ocaml-ng.ocamlPackages_4_10; [
            benchmark
            ocp-indent
            pkgsMusl.ocaml
        ])
        shellcheck
        xxd
    ];
    shellHook = ''
        export BENCHMARK_CMA_PATH=$(
            find ${ocaml-ng.ocamlPackages_4_10.benchmark} \
                -name "benchmark.cma" \
                -printf '%h\n'
        )
        . .shellhook
    '';
}
