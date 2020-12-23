with import <nixpkgs> {};
pkgsMusl.mkShell {
    buildInputs = [
        (with pkgsMusl.ocaml-ng.ocamlPackages_4_10; [
            benchmark
            ocaml
            ocp-indent
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
