with import <nixpkgs> {};
mkShell {
    buildInputs = [
        (with ocaml-ng.ocamlPackages_4_09; [
            ocaml
            ocp-indent
        ])
        shellcheck
        xxd
    ];
    shellHook = ''
        . .shellhook
    '';
}
