{ melpaBuild, fetchFromGitHub, fakeHash, compat, lsp-mode, dash, magit-section }:
melpaBuild rec {
  src = fetchFromGitHub {
    owner = "leanprover-community";
    repo = "lean4-mode";
    rev = "76895d8939111654a472cfc617cfd43fbf5f1eb6";
    hash = "sha256-DLgdxd0m3SmJ9heJ/pe5k8bZCfvWdaKAF0BDYEkwlMQ";
  };
  commit = "76895d8939111654a472cfc617cfd43fbf5f1eb6";
  version = "1";
  pname = "lean4-mode";
  packageRequires = [ compat lsp-mode dash magit-section ];
  postInstall = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/lean4-mode-1/data/
    cp -r $src/data/abbreviations.json $out/share/emacs/site-lisp/elpa/lean4-mode-1/data/
  '';
}
