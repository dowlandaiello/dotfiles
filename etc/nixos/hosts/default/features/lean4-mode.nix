{ trivialBuild, fetchFromGitHub, fakeHash, compat, lsp-mode, dash, magit-section }:
trivialBuild rec {
  pname = "lean4-mode";
  version = "1.1.2";
  src = fetchFromGitHub {
    owner = "leanprover-community";
    repo = "lean4-mode";
    rev = "76895d8939111654a472cfc617cfd43fbf5f1eb6";
    hash = "sha256-DLgdxd0m3SmJ9heJ/pe5k8bZCfvWdaKAF0BDYEkwlMQ";
  };
  propagatedUserEnvPkgs = [ compat lsp-mode dash magit-section ];
  buildInputs = propagatedUserEnvPkgs;
}
