{ lib, stdenv, cmake, fetchFromGitHub, git, gmp, perl, testers, }:

stdenv.mkDerivation (finalAttrs:
  let version = "4.14.0";
  in {
    pname = "lean4";

    inherit version;

    src = fetchFromGitHub {
      owner = "leanprover";
      repo = "lean4";
      rev = "v${version}";
      hash = "sha256-5KIZGt4glC2rZDKDL0FiHUNVjVZAyY8iWDWQgdF/PIs=";
    };

    postPatch = ''
      substituteInPlace src/CMakeLists.txt \
        --replace 'set(GIT_SHA1 "")' 'set(GIT_SHA1 "${finalAttrs.src.rev}")'

      # Remove tests that fails in sandbox.
      # It expects `sourceRoot` to be a git repository.
      rm -rf src/lake/examples/git/
    '';

    preConfigure = ''
      patchShebangs stage0/src/bin/ src/bin/
    '';

    nativeBuildInputs = [ cmake ];

    buildInputs = [ gmp ];

    nativeCheckInputs = [ git perl ];

    cmakeFlags = [ "-DUSE_GITHASH=OFF" "-DINSTALL_LICENSE=OFF" ];

    passthru.tests = {
      version = testers.testVersion {
        package = finalAttrs.finalPackage;
        version = "v${version}";
      };
    };

    meta = with lib; {
      description = "Automatic and interactive theorem prover";
      homepage = "https://leanprover.github.io/";
      changelog =
        "https://github.com/leanprover/lean4/blob/${finalAttrs.src.rev}/RELEASES.md";
      license = licenses.asl20;
      platforms = platforms.all;
      maintainers = with maintainers; [ danielbritten jthulhu ];
      mainProgram = "lean";
    };
  })
