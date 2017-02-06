{ fetchurl, buildPythonPackage, doCheck ? false, extradeps ? {} }:
with extradeps;
rec {
  
  "plumbum" = buildPythonPackage {
    name = "plumbum-1.6.3";
    src = fetchurl {
      url = "https://pypi.python.org/packages/50/15/f26f60e1bb82aabed7ff86f3fd2976784047f9a291c63ac9019086a69559/plumbum-1.6.3.tar.gz";
      sha256 = "083kikr1f7qzpp5jllss97dy8d6249v7ia3wg9i0a6wz8l4ffj82";
    };
    inherit doCheck;
    buildInputs = [  ];
  };
}
