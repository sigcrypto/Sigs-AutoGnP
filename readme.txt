# Sigs-AutoGnP
The Tool has been tested on ubuntu 16.04 platform

Installation:

The sigs-AutoGnP requires C++ libraries NTL and Libfactories, Ocaml and its varies libraries

Procedure to Install:

Linux Platform:
1. Install gmp and libffi development packages to linux is required (If not pre-installed)
2. Install Opam package manager for ocaml
            Follow the link for the procedure: https://opam.ocaml.org/doc/Install.html
3. C++ libraries:
          i.Download and install NTL from link: http://www.shoup.net/ntl/download.html
          ii.Download and install libfactory from link: http://www.mathematik.uni-kl.de/ftp/pub/Math/Factory/
4. Compile  Sigs-AutoGnP
          #Clone the Sigs-AutoGnP repository
          #complile the tool
                        cd AutoGnP
                        # tell the opam package manager to use the cloned repo
                        opam pin add autognp . -n
                        # install Ocaml dependencies
                        opam install autognp --deps-only
                        # compile autognp
                        make
                        #run test-suite  (Run it from copied directory)
                        make test-examples

