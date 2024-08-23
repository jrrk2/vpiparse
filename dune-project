(lang dune 3.7)
(using menhir 2.1)

(name hardcaml-lua)
(version opam-publish-hardcaml-lua.0.0.1-dirty)

(generate_opam_files true)

(source
 (github jrrk2/vpiparse))

(authors "Jonathan Kimmitt")

(maintainers "Jonathan Kimmitt")

(license MIT)

(documentation https://github.com/jrrk2/vpiparse)

(package
 (name hardcaml-lua)
 (allow_empty)
 (synopsis "A lua client for interfacing hardcaml to verilator, UHDM, Verible and RTLIL front-ends")
 (description "Verilator, Surelog and Verible do not generate synthesised Verilog code directly. This software bridges the gap and verifies the results using build-in minisat solver, z3 or external eqy script")
 (depends xml-light msat hardcaml hardcaml_circuits lua-ml ppx_deriving_yojson z3)
 (tags (Verilator Surelog UHDM Verible Yosys RTLIL)))
