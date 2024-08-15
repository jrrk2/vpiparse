
test_combined: _build/default/outputparser/myluaclient.exe
	$< verify.lua test/hardcaml/blocking_add.sv

 _build/default/outputparser/myluaclient.exe:
	dune build
