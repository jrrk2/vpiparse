xml="obj_dir"
src=Sys.arg(2)
print(src)
execute("rm -f "..xml.."/*")
execute("verilator --quiet --xml-only -Wno-WIDTHEXPAND -Wno-WIDTHTRUNC "..src)
verilator.tran(xml,src)
print("completed")
