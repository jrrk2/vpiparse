execute("rm -rf obj_dir")

xml=Sys.arg(2)
print(xml)
src=Sys.arg(3)
print(src)
verilator.tran(xml,src)
print("completed")
