open Input
open Input_rewrite

let il = "../UHDM/dump.txt";;

let op fd v =
    let p = Input_rewrite.parse v in
    p
