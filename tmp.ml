open Input
open Input_pat4
open Input_dump

let tmpcond =
         TUPLE2 (Vpicondition,
           TUPLE3 (Vpieventorop,
             TUPLE2 (Vpiposedgeop,
               TUPLE4 (Ref_obj, STRING "clk",
                 TLIST [STRING "clk"; STRING "counter_async_test"],
                 TUPLE2 (Vpiactual,
                   TUPLE3 (Logic_net, STRING "clk",
                     TLIST [STRING "clk"; STRING "counter_async_test"])))),
             TUPLE2 (Vpiposedgeop,
               TUPLE4 (Ref_obj, STRING "rst",
                 TLIST [STRING "rst"; STRING "counter_async_test"],
                 TUPLE2 (Vpiactual,
                   TUPLE3 (Logic_net, STRING "rst",
                     TLIST [STRING "rst"; STRING "counter_async_test"]))))))

let rslt () = pat (empty_itms[]) tmpcond
		     
let crntp = 
  [TUPLE2 (Vpitop, Int 1);
   TUPLE2
    (Vpinet,
     TUPLE4
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST [STRING "rst"; STRING "counter_async_test"],
           TUPLE2
            (Vpiactual,
             TUPLE3 (Logic_typespec, LOC (2, 11, 2, 11), Logic_net)))),
       STRING "rst", TLIST [STRING "rst"; STRING "counter_async_test"]));
   TUPLE2
    (Vpinet,
     TUPLE4
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST [STRING "clk"; STRING "counter_async_test"],
           TUPLE2
            (Vpiactual,
             TUPLE3 (Logic_typespec, LOC (3, 11, 3, 11), Logic_net)))),
       STRING "clk", TLIST [STRING "clk"; STRING "counter_async_test"]));
   TUPLE2
    (Vpinet,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST [STRING "cnt"; STRING "counter_async_test"],
           TUPLE2
            (Vpiactual,
             TUPLE3
              (Logic_typespec, LOC (4, 12, 4, 21),
               TUPLE3
                (Vpirange,
                 TUPLE2
                  (Vpileftrange,
                   TUPLE5
                    (Constant, Vpidecompile "8", TUPLE2 (Vpisize, Int 64),
                     TUPLE2 (UINT, Int 8), Vpiuintconst)),
                 TUPLE2
                  (Vpirightrange,
                   TUPLE5
                    (Constant, Vpidecompile "0", TUPLE2 (Vpisize, Int 64),
                     TUPLE2 (UINT, Int 0), Vpiuintconst))))))),
       STRING "cnt", TLIST [STRING "cnt"; STRING "counter_async_test"],
       TUPLE2 (Vpinettype, Vpireg)));
   TUPLE2 (Vpitopmodule, Int 1);
   TUPLE2
    (Vpiport,
     TUPLE5
      (Input.Port, STRING "rst", TUPLE2 (Vpidirection, Vpiinput),
       TUPLE2
        (Vpilowconn,
         TUPLE4
          (Ref_obj, STRING "rst",
           TLIST [STRING "rst"; STRING "counter_async_test"],
           TUPLE2
            (Vpiactual,
             TUPLE4
              (Logic_net,
               TUPLE2
                (Vpitypespec,
                 TUPLE3
                  (Ref_typespec,
                   TLIST [STRING "rst"; STRING "counter_async_test"],
                   TUPLE2
                    (Vpiactual,
                     TUPLE3 (Logic_typespec, LOC (2, 11, 2, 11), Logic_net)))),
               STRING "rst",
               TLIST [STRING "rst"; STRING "counter_async_test"])))),
       TUPLE3
        (Ref_typespec, TLIST [STRING "rst"; STRING "counter_async_test"],
         TUPLE2
          (Vpiactual, TUPLE3 (Logic_typespec, LOC (2, 11, 2, 11), Logic_net)))));
   TUPLE2
    (Vpiport,
     TUPLE5
      (Input.Port, STRING "clk", TUPLE2 (Vpidirection, Vpiinput),
       TUPLE2
        (Vpilowconn,
         TUPLE4
          (Ref_obj, STRING "clk",
           TLIST [STRING "clk"; STRING "counter_async_test"],
           TUPLE2
            (Vpiactual,
             TUPLE4
              (Logic_net,
               TUPLE2
                (Vpitypespec,
                 TUPLE3
                  (Ref_typespec,
                   TLIST [STRING "clk"; STRING "counter_async_test"],
                   TUPLE2
                    (Vpiactual,
                     TUPLE3 (Logic_typespec, LOC (3, 11, 3, 11), Logic_net)))),
               STRING "clk",
               TLIST [STRING "clk"; STRING "counter_async_test"])))),
       TUPLE3
        (Ref_typespec, TLIST [STRING "clk"; STRING "counter_async_test"],
         TUPLE2
          (Vpiactual, TUPLE3 (Logic_typespec, LOC (3, 11, 3, 11), Logic_net)))));
   TUPLE2
    (Vpiport,
     TUPLE5
      (Input.Port, STRING "cnt", TUPLE2 (Vpidirection, Vpioutput),
       TUPLE2
        (Vpilowconn,
         TUPLE4
          (Ref_obj, STRING "cnt",
           TLIST [STRING "cnt"; STRING "counter_async_test"],
           TUPLE2
            (Vpiactual,
             TUPLE5
              (Logic_net,
               TUPLE2
                (Vpitypespec,
                 TUPLE3
                  (Ref_typespec,
                   TLIST [STRING "cnt"; STRING "counter_async_test"],
                   TUPLE2
                    (Vpiactual,
                     TUPLE3
                      (Logic_typespec, LOC (4, 12, 4, 21),
                       TUPLE3
                        (Vpirange,
                         TUPLE2
                          (Vpileftrange,
                           TUPLE5
                            (Constant, Vpidecompile "8",
                             TUPLE2 (Vpisize, Int 64), TUPLE2 (UINT, Int 8),
                             Vpiuintconst)),
                         TUPLE2
                          (Vpirightrange,
                           TUPLE5
                            (Constant, Vpidecompile "0",
                             TUPLE2 (Vpisize, Int 64), TUPLE2 (UINT, Int 0),
                             Vpiuintconst))))))),
               STRING "cnt",
               TLIST [STRING "cnt"; STRING "counter_async_test"],
               TUPLE2 (Vpinettype, Vpireg))))),
       TUPLE3
        (Ref_typespec, TLIST [STRING "cnt"; STRING "counter_async_test"],
         TUPLE2
          (Vpiactual,
           TUPLE3
            (Logic_typespec, LOC (4, 12, 4, 21),
             TUPLE3
              (Vpirange,
               TUPLE2
                (Vpileftrange,
                 TUPLE5
                  (Constant, Vpidecompile "8", TUPLE2 (Vpisize, Int 64),
                   TUPLE2 (UINT, Int 8), Vpiuintconst)),
               TUPLE2
                (Vpirightrange,
                 TUPLE5
                  (Constant, Vpidecompile "0", TUPLE2 (Vpisize, Int 64),
                   TUPLE2 (UINT, Int 0), Vpiuintconst))))))));
   TUPLE2
    (Vpiprocess,
     TUPLE2
      (TUPLE2 (Input.Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE3
        (Event_control,
         TUPLE2
          (Vpicondition,
           TUPLE3
            (Vpieventorop,
             TUPLE2
              (Vpiposedgeop,
               TUPLE4
                (Ref_obj, STRING "clk",
                 TLIST [STRING "clk"; STRING "counter_async_test"],
                 TUPLE2
                  (Vpiactual,
                   TUPLE3
                    (Logic_net, STRING "clk",
                     TLIST [STRING "clk"; STRING "counter_async_test"])))),
             TUPLE2
              (Vpiposedgeop,
               TUPLE4
                (Ref_obj, STRING "rst",
                 TLIST [STRING "rst"; STRING "counter_async_test"],
                 TUPLE2
                  (Vpiactual,
                   TUPLE3
                    (Logic_net, STRING "rst",
                     TLIST [STRING "rst"; STRING "counter_async_test"])))))),
         TUPLE4
          (Input.If_else,
           TUPLE2
            (Vpicondition,
             TUPLE4
              (Ref_obj, STRING "rst",
               TLIST [STRING "rst"; STRING "counter_async_test"],
               TUPLE2
                (Vpiactual,
                 TUPLE3
                  (Logic_net, STRING "rst",
                   TLIST [STRING "rst"; STRING "counter_async_test"])))),
           TUPLE4
            (Assignment, Vpirhs,
             TUPLE2
              (Vpirhs,
               TUPLE5
                (Constant, Vpidecompile "8'b00000000",
                 TUPLE2 (Vpisize, Int 8), BIN "00000000", Vpibinaryconst)),
             TUPLE2
              (Vpilhs,
               TUPLE4
                (Ref_obj, STRING "cnt",
                 TLIST [STRING "cnt"; STRING "counter_async_test"],
                 TUPLE2
                  (Vpiactual,
                   TUPLE4
                    (Logic_net, STRING "cnt",
                     TLIST [STRING "cnt"; STRING "counter_async_test"],
                     TUPLE2 (Vpinettype, Vpireg)))))),
           TUPLE4
            (Assignment, Vpirhs,
             TUPLE2
              (Vpirhs,
               TUPLE3
                (Vpiaddop,
                 TUPLE4
                  (Ref_obj, STRING "cnt",
                   TLIST [STRING "cnt"; STRING "counter_async_test"],
                   TUPLE2
                    (Vpiactual,
                     TUPLE4
                      (Logic_net, STRING "cnt",
                       TLIST [STRING "cnt"; STRING "counter_async_test"],
                       TUPLE2 (Vpinettype, Vpireg)))),
                 TUPLE5
                  (Constant, Vpidecompile "1", TUPLE2 (Vpisize, Int 64),
                   TUPLE2 (UINT, Int 1), Vpiuintconst))),
             TUPLE2
              (Vpilhs,
               TUPLE4
                (Ref_obj, STRING "cnt",
                 TLIST [STRING "cnt"; STRING "counter_async_test"],
                 TUPLE2
                  (Vpiactual,
                   TUPLE4
                    (Logic_net, STRING "cnt",
                     TLIST [STRING "cnt"; STRING "counter_async_test"],
                     TUPLE2 (Vpinettype, Vpireg))))))))))]
