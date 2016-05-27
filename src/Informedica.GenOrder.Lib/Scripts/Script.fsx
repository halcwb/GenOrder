#load "load-project-release.fsx"

#time
    
open Informedica.GenUtils.Lib.BCL
open Informedica.GenOrder.Lib

module Name = VariableUnit.Name
module Id = Primitives.Id
module Map = Order.Mapping

let print ord = 
    for s in ord |> Order.toString do printfn "%s" s
    ord

let lab id frq unt itms =
    let getItmNm itm =
        [id; itm |> Name.toString] |> Name.create |> Name.toString

    let items =
        itms |> List.map (fun i -> ([i] |> Name.create, "Count"))

    let ord =
        Order.createNew 
            (id |> Id.create)
            (["lab"] |> Name.create)
            ([["lab"] |> Name.create, items])
            "Count" 
            "Weight" 
            Prescription.discontinuous
            "None"
    ord
//    items
//    |> List.fold (fun ord itm ->
//            ord
//            |> Order.solve (itm |> fst |> getItmNm) Map.ItemComponentQty  Solver.Vals [1N] "count" 
//            |> Order.solve (itm |> fst |> getItmNm)  Map.ItemOrderableQty Solver.Vals [1N] "count" 
//            |> Order.solve (itm |> fst |> getItmNm)  Map.ItemDoseQty      Solver.Vals [1N] "count" 
//            ) ord
//    |> Order.solve id Map.Freq Solver.Vals [frq] unt

let testLab =
    lab "1" 3N "x/day" ["gluc"; "Na"; "Chl"; "K"; "Ca"; "Hb"; "Ht"; "L"; "T"; 
                    "alb"; "LDH"; "ASA"; "ALAT"; "PT" ; "APTT"]  

lab "1" 1N "x/day" ["Hb"] 
|> print
|> ignore

testLab
|> Order.solve "1.APTT" Map.ItemDoseQty Solver.Vals [10N] "count"
|> print
|> ignore
