#load "load-project-release.fsx"

#time
    
open Informedica.GenUtils.Lib.BCL
open Informedica.GenOrder.Lib

module VU = VariableUnit
module NM = VU.Name
module FR = VU.Frequency
module OR = Orderable
module IT = OR.Item
module CM = OR.Component
module PR = Prescription
module OD = Order
module MP = OD.Mapping
module SV = Solver
module UN = Unit
module UG = Informedica.GenUnits.Lib.UnitGroup

let print ord = 
    for s in ord |> OD.toString do printfn "%s" s
    ord

let lab frq unt itms =
    let items =
        itms |> List.map (fun i -> [(i, "Count")])

    let ord =
        OD.createNew 
            items
            "Count" 
            "Weight" 
            PR.discontinuous
            "None"
    items
    |> List.collect id
    |> List.fold (fun ord itm ->
            ord
            |> OD.solve (itm |> fst) MP.ItemComponentQty SV.Vals [1N] "count" 
            |> OD.solve (itm |> fst) MP.ItemOrderableQty SV.Vals [1N] "count" 
            |> OD.solve (itm |> fst) MP.ItemDoseQty SV.Vals [1N] "count" 
            |> OD.solve (itm |> fst) MP.ComponentComponentQty SV.Vals [1N] "count" 
            |> OD.solve (itm |> fst) MP.ComponentOrderableQty SV.Vals [1N] "count" 
            ) ord
    |> OD.solve "" MP.Freq SV.Vals [frq] unt

lab 3N "x/day" ["gluc"; "Na"; "Chl"; "K"; "Ca"; "Hb"; "Ht"; "L"; "T"; "alb"]  
|> print
|> ignore
