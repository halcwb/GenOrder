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

"mg[Mass]/kg[Weight]/min[Time]" |> Informedica.GenUnits.Lib.CombiUnit.fromString
|> UG.fromUnit
|> UG.getUnits
|> List.map Informedica.GenUnits.Lib.CombiUnit.toString
|> List.iter (printfn "%s")

"Mass/Weight/Time" |> UG.fromString
|> UG.getUnits
|> List.map Informedica.GenUnits.Lib.CombiUnit.toString
|> List.iter (printfn "%s")

let print ord =
    for s in ord |> OD.toString do
        printfn "%s" s
    ord
        
for o in OR.createNew [["Genta"]] "mg[Mass]" "ml[Volume]" "kg[Weight]" |> OR.toString do
    printfn "%s" o

for o in OR.createNew [["dopamine"];["sodium";"chloride"]] "mg[Mass]" "ml[Volume]" "kg[Weight]" |> OR.toString do
    printfn "%s" o

let pcm = OR.createNew [["paracetamol"]] "mg[Mass]" "tabl[Shape]" "kg[Weight]" 
let pre = PR.discontinuous

let ord = OD.createNew "kg[Weight]" pcm pre "oral"


ord |> print |> ignore
ord
|> OD.solve "paracetamol" MP.ItemComponentQty SV.Vals [240N; 300N; 500N] "mg"
|> print
|> OD.solve "" MP.Freq SV.Vals [2N;3N;4N;5N;6N] "X/day"
|> print
|> OD.solve "paracetamol" MP.OrderableOrderableQty SV.Vals [1N] "tabl"
|> print
|> OD.solve "paracetamol" MP.OrderableDoseQty SV.Vals [1N] "tabl"
|> print
|> OD.solve "paracetamol" MP.ItemDoseTotal SV.MaxIncl [4N] "gram/day"
|> print
|> OD.solve "paracetamol" MP.ItemDoseAdjustTotalAdjust SV.MaxIncl [90N] "mg/kg/day"
|> print
|> OD.solve "" MP.AdjustQty SV.Vals [10N] "kg"
|> print
|> ignore
