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

"Shape" |> UG.fromString |> UG.getUnits

let print ord =
    for s in ord |> OD.toString do
        printfn "%s" s
    ord
        
for o in OR.createNew [["Genta", "Mass"]] "Volume" "Weight" |> OR.toString do
    printfn "%s" o

for o in OR.createNew [["dopamine", "Mass"];["sodium", "Molar";"chloride", "Molar"]] "Volume" "Weight" |> OR.toString do
    printfn "%s" o

let pcm = OR.createNew [["paracetamol", "Mass"]] "Shape" "Weight"
let prs = PR.discontinuous 

let ord = OD.createNew "Weight" pcm prs "oral"

ord |> print |> ignore
let solve = OD.solve "paracetamol"
ord
|> solve  MP.ItemComponentQty SV.Vals [240N; 300N; 500N] "mg"
|> print
|> solve  MP.Freq SV.Vals [2N;3N;4N;5N;6N] "x/day"
|> print
|> solve  MP.OrderableOrderableQty SV.Vals [1N] "tabl"
|> print
|> solve  MP.OrderableDoseQty SV.Vals [1N] "tabl"
|> print
|> solve  MP.ItemDoseTotal SV.MaxIncl [4N] "gram/day"
|> print
|> solve  MP.ItemDoseAdjustTotalAdjust SV.MaxIncl [90N] "mg/kg/day"
|> print
|> solve  MP.AdjustQty SV.Vals [10N] "kg"
|> print
|> ignore
