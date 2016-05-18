#load "load-project-release.fsx"

#time
    
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

let print ord =
    for s in ord |> OD.toString do
        printfn "%s" s
    ord
        
for o in OR.createNew [["Genta"]] "mg" "ml" "kg" |> OR.toString do
    printfn "%s" o

for o in OR.createNew [["dopamine"];["sodium";"chloride"]] "mg" "ml" "kg" |> OR.toString do
    printfn "%s" o

let pcm = OR.createNew [["paracetamol"]] "mg" "tabl" "kg" 
let pre = PR.discontinuous

let ord = OD.createNew "kg" pcm pre "oral"


ord |> print |> ignore
ord
|> OD.solve "paracetamol" MP.ItemComponentQty SV.Vals [240N; 300N; 500N] (UN.qtyUnit "mg")
|> print
|> OD.solve "" MP.Freq SV.Vals [2N;3N;4N;5N;6N] (UN.freqUnit)
|> print
|> OD.solve "paracetamol" MP.OrderableOrderableQty SV.Vals [1N] (UN.qtyUnit "tabl")
|> print
|> OD.solve "paracetamol" MP.OrderableDoseQty SV.Vals [1N] (UN.qtyUnit "tabl")
|> print
|> OD.solve "paracetamol" MP.ItemDoseTotal SV.MaxIncl [4N] (UN.qtyUnit "gram")
|> print
|> OD.solve "paracetamol" MP.ItemDoseAdjustTotalAdjust SV.MaxIncl [90N] ("mg" |> UN.totalUnit |> UN.adjUnit "kg")
|> print
|> OD.solve "" MP.AdjustQty SV.Vals [10N] (UN.qtyUnit "kg")
|> print
|> ignore
