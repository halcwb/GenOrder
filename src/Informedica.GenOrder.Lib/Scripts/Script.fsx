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
module SV = Solver
module UN = Unit

for o in OR.createNew [["Genta"]] "mg" "ml" "kg" |> OR.toString do
    printfn "%s" o

for o in OR.createNew [["dopamine"];["sodium";"chloride"]] "mg" "ml" "kg" |> OR.toString do
    printfn "%s" o

let pcm = OR.createNew [["paracetamol"]] "mg" "tabl" "kg" 
let pre = PR.discontinuous

OD.createNew "kg" pcm pre "oral"
|> OD.toEqs
|> SV.solve (["paracetamol.Item.Component.Qty"] |> NM.create) SV.Vals [240N; 300N; 500N] (UN.qtyUnit "mg")
