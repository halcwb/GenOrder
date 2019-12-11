
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../WrappedString.fs"
#load "../Solver.fs"
#load "../ValueUnit.fs"
#load "../VariableUnit.fs"
#load "../Orderable.fs"
#load "../Prescription.fs"
#load "../Order.fs"
#load "../OrderSet.fs"

#time

open MathNet.Numerics
open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

module Name = VariableUnit.Name
module Units = ValueUnit.Units
module Map = Order.Mapping

let lab id itms =

    let items =
        itms |> List.map (fun i -> 
            [i] |> Name.create,
            ValueUnit.NoUnit
        )

    let ord =
        Order.createNew 
            (id |> WrappedString.Id.create)
            (["lab"] |> Name.create)
            [ ["lab"] |> Name.create, items ]
            Units.Count.times 
            Units.Count.times
            ValueUnit.NoUnit 
            Units.Time.day
            (Prescription.discontinuous Units.Time.day Units.Time.day)
            "None"
    ord

let test1 =
    lab "1" ["gluc"; "Na"]  

let pcm =
    Order.createNew 
        ("2" |> WrappedString.Id.create)
        (["paracetamol"] |> Name.create)
        ([["paracetamol"] |> Name.create, [
            ["paracetamol"] |> Name.create, Units.Mass.milliGram
        ]])
        (Units.General.general "piece")
        (Units.General.general "piece")
        Units.Weight.kiloGram
        Units.Time.day
        (Prescription.discontinuous Units.Time.day Units.Time.day)
        "oral"

let pcm2 =
    Order.createNew 
        ("3" |> WrappedString.Id.create)
        (["paracetamol"] |> Name.create)
        ([["paracetamol"] |> Name.create, [
            ["paracetamol"] |> Name.create, Units.Mass.milliGram
        ]])
        (Units.General.general "piece")
        (Units.General.general "piece")
        Units.Weight.kiloGram
        Units.Time.day
        (Prescription.discontinuous Units.Time.day Units.Time.day)
        "oral"


let ors = 
    OrderSet.empty
    |> OrderSet.add test1
    |> OrderSet.add pcm
    |> OrderSet.add pcm2


let print ors =
    let (prod, sum) = ors |> OrderSet.toEqs 
    printfn "Total Equations: %A" (prod @ sum |> List.length)
    for n in ((prod @ sum) |> List.collect id) do
        printfn "%A" (n |> VariableUnit.getName)

let ors' =
    ors
    |> OrderSet.solve "1.lab.Freq" Solver.Vals [3N] Units.Time.day
    |> OrderSet.solve "1.paracetamol.Freq" Solver.Vals [3N] Units.Time.day
    |> OrderSet.solve "1.paracetamol.Item.Dose.Qty" Solver.Vals [120N] Units.Mass.milliGram


print ors

let toEqs ors =
    let p, s = ors |> OrderSet.toEqs
    p @ s 

ors 
|> toEqs
|> List.map (List.map VariableUnit.getVar)
|> OrderSet.fromEqs ors
|> print

ors
|> OrderSet.solve "1.lab.Freq" Solver.Vals [3N] Units.Time.day
|> OrderSet.solve "2.paracetamol.Freq" Solver.Vals [2N..6N] Units.Time.day
//|> OrderSet.solve "2.paracetamol.Item.Dose.Qty" Solver.Vals [120N] "mg"
|> OrderSet.solve "paracetamol.Total" Solver.MaxIncl [4N] Units.Mass.gram
|> OrderSet.solve "paracetamol.TotalAdjust" Solver.MaxIncl [90N] Units.Mass.milliGram
|> print
