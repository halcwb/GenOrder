

#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../WrappedString.fs"
#load "../Solver.fs"
#load "../ValueUnit.fs"
#load "../VariableUnit.fs"
#load "../Orderable.fs"
#load "../Prescription.fs"
#load "../Order.fs"

#time

open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

module Name = VariableUnit.Name
module Units = ValueUnit.Units

let lab id itms =

    let items =
        itms |> List.map (fun i -> 
            let n =
                [i] |> Name.create
            let u = 
                Units.Count.times
            n, u
        )

    let ord =
        Order.createNew 
            (id |> WrappedString.Id.create)
            (["lab"] |> Name.create)
            ([["lab"] |> Name.create, items])
            Units.Count.times 
            Units.Count.times
            ValueUnit.NoUnit 
            Units.Time.day
            (Prescription.discontinuous Units.Time.day Units.Time.day)
            "None"
    ord

lab "cardio" ["lact"; "pO2"]
