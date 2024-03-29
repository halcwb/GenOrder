﻿
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../WrappedString.fs"
#load "../List.fs"
#load "../ValueUnit.fs"
#load "../ValueRange.fs"
#load "../VariableUnit.fs"
#load "../Solver.fs"

#time

open MathNet.Numerics
    
open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

module Name = WrappedString.Name
module Units = ValueUnit.Units
module Props = Solver.Props

let noUnit = ValueUnit.NoUnit

let vru1 = 
    VariableUnit.createNew
        (Name.create ["A"])
        Units.Mass.milliGram

let vru2 = 
    VariableUnit.createNew
        (Name.create ["B"])
        noUnit

let vru3 =
    VariableUnit.createNew
        (Name.create ["C"])
        noUnit

let vru4 =
    VariableUnit.createNew
        (Name.create ["D"])
        Units.Volume.milliLiter

let vru5 =
    VariableUnit.createNew
        (Name.create ["E"])
        Units.Volume.milliLiter
 
// Unit replace tests

(vru1, [vru2; vru3]) 
|> Solver.SumEquation
|> List.singleton
|> Solver.replaceUnit (["C"] |> Name.create) vru1.Unit
|> Solver.toVariableUnits
|> List.collect id
|> List.filter VariableUnit.hasUnit

(vru1, [vru2; vru3]) 
|> Solver.SumEquation
|> List.singleton 
|> List.append [ (vru3, [vru4; vru2]) |> Solver.ProductEquation ]
|> Solver.replaceUnit (["C"] |> Name.create) vru1.Unit
|> Solver.toVariableUnits
|> List.collect id
|> List.filter VariableUnit.hasUnit

// Test Unit solver

// no units
(vru2, [vru3]) 
|> Solver.SumEquation
|> List.singleton
|> Solver.solveUnits
|> Solver.toVariableUnits
|> List.collect id
|> List.filter VariableUnit.hasUnit

// one unit in sum eq
(vru1, [vru2]) 
|> Solver.SumEquation
|> List.singleton
|> Solver.solveUnits
|> Solver.toVariableUnits
|> List.collect id
|> List.filter VariableUnit.hasUnit

// one unit in 3 vru sum eq
(vru1, [vru2; vru3]) 
|> Solver.SumEquation
|> List.singleton
|> Solver.solveUnits
|> Solver.toVariableUnits
|> List.collect id
|> List.filter VariableUnit.hasUnit

// no unit in prod eq
(vru2, [vru3]) 
|> Solver.ProductEquation
|> List.singleton
|> Solver.solveUnits
|> Solver.toVariableUnits
|> List.collect id
|> List.filter VariableUnit.hasUnit

// one onit in 2 product eq
(vru1, [vru2]) 
|> Solver.ProductEquation
|> List.singleton
|> Solver.solveUnits
|> Solver.toVariableUnits
|> List.collect id
|> List.filter VariableUnit.hasUnit

// one unit in 3 product eq
(vru1, [vru2; vru3]) 
|> Solver.ProductEquation
|> List.singleton
|> Solver.solveUnits
|> Solver.toVariableUnits
|> List.collect id
|> List.filter VariableUnit.hasUnit

// two unit in 3 product eq
(vru1, [vru2; vru4]) 
|> Solver.ProductEquation
|> List.singleton
|> Solver.solveUnits
|> Solver.toVariableUnits
|> List.collect id
|> List.filter VariableUnit.hasUnit


(vru1, [vru2]) 
|> Solver.SumEquation
|> List.singleton
|> List.append [ (vru3, [vru4; vru2]) |> Solver.ProductEquation ]
|> Solver.solveUnits
|> Solver.toVariableUnits

// Test solver

(vru1, [vru3]) 
|> Solver.SumEquation
|> List.singleton
|> Solver.solve vru2.Variable.Name Props.MaxIncl [ 10N ]

(vru1, [vru2]) 
|> Solver.SumEquation
|> List.singleton
|> Solver.solve vru2.Variable.Name Props.MaxIncl [ 10N ]


let toEq  vrus =
    vrus
    |> List.collect (fun vrul ->
        
        vrul
        |> function 
        | h::tail -> [ (h, tail) |> Solver.SumEquation ]
        | _ -> "oops" |> failwith
    )

// Test sum equations with increments
(vru1, [vru2; vru3])
|> Solver.SumEquation
|> List.singleton
|> Solver.solve vru1.Variable.Name Props.Vals [ 10N ]
|> toEq
|> Solver.solve vru2.Variable.Name Props.Incr [ 1N ]
|> toEq
|> Solver.solve vru3.Variable.Name Props.Vals [ 3N ]


module SumTests =

    let vru1 = 
        VariableUnit.createNew
            (Name.create ["A"])
            Units.Mass.gram

    let vru2 = 
        VariableUnit.createNew
            (Name.create ["B"])
            noUnit

    let vru3 =
        VariableUnit.createNew
            (Name.create ["C"])
            noUnit

    let vru4 =
        VariableUnit.createNew
            (Name.create ["D"])
            Units.Mass.gram

    let vru5 =
        VariableUnit.createNew
            (Name.create ["E"])
            Units.Mass.gram

    // Test sum equations with increments
    // And product equation
    (vru1, [vru2; vru3])
    |> Solver.SumEquation
    |> List.singleton 
    |> List.append [ (vru2, [vru4; vru5]) |> Solver.SumEquation ]
    |> Solver.solve vru1.Variable.Name Props.Vals [ 10N ]
    |> toEq
    |> Solver.solve vru2.Variable.Name Props.Incr [ 1N ]
    |> toEq
    |> Solver.solve vru5.Variable.Name Props.Vals [ 3N ]
    |> toEq 
    |> Solver.solve vru4.Variable.Name Props.Vals [ 2N ]