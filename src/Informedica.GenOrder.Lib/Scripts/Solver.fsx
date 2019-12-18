
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


(vru1, [vru2]) 
|> Solver.SumEquation
|> List.singleton
|> Solver.solve vru2.Variable.Name Props.MaxIncl [ 10N ]

(vru1, [vru2]) 
|> Solver.SumEquation
|> List.singleton
|> Solver.solveUnits
|> Solver.toVariableUnits
