
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../WrappedString.fs"
#load "../Solver.fs"
#load "../ValueUnit.fs"
#load "../VariableUnit.fs"

#time
    
open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

module Quantity = VariableUnit.Quantity

ValueUnit.Units.Mass.milliGram
|> VariableUnit.Quantity.quantity ["gentamicin"] 
|> Quantity.toString

VariableUnit.RateAdjust.rateAdjust 
    ["dopamin"]
    ValueUnit.Units.Mass.microGram
    ValueUnit.Units.Weight.kiloGram
    ValueUnit.Units.Time.minute

