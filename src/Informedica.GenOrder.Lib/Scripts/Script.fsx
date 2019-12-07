
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../WrappedString.fs"
#load "../Solver.fs"
#load "../ValueUnit.fs"
#load "../VariableUnit.fs"

#time
    
open Informedica.GenUtils.Lib.BCL
open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

ValueUnit.Units.Mass.milliGram
|> VariableUnit.Quantity.quantity ["gentamicin"] 

VariableUnit.RateAdjust.rateAdjust 
    ["dopamin"]
    ValueUnit.Units.Mass.microGram
    ValueUnit.Units.Weight.kiloGram
    ValueUnit.Units.Time.minute

