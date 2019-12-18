
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../WrappedString.fs"
#load "../List.fs"
#load "../ValueUnit.fs"
#load "../ValueRange.fs"
#load "../VariableUnit.fs"

#time

open MathNet.Numerics
    
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


module Units = ValueUnit.Units

let mg1 = ValueUnit.create Units.Mass.milliGram 1N
let piece = ValueUnit.create (Units.General.general "piece") 1N
[mg1; mg1 / piece] |> List.reduce (*) |> ValueUnit.get |> snd

mg1 * (mg1 / piece)
(mg1 / piece) * mg1
mg1 * (piece / mg1)
