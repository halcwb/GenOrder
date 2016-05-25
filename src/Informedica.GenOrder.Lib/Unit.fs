namespace Informedica.GenOrder.Lib

/// Helper functions to facilitate the use 
/// of `Informedica.GenUnits.Lib`
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Unit = 

    open Informedica.GenUtils.Lib.BCL
    
    module UN = Informedica.GenUnits.Lib.CombiUnit

    /// Create a frequency unit
    let freqUnit = "1 X[Count]/day[Time]" |> UN.fromString
    /// Create a time unit
    let timeUnit = "1 hour[Time]"  |> UN.fromString
    /// Create a quantity unit
    let qtyUnit s = s |> UN.fromString
    /// Create a total unit
    let totalUnit s = s + "/day[Time]"  |> UN.fromString
    /// Create a rate unit
    let rateUnit s  = s + "/hour[Time]" |> UN.fromString
    /// Create a unit `s1` per unit `s2`
    let perUnit s2 s1 = "1 " + s1 + "/1 " + s2 |> UN.fromString
    /// Create a unit `cu` adjusted by unit `s2`
    let adjUnit s2 cu =
        match cu |> UN.toString |> String.split "/" with
        | [u] -> u + "/1 " + s2 
        | [u1;u2] -> u1 + "/1 " + s2 + "/" + u2 
        | _ -> failwith "Not suported"
        |> UN.fromString

/// Helper functions for the `Informedica.GenUnits.Lib.UnitGroup`
/// module
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module UnitGroups =
    
    open Informedica.GenUnits.Lib.UnitGroup

    module CS = Informedica.GenUnits.Lib.Constants

    /// Create a mass unitgroup
    let mass = CS.massGroup |> fromString

    /// Create a mass unitgroup
    let bsa = CS.bsaGroup |> fromString

    /// Create a count unitgroup
    let count = CS.countGroup |> fromString

    /// Create a distance unitgroup
    let distance = CS.distanceGroup |> fromString

    /// Create a molar unitgroup
    let molar = CS.molarGroup |> fromString

    /// Create a time unitgroup
    let time = CS.timeGroup |> fromString

    /// Create a volume unitgroup
    let volume = CS.volumeGroup |> fromString

    /// Create a weight unitgroup
    let weight = CS.weightGroup |> fromString
    