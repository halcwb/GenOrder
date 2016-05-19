namespace Informedica.GenOrder.Lib

/// Helper functions to facilitate the use 
/// of `Informedica.GenUnits.Lib`
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Unit = 

    open Informedica.GenUtils.Lib.BCL
    
    module UN = Informedica.GenUnits.Lib.CombiUnit

    /// Create a frequency unit
    let freqUnit = "1 X(Count)/day(Time)" |> UN.fromString
    /// Create a time unit
    let timeUnit = "1 hour(Time)"  |> UN.fromString
    /// Create a quantity unit
    let qtyUnit s = s |> UN.fromString
    /// Create a total unit
    let totalUnit s = s + "/day(Time)"  |> UN.fromString
    /// Create a rate unit
    let rateUnit s  = s + "/hour(Time)" |> UN.fromString
    /// Create a unit `s1` per unit `s2`
    let perUnit s2 s1 = "1 " + s1 + "/1 " + s2 |> UN.fromString
    /// Create a unit `cu` adjusted by unit `s2`
    let adjUnit s2 cu =
        match cu |> UN.toString |> String.split "/" with
        | [u] -> u + "/1 " + s2 
        | [u1;u2] -> u1 + "/1 " + s2 + "/" + u2 
        | _ -> failwith "Not suported"
        |> UN.fromString

