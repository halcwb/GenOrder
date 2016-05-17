namespace Informedica.GenOrder.Lib

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Unit = 

    open Informedica.GenUtils.Lib.BCL
    
    module UN = Informedica.GenUnits.Lib.CombiUnit

    let freqUnit = "1 X/day" |> UN.fromString
    let timeUnit = "1 hour"  |> UN.fromString
    let qtyUnit s = s |> UN.fromString
    let totalUnit s = s + "/day"  |> UN.fromString
    let rateUnit s  = s + "/hour" |> UN.fromString
    let perUnit s2 s1 = "1 " + s1 + "/1 " + s2 |> UN.fromString
    let adjUnit s2 cu =
        match cu |> UN.toString |> String.split "/" with
        | [u] -> u + "/1 " + s2 
        | [u1;u2] -> u1 + "/1 " + s2 + "/" + u2 
        | _ -> failwith "Not suported"
        |> UN.fromString
