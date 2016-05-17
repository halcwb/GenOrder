namespace Informedica.GenOrder.Lib

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Unit = 
    
    module UN = Informedica.GenUnits.Lib.CombiUnit

    let freqUnit = "1 X/day" |> UN.fromString
    let timeUnit = "1 hour"  |> UN.fromString
    let qtyUnit s = s |> UN.fromString
    let totalUnit s = s + "/day"  |> UN.fromString
    let rateUnit s  = s + "/hour" |> UN.fromString

