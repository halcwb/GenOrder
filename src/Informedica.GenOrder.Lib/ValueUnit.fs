namespace Informedica.GenOrder.Lib

/// Helper functions to 
/// facilitate the use of the
/// `Informedica.GenUnits.Lib`
module ValueUnit =

    open MathNet.Numerics

    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib
    
    open ValueUnit

    module Units = ValueUnit.Units

    let unitToString = 
        ValueUnit.Units.toString Units.Dutch Units.Short

    let unitFromString s =
        if s |> String.isNullOrWhiteSpace then None
        else
            try
                "1 " + s
                |> ValueUnit.fromString
                |> ValueUnit.get
                |> snd
                |> Some
            with 
            | _ -> 
                printfn "could not parse to unit: %s" s
                None

    let calcUnit op u1 u2 = 
        
        match u1, u2 with
        | NoUnit, _ 
        | _, NoUnit -> NoUnit
        | u1, u2 ->
            let vu1 = 1N |> create u1
            let vu2 = 1N |> create u2

            vu1 |> op <| vu2
            |> get
            |> snd

    module Units =
        
        let noUnit = ValueUnit.NoUnit