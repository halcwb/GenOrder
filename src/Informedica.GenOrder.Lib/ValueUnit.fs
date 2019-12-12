namespace Informedica.GenOrder.Lib

/// Helper functions to 
/// facilitate the use of the
/// `Informedica.GenSolver.Lib`
module ValueUnit =

    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib

    let unitToString = ValueUnit.Units.toString ValueUnit.Units.Dutch ValueUnit.Units.Short

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
