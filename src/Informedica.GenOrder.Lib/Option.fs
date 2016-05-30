namespace Informedica.GenUtils.Lib

module Option =
    
    let map f = Option.bind (fun x -> x |> f |> Some)


