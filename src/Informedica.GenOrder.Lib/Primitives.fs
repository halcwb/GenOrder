namespace Informedica.GenOrder.Lib

module Primitives =

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Id = 

        type Id = Id of string

        let create s = s |> Id

        let toString (Id s) = s

