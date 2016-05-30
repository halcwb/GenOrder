namespace Informedica.GenWrap.Lib

/// Types and functions to deal with
/// value primitives
module WrappedString =

    /// Type and functions that 
    /// deal with an identifier
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Id = 

        type Id = Id of string

        let create s = s |> Id

        let lift f = fun (Id s) -> s |> f |> create

        let toString (Id s) = s

    /// Helper functions for `Informedica.GenSolver.Variable.Name` type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Name =
        
        open Informedica.GenSolver.Lib
        
        module N = Variable.Name

        type Name = N.Name

        /// Create a `Name` from a list of strings that 
        let create ns = ns |> String.concat "." |> N.createExc

        let lift f = fun (N.Name s) -> s |> f |> N.Name

        let toString = N.toString


