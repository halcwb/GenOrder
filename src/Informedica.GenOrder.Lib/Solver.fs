namespace Informedica.GenOrder.Lib

/// Helper functions to 
/// facilitate the use of the
/// `Informedica.GenSolver.Lib`
module Solver =

    open Informedica.GenUtils.Lib

    module N = Informedica.GenSolver.Lib.Variable.Name
    module SV = Informedica.GenSolver.Api
    module UN = Informedica.GenUnits.Lib.CombiUnit
    
    [<Literal>]
    let vals = "vals"
    [<Literal>]
    let minincl = "minincl"
    [<Literal>]
    let minexcl = "minexcl"
    [<Literal>]
    let incr = "incr"
    [<Literal>]
    let maxincl = "maxincl"
    [<Literal>]
    let maxexcl = "maxexcl"

    /// The properties that can be set
    /// for a variable.
    type Prop = 
        | Vals
        | MinIncl
        | MinExcl
        | Incr
        | MaxIncl
        | MaxExcl

    /// Return a string for a property `Prop`
    let propToString = function
        | Vals -> vals
        | MinIncl -> minincl
        | MinExcl -> minexcl
        | Incr -> incr
        | MaxIncl -> maxincl
        | MaxExcl -> maxexcl

    /// Turn a set of values `vs` to base values of
    /// unit `u`
    let valsToString u vs = 
        vs 
        |> List.map (UN.toBase u)
        |> List.map BigRational.toString 
        |> String.concat ", "
    
    /// Solve a set of equations setting a property `p` with
    /// name `n`, to a valueset `vs` with unit `u`.
    let solve (N.Name n) p vs u = SV.solve id n (p |> propToString) (vs |> valsToString u)


