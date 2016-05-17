namespace Informedica.GenOrder.Lib

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

    type Prop = 
        | Vals
        | MinIncl
        | MinExcl
        | Incr
        | MaxIncl
        | MaxExcl

    let propToString = function
        | Vals -> vals
        | MinIncl -> minincl
        | MinExcl -> minexcl
        | Incr -> incr
        | MaxIncl -> maxincl
        | MaxExcl -> maxexcl

    let valsToString u vs = 
        vs 
        |> List.map (UN.toBase u)
        |> List.map BigRational.toString 
        |> String.concat ", "
    
    let solve (N.Name n) p vs u = SV.solve id n (p |> propToString) (vs |> valsToString u)
