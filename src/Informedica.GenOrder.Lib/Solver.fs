namespace Informedica.GenOrder.Lib

/// Helper functions to 
/// facilitate the use of the
/// `Informedica.GenSolver.Lib`
module Solver =

    open Informedica.GenUtils.Lib
    open Informedica.GenUnits.Lib

    module N = Informedica.GenSolver.Lib.Variable.Name
    module SV = Informedica.GenSolver.Api
    module VR = Informedica.GenSolver.Lib.Variable
    module VL = VR.ValueRange
    module EQ = Informedica.GenSolver.Lib.Equation
    
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

    module Props =

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

    open Props

    /// Turn a set of values `vs` to base values 
    let toBase = List.map ValueUnit.toBase
    
    /// Solve a set of equations setting a property `p` with
    /// name `n`, to a valueset `vs`.
    let solve (N.Name n) p vs = 
        SV.solve (fun s -> printfn "%s" s) n (p |> propToString) (vs |> toBase)


module ValueRange =

    open Informedica.GenUnits.Lib
    open Informedica.GenSolver.Lib.Variable.ValueRange

    /// Convert a `ValueRange` to a `string`.
    let toStringWithUnit un vr =
        let fVs vs = 
            let vs = 
                vs 
                |> Set.toList
                |> List.map (ValueUnit.create un)
                |> List.map ValueUnit.toUnit

            print false vs None false None None false
    
        let some =
            ValueUnit.create un
            >> ValueUnit.toUnit
            >> Some

        let fRange =
            let print min minincl incr max maxincl = 
                print false [] min minincl incr max maxincl

            let fMin min =
                let min, minincl = 
                    match min with
                    | MinIncl v -> v |> some, true
                    | MinExcl v -> v |> some, false  
                print min minincl None None false

            let fMax max =
                let max, maxincl = 
                    match max with
                    | MaxIncl v -> v |> some, true
                    | MaxExcl v -> v |> some ,false  

                print None false None max maxincl

            let fMinIncr (min, incr)  = 
                let min, minincl = 
                    match min with
                    | MinIncl v -> v |> some, true
                    | MinExcl v -> v |> some ,false  

                let incr = incr |> incrToValue |> some
        
                print min minincl incr None false

            let fIncrMax (incr, max)  = 
                let max, maxincl = 
                    match max with
                    | MaxIncl v -> v |> some, true
                    | MaxExcl v -> v |> some ,false  

                let incr = incr |> incrToValue |> some
        
                print None false incr max maxincl

            let fMinMax (min, max) =
                let min, minincl = 
                    match min with
                    | MinIncl v -> v |> some, true
                    | MinExcl v -> v |> some ,false  

                let max, maxincl = 
                    match max with
                    | MaxIncl v -> v |> some, true
                    | MaxExcl v -> v |> some ,false  

                print min minincl None max maxincl

            applyRange fMin fMax fMinIncr fIncrMax fMinMax

        let unr = print true [] None false None None false
    
        vr |> apply unr fVs fRange 

