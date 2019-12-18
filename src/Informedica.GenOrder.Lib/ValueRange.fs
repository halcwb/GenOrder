namespace Informedica.GenOrder.Lib

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


