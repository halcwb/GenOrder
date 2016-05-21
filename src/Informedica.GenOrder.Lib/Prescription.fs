namespace Informedica.GenOrder.Lib

module Prescription =
    
    module FR = VariableUnit.Frequency
    module TM = VariableUnit.Time

    type Prescription = 
        | Process
        | Continuous
        | Discontinuous of FR.Frequency
        | Timed of FR.Frequency * TM.Time

    let ``process`` = Process
    
    let continuous = Continuous

    let discontinuous n = n |> FR.frequency |> Discontinuous
    
    let timed n = (n |> FR.frequency, n |> TM.time) |> Timed

    let isContinuous = function | Continuous -> true | _ -> false

    let isTimed = function | Timed _ -> true | _ -> false

    let toEqs pres =
        match pres with
        | Process    -> None, None
        | Continuous -> None, None
        | Discontinuous (frq) -> frq |> FR.toVar |> Some, None
        | Timed(frq, tme)     -> frq |> FR.toVar |> Some, tme |> TM.toVar |> Some

    let fromEqs eqs pres =
        match pres with
        | Process    -> Process
        | Continuous -> Continuous
        | Discontinuous (frq) -> (frq |> FR.fromVar eqs) |> Discontinuous
        | Timed(frq, tme)     -> (frq |> FR.fromVar eqs, tme |> TM.fromVar eqs) |> Timed
        

    let toString (prs: Prescription) =
            match prs with
            | Process    -> ["Process"]
            | Continuous -> ["Continuous"]
            | Discontinuous (frq) -> [frq |> FR.toString]
            | Timed(frq, tme)     -> [frq |> FR.toString; tme |> TM.toString]
        
                
        

