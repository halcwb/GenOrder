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

    let discontinuous = FR.frequency |> Discontinuous
    
    let timed = (FR.frequency, TM.time) |> Timed

    let toVarUns pres =
        let frq, tme =
            match pres with
            | Process    -> FR.frequency, TM.time
            | Continuous -> FR.frequency, TM.time
            | Discontinuous (frq) -> frq, TM.time
            | Timed(frq, tme)     -> frq, tme
        
        frq |> FR.toVar, tme |> TM.toVar

    let fromVarUns eqs pres =
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
        
                
        

