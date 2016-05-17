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


