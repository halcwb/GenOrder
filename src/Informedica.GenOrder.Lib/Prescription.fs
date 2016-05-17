namespace Informedica.GenOrder.Lib

module Prescription =
    
    module FR = VariableUnit.Frequency
    module TM = VariableUnit.Time

    type Prescription = 
        | Process
        | Continuous
        | Discontinuous of FR.Frequency
        | Timed of FR.Frequency * TM.Time



