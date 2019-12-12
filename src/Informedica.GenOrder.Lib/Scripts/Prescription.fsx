
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../WrappedString.fs"
#load "../Solver.fs"
#load "../ValueUnit.fs"
#load "../VariableUnit.fs"
#load "../Orderable.fs"
#load "../Prescription.fs"

#time
    
open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

let dto = Prescription.Dto.dto "1"

dto
|> Prescription.Dto.fromDto


dto
|> Prescription.Dto.setToContinuous
|> Prescription.Dto.fromDto

dto
|> Prescription.Dto.setToDiscontinuous
|> Prescription.Dto.fromDto

dto
|> Prescription.Dto.setToTimed
|> Prescription.Dto.fromDto
