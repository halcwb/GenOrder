namespace Informedica.GenOrder.Lib

module Order =

    module StartStop =
        
        open System

        type StartStop =
            | Start of DateTime
            | StartStop of DateTime * DateTime

    module OR = Orderable
    module PR = Prescription
    module DT = StartStop

    type T = 
        {
            Id: string // OrderId.T
            Patient: string // Patient T
            Prescriber: string // Prescriber T
            Indications: string * string list //Indication.T * Indication.T list
            Orderable: OR.Orderable
            Prescription: PR.Prescription
            Route: string // Route.T
            StartStop: DT.StartStop
        }


