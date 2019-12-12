
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../WrappedString.fs"
#load "../Solver.fs"
#load "../ValueUnit.fs"
#load "../VariableUnit.fs"
#load "../Orderable.fs"
#load "../Prescription.fs"
#load "../Order.fs"

#time
    
open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib


// Creating a paracetamol orderable from dtos
module Paracetamol =

        module Item = Orderable.Item
        module IDto = Item.Dto

        let idto = IDto.dto "1" "paracetamol"

        idto.ComponentQuantity.Unit <- "mg[Mass]"

        idto
        |> IDto.fromDto
        |> Item.toString
        |> List.iter (printfn "%s")


        module Component = Orderable.Component
        module CDto = Component.Dto

        let cdto = CDto.dto "1" "paracetamol"

        cdto.Items <- [ idto ]

        cdto
        |> CDto.fromDto
        |> Component.toString
        |> List.iter (printfn "%s")

        module ODto = Orderable.Dto

        let odto = ODto.dto "1" "paracetamol"

        odto.Components <- [ cdto ]

        let dto = Order.Dto.discontinuous "1" "paracetamol" "or"

        dto.Orderable <- odto

        dto.Prescription.Frequency.Unit <- "x[Count]/day[Time]"

        let print () =
            dto
            |> Order.Dto.fromDto
            |> Order.toString
            |> List.iteri (fun i s ->  printfn "%i\t%s" i s)


// sulfamethoxazol + trimethoprim orderable
module Cotrimoxazol =

    module Item = Orderable.Item
    module IDto = Item.Dto

    let sulfa_idto = IDto.dto "1" "sulfamethoxazol"


    sulfa_idto
    |> IDto.fromDto
    |> Item.toString
    |> List.iter (printfn "%s")

    let trim_idto = IDto.dto "1" "trimethoprim"


    trim_idto
    |> IDto.fromDto
    |> Item.toString
    |> List.iter (printfn "%s")


    module Component = Orderable.Component
    module CDto = Component.Dto

    let cdto = CDto.dto "1" "cotrimoxazol"

    cdto.Items <- [ sulfa_idto; trim_idto ]

    cdto
    |> CDto.fromDto
    |> Component.toString
    |> List.iter (printfn "%s")

    module ODto = Orderable.Dto

    let odto = ODto.dto "1" "cotrimoxazol"

    odto.Components <- [ cdto ]

    let print () =
        odto
        |> ODto.fromDto
        |> Orderable.toString
        |> List.iteri (fun i s ->  printfn "%i\t%s" i s)


// dopamin infusion orderable
module Dopamin =

    module Item = Orderable.Item
    module IDto = Item.Dto

    let dopa_idto = IDto.dto "1" "dopamin"

    dopa_idto
    |> IDto.fromDto
    |> Item.toString
    |> List.iter (printfn "%s")

    let sod_idto = IDto.dto "1" "sodium"


    sod_idto
    |> IDto.fromDto
    |> Item.toString
    |> List.iter (printfn "%s")

    let chlor_idto = IDto.dto "1" "chloride"

    chlor_idto
    |> IDto.fromDto
    |> Item.toString
    |> List.iter (printfn "%s")


    module Component = Orderable.Component
    module CDto = Component.Dto

    let dopa_cdto = CDto.dto "1" "dopamin"

    dopa_cdto.Items <- [ dopa_idto ]

    dopa_cdto
    |> CDto.fromDto
    |> Component.toString
    |> List.iter (printfn "%s")

    let saline_cdto = CDto.dto "1" "saline"

    saline_cdto.Items <- [ sod_idto; chlor_idto ]

    saline_cdto
    |> CDto.fromDto
    |> Component.toString
    |> List.iter (printfn "%s")

    module ODto = Orderable.Dto

    let odto = ODto.dto "1" "dopamine infusion"

    odto.Components <- [ dopa_cdto; saline_cdto ]

    let print () =
        odto
        |> ODto.fromDto
        |> Orderable.toString
        |> List.iteri (fun i s ->  printfn "%i\t%s" i s)


Paracetamol.print ()

