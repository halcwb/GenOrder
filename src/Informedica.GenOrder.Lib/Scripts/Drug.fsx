
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../WrappedString.fs"
#load "../List.fs"
#load "../ValueUnit.fs"
#load "../ValueRange.fs"
#load "../VariableUnit.fs"
#load "../Solver.fs"
#load "../Orderable.fs"
#load "../Prescription.fs"
#load "../Order.fs"

#time
   
open MathNet.Numerics

open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

module Units = ValueUnit.Units


// Creating a non fluid drug
module Drug =

        module RouteShape =

            type RouteShape =
                | IntravenousFluid
                | OralFluid
                | OralSolid
                | RectalSolid

            let mapping = 
                [
                    "rect", "supp", RectalSolid
                    "rect", "zetpil", RectalSolid
                    "rectaal", "supp", RectalSolid
                    "rectaal", "zetpil", RectalSolid
                    "or", "tablet", OralSolid
                    "or", "pill", OralSolid
                    "or", "pil", OralSolid
                    "or", "capsule", OralSolid
                    "oraal", "tablet", OralSolid
                    "oraal", "pill", OralSolid
                    "oraal", "pil", OralSolid
                    "oraal", "capsule", OralSolid
                    "or", "drink", OralFluid
                    "or", "drank", OralFluid
                ]

            let map route shape =
                mapping
                |> List.tryFind (fun (r, s, _) -> r = route && s = shape )
                |> Option.bind (fun (_, _, rs) -> rs |> Some)


        module Constraints =

            module Mapping = Order.Mapping
            module Props = Solver.Props
            module Name = WrappedString.Name
            
    
            type Constraint = 
                | FreqPerDayMin of BigRational
                | FreqPerDayMax of BigRational
                | PatientWeightMin of BigRational
                | PatientWeightMax of BigRational
                | PatientHeightMin of BigRational
                | PatientHeightMax of BigRational
                | SolidComponentOrderableCount of BigRational
                | SolidComponentOrderableConcentration of BigRational
                | SolidComponentComponentQuantityMax of BigRational
                | SolidOralOrderableDoseQuantityMax of BigRational
                | SuppositoryOrderableDoseQuantity of BigRational
                | FluidOrderableDoseQuantityMax of BigRational
                | FluidOrderableDoseRateMin of BigRational
                | FluidOrderableDoseRateMax of BigRational
                | FluidOrderableDoseAdjustQuantityMax of BigRational

            let constraints =
                [
                    FreqPerDayMin 1N
                    FreqPerDayMax 12N
                    PatientWeightMin (245N / 1000N)
                    PatientWeightMax 635N
                    PatientHeightMin 23N
                    PatientHeightMax 272N
                    SolidComponentOrderableCount 1N
                    SolidComponentOrderableConcentration 1N
                    SolidComponentComponentQuantityMax 1N
                    FluidOrderableDoseQuantityMax 1000N
                    FluidOrderableDoseRateMin (1N/100N)
                    FluidOrderableDoseRateMax (1000N)
                    FluidOrderableDoseAdjustQuantityMax 20N
                    SuppositoryOrderableDoseQuantity 1N
                    SolidOralOrderableDoseQuantityMax 10N
                ]

            let apply cs shape (o : Order.Order) =
                let n = o.Orderable.Name |> Name.toString
                let os = RouteShape.map o.Route shape

                cs
                |> List.fold (fun acc c ->
                    printfn "setting constraint: %A" c
                    match c with
                    | FreqPerDayMin x -> 
                        acc
                        |> Order.solve n Mapping.Freq Props.MinIncl [x]
                    | FreqPerDayMax x -> 
                        acc
                        |> Order.solve n Mapping.Freq Props.MaxIncl [x]                
                    | PatientWeightMin x -> 
                        acc
                        |> Order.solve n Mapping.AdjustQty Props.MinIncl [x]
                    | PatientWeightMax x -> 
                        acc
                        |> Order.solve n Mapping.AdjustQty Props.MaxIncl [x]
                    | PatientHeightMin x -> 
                        acc
        //                |> Order.solve n Mapping.Freq Props.Vals [x]
                    | PatientHeightMax x -> 
                        acc
        //                |> Order.solve n Mapping.Freq Props.Vals [x]
                    | SolidComponentOrderableCount x ->
                        os
                        |> function
                        | Some RouteShape.OralSolid | Some RouteShape.RectalSolid ->
                            acc
                            |> Order.solve n Mapping.ComponentOrderableCount Props.Vals [x]
                        | _ -> acc
                    | SolidComponentOrderableConcentration x ->
                        os
                        |> function
                        | Some RouteShape.OralSolid | Some RouteShape.RectalSolid ->
                            acc
                            |> Order.solve n Mapping.ComponentOrderableConc Props.Vals [x]
                        | _ -> acc
                    | SolidComponentComponentQuantityMax x ->
                        os
                        |> function
                        | Some RouteShape.OralSolid | Some RouteShape.RectalSolid ->
                            acc
                            |> Order.solve n Mapping.ComponentComponentQty Props.MaxIncl [x]
                        | _ -> acc
                    | FluidOrderableDoseQuantityMax x -> 
                        os
                        |> function
                        | Some RouteShape.OralFluid | Some RouteShape.IntravenousFluid ->
                            acc
                            |> Order.solve n Mapping.OrderableDoseQty Props.MaxIncl [x]
                        | _ -> acc
                    | FluidOrderableDoseRateMin x -> 
                        os
                        |> function
                        | Some RouteShape.OralFluid | Some RouteShape.IntravenousFluid ->
                            acc
                            |> Order.solve n Mapping.OrderableDoseRate Props.MinIncl [x]
                        | _ -> acc
                    | FluidOrderableDoseRateMax x -> 
                        os
                        |> function
                        | Some RouteShape.OralFluid | Some RouteShape.IntravenousFluid ->
                            acc
                            |> Order.solve n Mapping.OrderableDoseRate Props.MaxIncl [x]
                        | _ -> acc
                    | FluidOrderableDoseAdjustQuantityMax x -> 
                        os
                        |> function
                        | Some RouteShape.OralFluid | Some RouteShape.IntravenousFluid ->
                            acc
                            |> Order.solve n Mapping.OrderableDoseAdjustQtyAdjust Props.MaxIncl [x]
                        | _ -> acc
                    | SuppositoryOrderableDoseQuantity x -> 
                        os
                        |> function
                        | Some RouteShape.RectalSolid ->
                            acc
                            |> Order.solve n Mapping.OrderableDoseQty Props.Vals [x]
                        | _ -> acc
                    | SolidOralOrderableDoseQuantityMax x -> 
                        os
                        |> function
                        | Some RouteShape.OralSolid ->                
                            acc
                            |> Order.solve n Mapping.OrderableDoseQty Props.MaxIncl [x]
                        | _ -> acc

                ) o

        module Item = Orderable.Item
        module IDto = Item.Dto
        module Component = Orderable.Component
        module CDto = Component.Dto
        module ODto = Orderable.Dto

        module Mapping = Order.Mapping
        module Props = Solver.Props

        let create id name substs qty ug un du tu shape div route =
            let su = 
                match shape |> RouteShape.map route with
                | Some sr ->
                    match sr with
                    | RouteShape.OralFluid | RouteShape.IntravenousFluid ->
                         "ml[Volume"
                    | _ -> sprintf "%s[General]" shape
                | None  -> sprintf "%s[General]" shape

            let un =
                sprintf "%s[%s]" un ug
            let du =
                sprintf "%s[%s]" du ug
            let tu =
                sprintf "%s[Time]" tu

            let cdto = CDto.dto id name

            cdto.Items <- [ 
                for (s, _) in substs do
                    let idto = IDto.dto id s

                    idto.ComponentConcentration.Unit <- sprintf "%s/%s" un su
                    idto.ComponentQuantity.Unit <- un
                    idto.DoseQuantity.Unit <- du
                    idto.DoseTotalAdjust.Unit <- sprintf "%s/kg[Weight]/%s" du tu

                    idto                
            ]

            cdto.OrderableQuantity.Unit <- su
            cdto.OrderableConcentration.Unit <- "x[Count]"
            cdto.OrderQuantity.Unit <- su

            let odto = ODto.dto id name shape

            odto.OrderableQuantity.Unit <- su
            odto.OrderQuantity.Unit <- su

            odto.Components <- [ cdto ]

            let dto = Order.Dto.discontinuous id name shape route

            dto.Orderable <- odto

            dto.Prescription.Frequency.Unit <- sprintf "x[Count]/%s" tu
            dto.Adjust.Unit <- "kg[Weight]"

            dto
            |> Order.Dto.fromDto
            |> Order.solve name Mapping.ComponentComponentQty Props.Incr [ 1N/div ]
            |> Order.solve name Mapping.ComponentComponentQty Props.MaxIncl qty
            |> Order.solve name Mapping.OrderableDoseQty Props.Incr [ 1N/div ]
            |> fun o ->
                substs
                |> Seq.fold (fun acc (n, xs) ->
                    acc
                    |> Order.solve n Mapping.ItemComponentQty Props.Vals xs
                ) o
                
                
        type Drug =
            {
                Id:  string
                Name : string
                Substances : Substance list
                Quantity : BigRational
                UnitGroup : string
                Unit : string
                DoseUnit : string
                TimeUnit : string
                Shape : string
                Divisible : BigRational
                Route : string
            }
        and Substance = { Name : string; Quantity : BigRational list }

        let drug =
            {
                Id = ""
                Name = ""
                Substances = []
                Quantity = 1N
                UnitGroup = ""
                Unit = ""
                DoseUnit = ""
                TimeUnit = ""
                Shape = ""
                Divisible = 1N
                Route = ""
            }

        let toOrder (d : Drug) =
            create d.Id 
                   d.Name 
                   (d.Substances |> List.map (fun s -> s.Name, s.Quantity))
                   [d.Quantity]
                   d.UnitGroup
                   d.Unit
                   d.DoseUnit
                   d.TimeUnit
                   d.Shape
                   d.Divisible
                   d.Route

        type DoseLimits =
            {
                Name : string
                Frequencies : BigRational list
                MaxDoseQuantity : BigRational option
                MaxDoseTotal : BigRational option
                MaxDoseTotalAdjust : BigRational option
                MinDoseTotalAdjust : BigRational option
            }

        let doseLimits =
            {
                Name = ""
                Frequencies = []
                MaxDoseQuantity = None
                MaxDoseTotal = None
                MaxDoseTotalAdjust = None
                MinDoseTotalAdjust = None
            }

        let setConstraints constraints (o : Order.Order) =
            o
            |> Constraints.apply constraints o.Orderable.Shape

        let setDoseLimits (dl : DoseLimits) o =
            let set m p l o =
                match l with
                | Some l -> 
                    o
                    |> Order.solve dl.Name m p [ l ]
                | None -> o
                    
            o
            |> Order.solve dl.Name Mapping.Freq Props.Vals dl.Frequencies
            |> set Mapping.ItemDoseQty Props.MaxIncl dl.MaxDoseQuantity
            |> set Mapping.ItemDoseTotal Props.MaxIncl dl.MaxDoseTotal
            |> set Mapping.ItemDoseAdjustTotalAdjust Props.MaxIncl dl.MaxDoseTotalAdjust
            |> set Mapping.ItemDoseAdjustTotalAdjust Props.MinIncl dl.MinDoseTotalAdjust


        let setAdjust n a o =
            o
            |> Order.solve n Mapping.AdjustQty Props.Vals [a]



module Constraints = Drug.Constraints


// Paracetamol supp
{
    Drug.drug with
        Id = "1"
        Name = "paracetamol"
        Substances = 
            [ { Name = "paracetamol"; Quantity =  [60N; 120N; 240N; 500N; 1000N] }]
        UnitGroup = "Mass"
        Unit = "mg"
        DoseUnit = "mg"
        TimeUnit = "day"
        Shape = "supp"
        Route = "rect"
}
|> Drug.toOrder
|> Drug.setConstraints Constraints.constraints
|> Drug.setDoseLimits
    {   Drug.doseLimits with
            Name = "paracetamol"
            Frequencies = [ 2N .. 4N ]
            MaxDoseQuantity = Some 1000N
            MaxDoseTotal = Some 4000N
            MaxDoseTotalAdjust = Some 90N
    }
|> Drug.setAdjust "paracetamol" 10N
|> Order.toString 
|> List.iteri (fun i s -> printfn "%i\t%s" i s)


{
    Drug.drug with
        Id = "1"
        Name = "cotrimoxazol"
        Substances = 
            [ { Name = "sulfamethoxazol"; 
                Quantity = [ 20N; 80N; 160N ] }
              { Name = "trimethoprim"; 
                Quantity = [ 100N; 400N; 800N ] }
            ]
        UnitGroup = "Mass"
        Unit = "mg"
        DoseUnit = "mg"
        TimeUnit = "day"
        Shape = "tablet"
        Route = "or"
}
|> Drug.toOrder
|> Drug.setConstraints 
    (Constraints.constraints 
     |> List.map (fun c ->
        match c with
        | Constraints.SolidOralOrderableDoseQuantityMax _ ->
            Constraints.SolidOralOrderableDoseQuantityMax 2N
        | _ -> c
     ))
|> Drug.setDoseLimits
    {   Drug.doseLimits with
            Name = "sulfamethoxazol"
            Frequencies = [ 1N .. 4N ]
            MaxDoseQuantity = Some 800N
            MaxDoseTotal = Some 4800N
            MaxDoseTotalAdjust = Some 100N
    }
|> Drug.setAdjust "cotrimoxazol" 10N
|> Order.toString 
|> List.iteri (fun i s -> printfn "%i\t%s" i s)




// Paracetamol drink
{
    Drug.drug with
        Id = "1"
        Name = "paracetamol"
        Substances = [ { Name = "paracetamol"; Quantity = [ 24N ] }]
        UnitGroup = "Mass"
        Unit = "mg"
        DoseUnit = "mg"
        TimeUnit = "day"
        Shape = "drink"
        Route = "or"
}
|> Drug.toOrder
|> Drug.setConstraints Constraints.constraints
|> Drug.setDoseLimits
    {   Drug.doseLimits with
            Name = "paracetamol"
            Frequencies = [ 2N .. 4N ]
            MaxDoseQuantity = Some 1000N
            MaxDoseTotal = Some 4000N
            MaxDoseTotalAdjust = Some 90N
    }
|> Drug.setAdjust "paracetamol" 10N
|> Order.toString 
|> List.iteri (fun i s -> printfn "%i\t%s" i s)

