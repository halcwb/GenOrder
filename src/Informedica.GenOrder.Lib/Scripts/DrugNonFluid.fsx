
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
   
open MathNet.Numerics

open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

module Units = ValueUnit.Units


module OrderConstraints =

    module Mapping = Order.Mapping
    module Props = Solver.Props
    module Name = WrappedString.Name

    type OrderableShape =
        | IntravenousFluid
        | OralFluid
        | OralSolid
        | RectalSolid
    
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
        | FluidOrderableDoseQuantityMax of BigRational
        | FluidOrderableDoseRateMin of BigRational
        | FluidOrderableDoseRateMax of BigRational
        | FluidOrderableDoseAdjustQuantityMax of BigRational
        | SuppositoryOrderableDoseQuantity of BigRational
        | SolidOralOrderableDoseQuantityMax of BigRational

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

    let applyConstraints cs os (o : Order.Order) =
        let n = o.Orderable.Name |> Name.toString

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
                | OralSolid | RectalSolid ->
                    acc
                    |> Order.solve n Mapping.ComponentOrderableCount Props.Vals [x]
                | _ -> acc
            | SolidComponentOrderableConcentration x ->
                os
                |> function
                | OralSolid | RectalSolid ->
                    acc
                    |> Order.solve n Mapping.ComponentOrderableConc Props.Vals [x]
                | _ -> acc
            | SolidComponentComponentQuantityMax x ->
                os
                |> function
                | OralSolid | RectalSolid ->
                    acc
                    |> Order.solve n Mapping.ComponentComponentQty Props.MaxIncl [x]
                | _ -> acc
            | FluidOrderableDoseQuantityMax x -> 
                os
                |> function
                | OralFluid | IntravenousFluid ->
                    acc
                    |> Order.solve n Mapping.OrderableDoseQty Props.MaxIncl [x]
                | _ -> acc
            | FluidOrderableDoseRateMin x -> 
                os
                |> function
                | OralFluid | IntravenousFluid ->
                    acc
                    |> Order.solve n Mapping.OrderableDoseRate Props.MinIncl [x]
                | _ -> acc
            | FluidOrderableDoseRateMax x -> 
                os
                |> function
                | OralFluid | IntravenousFluid ->
                    acc
                    |> Order.solve n Mapping.OrderableDoseRate Props.MaxIncl [x]
                | _ -> acc
            | FluidOrderableDoseAdjustQuantityMax x -> 
                os
                |> function
                | OralFluid | IntravenousFluid ->
                    acc
                    |> Order.solve n Mapping.OrderableDoseAdjustQtyAdjust Props.MaxIncl [x]
                | _ -> acc
            | SuppositoryOrderableDoseQuantity x -> 
                os
                |> function
                | RectalSolid ->
                    acc
                    |> Order.solve n Mapping.OrderableDoseQty Props.Vals [x]
                | _ -> acc
            | SolidOralOrderableDoseQuantityMax x -> 
                os
                |> function
                | OralSolid ->                
                    acc
                    |> Order.solve n Mapping.OrderableDoseQty Props.MaxIncl [x]
                | _ -> acc

        ) o


// Creating a non fluid drug
module DrugNonFluid =

        module Item = Orderable.Item
        module IDto = Item.Dto
        module Component = Orderable.Component
        module CDto = Component.Dto
        module ODto = Orderable.Dto

        module Mapping = Order.Mapping
        module Props = Solver.Props

        let create id name substs ug un du tu shape div route =
            let su = 
                sprintf "%s[General]" shape
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

            let odto = ODto.dto id name

            odto.OrderableQuantity.Unit <- su
            odto.OrderQuantity.Unit <- su

            odto.Components <- [ cdto ]

            let dto = Order.Dto.discontinuous id name route

            dto.Orderable <- odto

            dto.Prescription.Frequency.Unit <- sprintf "x[Count]/%s" tu
            dto.Adjust.Unit <- "kg[Weight]"

            dto
            |> Order.Dto.fromDto
            |> Order.solve name Mapping.ComponentComponentQty Props.Incr [ 1N/div ]
            |> fun o ->
                substs
                |> Seq.fold (fun acc (n, xs) ->
                    acc
                    |> Order.solve n Mapping.ItemComponentConc Props.Vals xs
                ) o
            |> fun o ->
                match route with
                | _ when route = "rect" ->
                    o
                    |> OrderConstraints.applyConstraints 
                        OrderConstraints.constraints 
                        OrderConstraints.RectalSolid
                | _ -> o
                
                
        type Drug =
            {
                Id:  string
                Name : string
                Substances : Substance list
                UnitGroup : string
                Unit : string
                DoseUnit : string
                TimeUnit : string
                Shape : string
                Divisible : BigRational
                Route : string
            }
        and Substance = { Name : string; Concentrations : BigRational list }

        let drug =
            {
                Id = ""
                Name = ""
                Substances = []
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
                   (d.Substances |> List.map (fun s -> s.Name, s.Concentrations))
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



// Paracetamol supp
{
    DrugNonFluid.drug with
        Id = "1"
        Name = "paracetamol"
        Substances = [ { Name = "paracetamol"; Concentrations = [60N; 120N; 240N; 500N; 1000N] }]
        UnitGroup = "Mass"
        Unit = "mg"
        DoseUnit = "mg"
        TimeUnit = "day"
        Shape = "supp"
        Route = "rect"
}
|> DrugNonFluid.toOrder
|> DrugNonFluid.setDoseLimits
    {   DrugNonFluid.doseLimits with
            Name = "paracetamol"
            Frequencies = [ 2N .. 4N ]
            MaxDoseQuantity = Some 1000N
            MaxDoseTotal = Some 4000N
            MaxDoseTotalAdjust = Some 90N
    }
// |> DrugNonFluid.setAdjust "paracetamol" 10N
|> Order.toString 
|> List.iteri (fun i s -> printfn "%i\t%s" i s)


{
    DrugNonFluid.drug with
        Id = "1"
        Name = "cotrimoxazol"
        Substances = 
            [ { Name = "sulfamethoxazol"; Concentrations = [20N; 80N; 160N] }
              { Name = "trimethoprim"; Concentrations = [100N; 400N; 800N] }
            ]
        UnitGroup = "Mass"
        Unit = "mg"
        DoseUnit = "mg"
        TimeUnit = "day"
        Shape = "tablet"
        Route = "or"
}
|> DrugNonFluid.toOrder
|> Order.toString 
|> List.iteri (fun i s -> printfn "%i\t%s" i s)

