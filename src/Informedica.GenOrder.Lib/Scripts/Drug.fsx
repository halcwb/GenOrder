
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Utils.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Variable.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Equation.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Solver.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Dtos.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Api.fs"

#load "../DateTime.fs"
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


module Order =
    
    open Order
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenSolver.Lib

    module ValueRange = Variable.ValueRange
    module Frequency = VariableUnit.Frequency
    module Rate = VariableUnit.Rate
    module Quantity = VariableUnit.Quantity
    module TotalAdjust = VariableUnit.TotalAdjust
    module Dose = VariableUnit.Dose
    module DoseAdjust = VariableUnit.DoseAdjust
    module RateAdjust = VariableUnit.RateAdjust
    module Name = WrappedString.Name
    module Props = Solver.Props

    let calcScenarios (o : Order) =

        let calc get mo mc mi o =
            [

                // orderable doses
                let ods = 
                    o.Orderable.Dose
                    |> Dose.toVarUnt
                    |> get
                    |> VariableUnit.getUnitValues
                    |> Seq.distinct

                printfn "going to process %i orderable dose" (ods |> Seq.length)

                for v in ods do
                    let o =
                        o
                        |> Order.solve 
                            (o.Orderable.Name |> Name.toString)
                            mo Props.Vals [ v ]
        

                    // component doses
                    let cds =
                        o.Orderable.Components
                        |> Seq.collect (fun c ->
                            c.Dose
                            |> Dose.toVarUnt
                            |> get
                            |> VariableUnit.getUnitValues
                            |> Seq.map (fun v ->
                                c.Name, v
                            )
                        )
                        |> Seq.distinct

                    printfn "going to process %i component dose" (cds |> Seq.length)

                    for (n, v) in cds do
                        let o =
                            o
                            |> Order.solve 
                                (n |> Name.toString)
                                mc Props.Vals [ v ]
                

                        // item doses
                        let ids =
                            o.Orderable.Components
                            |> Seq.filter (fun c -> c.Name = n)
                            |> Seq.collect (fun c ->
                                c.Items
                                |> Seq.collect (fun i ->
                                    i.Dose
                                    |> Dose.toVarUnt
                                    |> get
                                    |> VariableUnit.getUnitValues
                                    |> Seq.map (fun v ->
                                        i.Name, v
                                    )
                                )
                            )
                            |> Seq.distinct

                        printfn "going to process %i item dose" (ids |> Seq.length)

                        for (n, v) in ids do
                            o
                            |> Order.solve 
                                (n |> Name.toString)
                                mi Props.Vals [ v ]
            
            ]
        
        match o.Prescription with
        | Prescription.Discontinuous fr ->

            [

                // frequencies
                let frs = fr |> Frequency.getUnitValues

                printfn "going to process %i freqs" (frs |> Seq.length)
                for v in frs do
                    o
                    |> Order.solve 
                        (o.Orderable.Name |> Name.toString) 
                        Mapping.Freq Props.Vals [v]
                    |> calc (fun (dq, _, _) -> dq)
                            Mapping.OrderableDoseQty
                            Mapping.ComponentDoseQty
                            Mapping.ItemDoseQty

            ]
            |> List.collect id

        | Prescription.Continuous ->
            o
            |> calc (fun (_, _, dr) -> dr)
                    Mapping.OrderableDoseRate
                    Mapping.ComponentDoseRate
                    Mapping.ItemDoseRate

        | _ -> []
//        |> (fun xs -> xs |> List.length |> printfn "found %i scenarios"; xs)
        |> List.distinct


    let printPrescription sn (o : Order) =
        let on = o.Orderable.Name |> Name.toString

        let printItmDose get unt o =
            o.Orderable.Components
            |> Seq.collect (fun c ->
                c.Items
                |> Seq.collect (fun i ->
                    let n = i.Name |> Name.toString
                    if sn |> Seq.exists ((=) n) then
                        i
                        |> get
                        |> unt 
                        |> Seq.map snd
                        |> fun xs ->
                            if on |> String.startsWith n then
                                xs 
                                |>Seq.map (sprintf "%s")
                            else
                                xs 
                                |> Seq.map (sprintf "%s %s" n)

                    else Seq.empty
                )
            )
            |> String.concat " + "


        match o.Prescription with
        | Prescription.Discontinuous fr ->
            // frequencies
            let fr =
                fr
                |> Frequency.toValueUnitStringList None
                |> Seq.map snd
                |> String.concat ";"

            let dq =
                o
                |> printItmDose 
                    (fun i -> i.Dose |> Dose.get |> (fun (dq, _, _) -> dq))
                    (VariableUnit.Quantity.toValueUnitStringList None)

            let dt =
                o
                |> printItmDose 
                    (fun i -> i.DoseAdjust |> DoseAdjust.get |> (fun (_, dt, _) -> dt))
                    (VariableUnit.TotalAdjust.toValueUnitStringList None)


            sprintf "%s %s %s = (%s)" (o.Orderable.Name |> Name.toString) fr dq dt

        | Prescription.Continuous ->
            // infusion rate
            let rt =
                o.Orderable.Dose
                |> Dose.get 
                |> fun (_, _, dr) ->
                    dr
                    |> Rate.toValueUnitStringList None
                    |> Seq.map snd
                    |> String.concat ""

            let oq =
                o.Orderable.OrderableQuantity
                |> Quantity.toValueUnitStringList None
                |> Seq.map snd
                |> String.concat ""

            let dq =
                o
                |> printItmDose
                    (fun i -> i.OrderableQuantity)
                    (Quantity.toValueUnitStringList None)

            let dr =
                o
                |> printItmDose 
                    (fun i -> i.DoseAdjust |> DoseAdjust.get |> (fun (_, _, dr) -> dr))
                    (VariableUnit.RateAdjust.toValueUnitStringList (Some 2))

            sprintf "%s %s in %s %s = %s" on dq oq rt dr

        | _ -> ""



// Creating a drug order
module DrugOrder =

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
                    "iv", "infusion fluid", IntravenousFluid
                ]

            let map route shape =
                mapping
                |> List.find (fun (r, s, _) -> r = route && s = shape )
                |> fun (_, _, x) -> x


        module Constraints =

            module Mapping = Order.Mapping
            module Props = Solver.Props
            module Name = WrappedString.Name
            
    
            type Constraint = 
                | SolidOralOrderableDoseQuantityMax of BigRational
                | SuppositoryOrderableDoseQuantity of BigRational
                | FluidOrderableDoseQuantityMax of BigRational
                | FluidOrderableDoseRateIncr of BigRational
                | FluidOrderableDoseRateMax of BigRational
                | FluidOrderableDoseRateAdjustedMax of BigRational
                | FluidOrderableDoseAdjustQuantityMax of BigRational
                | FluidOrderableQuantityIncrement of BigRational

            let constraints =
                [
                    SolidOralOrderableDoseQuantityMax 10N
                    SuppositoryOrderableDoseQuantity 1N
                    FluidOrderableDoseQuantityMax 30N
                    FluidOrderableDoseAdjustQuantityMax 5N
                    FluidOrderableDoseRateMax 20N
                    FluidOrderableDoseRateAdjustedMax 1N
                ]

            let apply cs shape (o : Order.Order) =
                let n = o.Orderable.Name |> Name.toString
                let os = RouteShape.map o.Route shape

                let solve c n m p v o = 
                    printfn "setting constraint %A" c
                    o
                    |> Order.solve n m p v 
                 

                cs
                |> List.fold (fun acc c ->
                    match c with
                    | FluidOrderableDoseQuantityMax x -> 
                        os
                        |> function
                        |  RouteShape.OralFluid |  RouteShape.IntravenousFluid ->
                            acc
                            |> solve c n Mapping.OrderableDoseQty Props.MaxIncl [x]
                        | _ -> acc
                    | FluidOrderableDoseRateIncr x -> 
                        os
                        |> function
                        |  RouteShape.OralFluid |  RouteShape.IntravenousFluid ->
                            acc
                            |> solve c n Mapping.OrderableDoseRate Props.Incr [x]
                        | _ -> acc
                    | FluidOrderableDoseRateMax x -> 
                        os
                        |> function
                        |  RouteShape.OralFluid |  RouteShape.IntravenousFluid ->
                            acc
                            |> solve c n Mapping.OrderableDoseRate Props.MaxIncl [x]
                        | _ -> acc
                    | FluidOrderableDoseRateAdjustedMax x -> 
                        os
                        |> function
                        |  RouteShape.OralFluid |  RouteShape.IntravenousFluid ->
                            acc
                            |> solve c n Mapping.OrderableDoseAdjustRateAdjust Props.MaxIncl [x]
                        | _ -> acc
                    | FluidOrderableDoseAdjustQuantityMax x -> 
                        os
                        |> function
                        |  RouteShape.OralFluid |  RouteShape.IntravenousFluid ->
                            acc
                            |> solve c n Mapping.OrderableDoseAdjustQtyAdjust Props.MaxIncl [x]
                        | _ -> acc
                    | FluidOrderableQuantityIncrement x ->
                        os
                        |> function 
                        | RouteShape.OralFluid | RouteShape.IntravenousFluid ->
                            acc.Orderable.Components
                            |> List.fold (fun acc c ->
                                let n = 
                                    c.Name
                                    |> Name.toString
                                acc
                                |> solve c n Mapping.ComponentOrderableQty Props.Incr [ x ]
                            ) acc
                        | _ -> acc
                    | SuppositoryOrderableDoseQuantity x -> 
                        os
                        |> function
                        |  RouteShape.RectalSolid ->
                            acc
                            |> solve c n Mapping.OrderableDoseQty Props.Vals [x]
                        | _ -> acc
                    | SolidOralOrderableDoseQuantityMax x -> 
                        os
                        |> function
                        |  RouteShape.OralSolid ->                
                            acc
                            |> solve c n Mapping.OrderableDoseQty Props.MaxIncl [x]
                        | _ -> acc

                ) o

        module Item = Orderable.Item
        module IDto = Item.Dto
        module Component = Orderable.Component
        module CDto = Component.Dto
        module ODto = Orderable.Dto

        module Mapping = Order.Mapping
        module Props = Solver.Props
        module Name = WrappedString.Name

        type OrderType =
            | Process
            | Continuous
            | Discontinuous
            | Timed

                
        type DrugOrder =
            {
                Id:  string
                Name : string
                Products : ProductComponent list
                Quantities :  BigRational list
                Unit : string
                TimeUnit : string
                RateUnit : string
                Shape : string
                Divisible : BigRational
                Route : string
                OrderType : OrderType
            }
        and ProductComponent = 
            { 
                Name : string
                Quantities : BigRational list
                TimeUnit : string
                RateUnit : string
                Substances: SubstanceItem list 
            }
        and SubstanceItem = 
            { 
                Name : string
                Concentrations : BigRational list
                OrderableQuantities : BigRational list
                Unit : string
                DoseUnit : string
                TimeUnit : string
                RateUnit : string
            }

        let drugOrder =
            {
                Id = ""
                Name = ""
                Products = []
                Quantities = []
                Unit = ""
                TimeUnit = ""
                RateUnit = "hour"
                Shape = ""
                Divisible = 1N
                Route = ""
                OrderType = Process
            }

        let productComponent =
            {
                Name = ""
                Quantities = []
                TimeUnit = "day"
                RateUnit = "hour"
                Substances = []
            }

        let substanceItem =
            {
                Name = ""
                Concentrations = []
                OrderableQuantities = []
                Unit = ""
                DoseUnit = ""
                TimeUnit = ""
                RateUnit = ""
            }


        let unitGroup u =
            ValueUnit.Units.units
            |> List.filter (fun ud ->
                ud.Group <> ValueUnit.Group.WeightGroup
            )
            |> List.tryFind (fun ud ->
                ud.Abbreviation.Dut::ud.Abbreviation.Eng::ud.Name.Dut::ud.Name.Eng::ud.Synonyms
                |> List.exists((=) u)
            )
            |> function 
            | Some ud -> 
                ud.Group 
                |> ValueUnit.Group.toString 
            | None -> "General"
            |> sprintf "%s[%s]" u
            

        let create (d : DrugOrder) =
            let ou = d.Unit |> unitGroup
            let odto = ODto.dto d.Id d.Name d.Shape

            odto.OrderableQuantity.Unit <- ou
            odto.OrderQuantity.Unit <- ou
            
            match d.OrderType with
            | Process -> ()
            | Continuous ->                
                odto.DoseRate.Unit <- 
                    d.RateUnit
                    |> unitGroup
                    |> sprintf "%s/%s" ou
            | Discontinuous ->                
                odto.DoseTotal.Unit <-
                    d.TimeUnit
                    |> unitGroup
                    |> sprintf "%s/%s" ou
            | Timed ->                
                odto.DoseTotal.Unit <-
                    d.TimeUnit
                    |> unitGroup
                    |> sprintf "%s/%s" ou
                odto.DoseRate.Unit <- 
                    d.RateUnit
                    |> unitGroup
                    |> sprintf "%s/%s" ou


            odto.Components <- 
                [
                    for p in d.Products do
                        let cdto = CDto.dto d.Id p.Name

                        cdto.Items <- [ 
                            for s in p.Substances do
                                let su = s.Unit |> unitGroup
                                let du = s.DoseUnit |> unitGroup
                                let tu = s.TimeUnit |> unitGroup

                                let idto = IDto.dto d.Id s.Name

                                idto.ComponentConcentration.Unit <- 
                                    sprintf "%s/%s" su ou
                                idto.ComponentQuantity.Unit <- su

                                match d.OrderType with
                                | Process -> ()
                                | Continuous ->
                                    idto.DoseRateAdjust.Unit <- 
                                        sprintf "%s/kg[Weight]/%s" du tu
                                | Discontinuous ->
                                    idto.DoseQuantity.Unit <- du
                                    idto.DoseTotalAdjust.Unit <- 
                                        p.TimeUnit
                                        |> unitGroup
                                        |> sprintf "%s/kg[Weight]/%s" du 
                                | Timed ->
                                    idto.DoseQuantity.Unit <- du
                                    idto.DoseTotalAdjust.Unit <- 
                                        p.TimeUnit
                                        |> unitGroup
                                        |> sprintf "%s/kg[Weight]/%s" du 
                                    idto.DoseRateAdjust.Unit <- 
                                        sprintf "%s/kg[Weight]/%s" du tu
                                
                                idto                
                        ]

                        cdto.OrderableQuantity.Unit <- ou
                        cdto.OrderableConcentration.Unit <- "x[Count]"
                        cdto.OrderQuantity.Unit <- ou

                        cdto                        
                ]

            let dto = 
                match d.OrderType with
                | Process ->
                    Order.Dto.``process`` d.Id d.Name d.Shape d.Route
                | Continuous ->
                    Order.Dto.continuous d.Id d.Name d.Shape d.Route
                | Discontinuous ->
                    Order.Dto.discontinuous d.Id d.Name d.Shape d.Route
                | Timed ->
                    Order.Dto.timed d.Id d.Name d.Shape d.Route

            dto.Orderable <- odto

            dto.Prescription.Frequency.Unit <- 
                sprintf "x[Count]/%s" (d.TimeUnit |> unitGroup)
            dto.Adjust.Unit <- "kg[Weight]"

            let rs = RouteShape.map d.Route d.Shape

            dto
            |> Order.Dto.fromDto
            |> Order.solve d.Name Mapping.OrderableDoseQty Props.Incr [ 1N / d.Divisible ]
            |> fun o ->
                match rs with
                | RouteShape.OralSolid
                | RouteShape.RectalSolid ->
                    o
                    |> Order.solve d.Name Mapping.OrderableOrderableQty Props.Vals [ 1N ]
                | _ -> 
                    o
                    |> Order.solve d.Name Mapping.OrderableOrderableQty Props.Vals d.Quantities
                            
            |> fun o ->
                d.Products
                |> Seq.fold (fun o p ->
                    let o =
                        match rs with
                        | RouteShape.OralSolid 
                        | RouteShape.RectalSolid
                        | _ when d.Products |> List.length = 1 ->
                            o
                            |> Order.solve p.Name Mapping.ComponentOrderableConc Props.Vals [ 1N ]
                        | _ -> o
                        |> Order.solve p.Name Mapping.ComponentComponentQty Props.Vals p.Quantities

                    p.Substances 
                    |> Seq.fold (fun o s ->
                        o
                        |> Order.solve s.Name Mapping.ItemComponentConc Props.Vals s.Concentrations 
                        |> Order.solve s.Name Mapping.ItemOrderableQty Props.Vals s.OrderableQuantities 
                        |> (fun o ->
                            match rs with
                            | RouteShape.OralSolid
                            | RouteShape.RectalSolid ->
                                o
                                |> Order.solve s.Name Mapping.ItemOrderableConc Props.Vals s.Concentrations
                            | _ -> o
                        )                   
                    ) o
                ) o
                

        type DoseLimits =
            {
                Name : string
                Frequencies : BigRational list
                SubstanceName : string
                MaxDoseQuantity : BigRational option
                MinDoseQuantity : BigRational option
                MaxDoseQuantityAdjust : BigRational option
                MinDoseQuantityAdjust : BigRational option
                MaxDoseTotal : BigRational option
                MinDoseTotal : BigRational option
                MaxDoseTotalAdjust : BigRational option
                MinDoseTotalAdjust : BigRational option
                MaxDoseRate : BigRational option
                MinDoseRate : BigRational option
                MaxDoseRateAdjust : BigRational option
                MinDoseRateAdjust : BigRational option
            }

        let doseLimits =
            {
                Name = ""
                Frequencies = []
                SubstanceName = ""
                MaxDoseQuantity = None
                MinDoseQuantity = None
                MinDoseQuantityAdjust = None
                MaxDoseQuantityAdjust = None
                MaxDoseTotal = None
                MinDoseTotal = None
                MaxDoseTotalAdjust = None
                MinDoseTotalAdjust = None
                MaxDoseRate = None
                MinDoseRate = None
                MaxDoseRateAdjust = None
                MinDoseRateAdjust = None
            }

        type SolutionLimits =
            {
                Name : string
                Component : string
                MinConcentration : BigRational option
                MaxConcentration : BigRational option
                DoseQuantityCount : BigRational option
                Increment : BigRational option
            }

        let solutionLimits =
            {
                Name = ""
                Component = ""
                MinConcentration = None
                MaxConcentration = None
                DoseQuantityCount = None
                Increment = Some 1N
            }

        let setConstraints constraints (o : Order.Order) =
            o
            |> Constraints.apply constraints o.Orderable.Shape

        let setDoseLimits (dl : DoseLimits) o =
            let set m p l o =
                match l with
                | Some l -> 
                    o
                    |> Order.solve dl.SubstanceName m p [ l ]
                | None -> o
                    
            o
            |> Order.solve dl.Name Mapping.Freq Props.Vals dl.Frequencies
            |> set Mapping.ItemDoseQty Props.MaxIncl dl.MaxDoseQuantity
            |> set Mapping.ItemDoseQty Props.MinIncl dl.MinDoseQuantity
            |> set Mapping.ItemDoseAdjustQtyAdjust Props.MaxIncl dl.MaxDoseQuantityAdjust
            |> set Mapping.ItemDoseAdjustQtyAdjust Props.MinIncl dl.MinDoseQuantityAdjust
            |> set Mapping.ItemDoseTotal Props.MaxIncl dl.MaxDoseTotal
            |> set Mapping.ItemDoseTotal Props.MinIncl dl.MinDoseTotal
            |> set Mapping.ItemDoseAdjustTotalAdjust Props.MaxIncl dl.MaxDoseTotalAdjust
            |> set Mapping.ItemDoseAdjustTotalAdjust Props.MinIncl dl.MinDoseTotalAdjust
            |> set Mapping.ItemDoseRate Props.MaxIncl dl.MaxDoseRate
            |> set Mapping.ItemDoseRate Props.MinIncl dl.MinDoseRate
            |> set Mapping.ItemDoseAdjustRateAdjust Props.MaxIncl dl.MaxDoseRateAdjust
            |> set Mapping.ItemDoseAdjustRateAdjust Props.MinIncl dl.MinDoseRateAdjust


        let setSolutionLimits (sl : SolutionLimits) (o : Order.Order) =
            let set n m p l o =
                match l with
                | Some l -> 
                    o
                    |> Order.solve n m p [ l ]
                | None -> o

            
            o
            |> set sl.Name Mapping.ItemOrderableConc Props.MinIncl sl.MinConcentration
            |> set sl.Name Mapping.ItemOrderableConc Props.MaxIncl sl.MaxConcentration
            |> fun o ->

                let dq = 
                    o.Orderable.Components
                    |> List.filter (fun c ->
                        c.Items
                        |> List.exists (fun i -> i.Name |> Name.toString = sl.Name)
                    )
                    |> List.collect (fun c ->
                        c.Dose
                        |> VariableUnit.Dose.toVarUnt
                        |> (fun (dq, _, _) ->
                            dq
                            |> VariableUnit.getUnitValues
                            |> Seq.toList
                            |> List.map (fun v -> c.Name, v)
                        )
                    )
                printfn "collected %i component dose qtys" (dq |> List.length)

                match dq with
                | (n, _)::_ -> 
                    o
                    |> Order.solve (n |> Name.toString)
                                   Mapping.ComponentOrderableQty Props.Vals
                                   (dq |> List.map snd)
                | _ -> o
            |> fun o ->
                let dq =
                    o.Orderable.Dose
                    |> VariableUnit.Dose.getQuantity
                    |> VariableUnit.Quantity.getUnitValues
                    |> Seq.toList

                o
                |> Order.solve (o.Orderable.Name |> Name.toString)
                               Mapping.OrderableOrderableQty Props.Vals
                               dq
            |> set sl.Component Mapping.ComponentOrderableQty Props.Incr sl.Increment


        let setAdjust n a o =
            o
            |> Order.solve n Mapping.AdjustQty Props.Vals [a]


module Constraints = DrugOrder.Constraints


// Paracetamol supp
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "paracetamol"
        Products = 
            [
                { 
                    DrugOrder.productComponent with 
                        Name = "paracetamol"
                        Quantities = [ 1N ]
                        TimeUnit = "day"
                        Substances =
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "paracetamol"
                                        Concentrations = 
                                            [ 60N; 120N; 240N; 500N; 1000N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]
                }
            ]
        Unit = "piece"
        TimeUnit = "day"
        Shape = "supp"
        Route = "rect"
        OrderType = DrugOrder.Discontinuous
}
|> DrugOrder.create
|> DrugOrder.setConstraints Constraints.constraints
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "paracetamol"
            Frequencies = [ 2N .. 4N ]
            SubstanceName = "paracetamol"
            MaxDoseQuantity = Some 1000N
            MaxDoseTotal = Some 4000N
            MaxDoseTotalAdjust = Some 90N
    }
|> DrugOrder.setAdjust "paracetamol" 10N
//|> Order.printPrescriptions "paracetamol"
|> Order.calcScenarios
//|> List.length
|> List.iteri (fun i o ->
    o
    |> Order.printPrescription ["paracetamol"]
    |> printfn "%i\t%s" (i + 1)
)


// Drug with multiple items
// cotrimoxazol for infection
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "cotrimoxazol"
        Products = 
            [
                { 
                    DrugOrder.productComponent with 
                        Name = "cotrimoxazol"
                        Quantities = [ 1N ]
                        TimeUnit = "day"
                        Substances =
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sulfamethoxazol"
                                        Concentrations = 
                                            [ 100N; 400N; 800N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "trimethoprim"
                                        Concentrations = 
                                            [ 20N; 80N; 160N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]
                }
            ]
        Unit = "piece"
        TimeUnit = "day"
        Shape = "tablet"
        Route = "or"
        OrderType = DrugOrder.Discontinuous
}
|> DrugOrder.create
|> DrugOrder.setConstraints 
    (Constraints.constraints 
     |> List.map (fun c ->
        match c with
        | Constraints.SolidOralOrderableDoseQuantityMax _ ->
            Constraints.SolidOralOrderableDoseQuantityMax 2N
        | _ -> c
     ))
// setting dose limits for infection
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "cotrimoxazol"
            Frequencies = [ 2N ]
            SubstanceName = "sulfamethoxazol"
            MaxDoseTotal = Some 1600N
            MaxDoseTotalAdjust = Some 30N
    }
|> DrugOrder.setAdjust "cotrimoxazol" 10N
// is not be necessary when a single product is chosen
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "cotrimoxazol"
            Frequencies = [ 2N ]
            SubstanceName = "trimethoprim"
            MaxDoseTotal = Some 320N
            MaxDoseTotalAdjust = Some 6N
    }
|> Order.calcScenarios
//|> List.length
|> List.iteri (fun i o ->
    o
    |> Order.printPrescription ["sulfamethoxazol"; "trimethoprim"]
    |> printfn "%i\t%s" (i + 1)
)



// Paracetamol drink
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "paracetamol"
        Products = 
            [
                { 
                    DrugOrder.productComponent with 
                        Name = "paracetamol"
                        Quantities = [ 100N ]
                        TimeUnit = "day"
                        Substances =
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "paracetamol"
                                        Concentrations = [ 24N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]
                }
            ]
        Unit = "ml"
        TimeUnit = "day"
        Shape = "drink"
        Route = "or"
        OrderType = DrugOrder.Discontinuous
}
|> DrugOrder.create
|> DrugOrder.setAdjust "paracetamol" 20N
|> DrugOrder.setConstraints Constraints.constraints
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "paracetamol"
            Frequencies = [ 2N .. 4N ]
            SubstanceName = "paracetamol"
            MaxDoseQuantity = Some 1000N
            MaxDoseTotal = Some 4000N
            MinDoseTotalAdjust = Some 20N
            MaxDoseTotalAdjust = Some 90N
    }
|> Order.calcScenarios
//|> List.length
|> List.iteri (fun i o ->
    o
    |> Order.printPrescription ["paracetamol"]
    |> printfn "%i\t%s" (i + 1)
)




// Drug with multiple items
// cotrimoxazol drink for infection
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "cotrimoxazol"
        Products = 
            [
                { 
                    DrugOrder.productComponent with 
                        Name = "cotrimoxazol"
                        Quantities = [ 1N ]
                        TimeUnit = "day"
                        Substances =
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sulfamethoxazol"
                                        Concentrations = 
                                            [ 40N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "trimethoprim"
                                        Concentrations = 
                                            [ 8N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]
                }
            ]
        Unit = "ml"
        TimeUnit = "day"
        Shape = "drink"
        Route = "or"
        OrderType = DrugOrder.Discontinuous
}
|> DrugOrder.create
|> DrugOrder.setConstraints 
    (Constraints.constraints 
     |> List.map (fun c ->
        match c with
        | Constraints.SolidOralOrderableDoseQuantityMax _ ->
            Constraints.SolidOralOrderableDoseQuantityMax 2N
        | _ -> c
     ))
// setting dose limits for infection
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "cotrimoxazol"
            Frequencies = [ 2N ]
            SubstanceName = "sulfamethoxazol"
            MaxDoseTotal = Some 1600N
            MaxDoseTotalAdjust = Some 30N
    }
|> DrugOrder.setAdjust "cotrimoxazol" 10N
// is not be necessary when a single product is chosen
//|> DrugOrder.setDoseLimits
//    {   DrugOrder.doseLimits with
//            Name = "cotrimoxazol"
//            Frequencies = [ 2N ]
//            SubstanceName = "trimethoprim"
//            MaxDoseTotal = Some 320N
//            MaxDoseTotalAdjust = Some 6N
//    }
|> Order.calcScenarios
//|> List.length
|> List.iteri (fun i o ->
    o
    |> Order.printPrescription ["sulfamethoxazol"; "trimethoprim"]
    |> printfn "%i\t%s" (i + 1)
)




// Dopamin infusion calculate scenario's 
// with a number of standard solutions
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "dopamin infusion"
        Quantities = [ 50N ]
        Unit = "ml"
        TimeUnit = "day"
        Shape = "infusion fluid"
        Route = "iv"
        Products = 
            [
                { 
                    DrugOrder.productComponent with
                        Name = "dopamin"
                        Quantities = [ 5N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "dopamin"
                                        Concentrations = [ 40N ]
                                        OrderableQuantities = [ 80N; 200N; 400N ]
                                        Unit = "mg"
                                        DoseUnit = "mcg"
                                        TimeUnit = "min"
                                }
                            ]

                }
                { 
                    DrugOrder.productComponent with
                        Name = "saline"
                        Quantities = [ 5000N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sodium"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "chloride"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                            ]

                }
            ]
        OrderType = DrugOrder.Continuous
}
|> DrugOrder.create
|> Order.Dto.toDto
|> Order.Dto.setToContinuous
|> Order.Dto.fromDto
|> DrugOrder.setAdjust "dopamin infusion" 10N
|> DrugOrder.setConstraints Constraints.constraints
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "dopamin infusion"
            SubstanceName = "dopamin"
            MinDoseRateAdjust = Some 2N
            MaxDoseRateAdjust = Some 20N
    }
|> DrugOrder.setAdjust "dopamin infusion" 10N
|> DrugOrder.setConstraints [ DrugOrder.Constraints.FluidOrderableDoseRateIncr (1N / 10N) ]
//|> Order.printPrescriptions "dopamin"
//|> List.iteri (fun i s -> printfn "%i\t%s" (i + 1) s)
|> Order.calcScenarios
|> List.sort
//|> List.length
|> List.iteri (fun i o ->
    o
    |> Order.printPrescription ["dopamin"]
    |> printfn "%i\t%s" (i + 1)
)




// Dopamin infusion calculate scenario's 
// with a a fixed infusion - dose rate
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "dopamin infusion"
        Quantities = [ 50N ]
        Unit = "ml"
        TimeUnit = "day"
        Shape = "infusion fluid"
        Route = "iv"
        Products = 
            [
                { 
                    DrugOrder.productComponent with
                        Name = "dopamin"
                        Quantities = [ 5N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "dopamin"
                                        Concentrations = [ 40N ]
//                                        OrderableQuantities = [ 80N; 200N; 400N ]
                                        Unit = "mg"
                                        DoseUnit = "mcg"
                                        TimeUnit = "min"
                                }
                            ]

                }
                { 
                    DrugOrder.productComponent with
                        Name = "saline"
                        Quantities = [ 5000N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sodium"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "chloride"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                            ]

                }
            ]
        OrderType = DrugOrder.Continuous
}
|> DrugOrder.create
|> Order.Dto.toDto
|> Order.Dto.setToContinuous
|> Order.Dto.fromDto
|> DrugOrder.setAdjust "dopamin infusion" 10N
|> DrugOrder.setConstraints Constraints.constraints
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "dopamin infusion"
            SubstanceName = "dopamin"
//            MinDoseRateAdjust = Some 1N
            MaxDoseRateAdjust = Some 20N
    }
|> DrugOrder.setAdjust "dopamin infusion" 10N
|> Order.solve "dopamin infusion" 
    Order.Mapping.OrderableDoseRate 
    Solver.Props.Vals [ 1N ]
|> Order.solve "dopamin" 
    Order.Mapping.ComponentOrderableQty 
    Solver.Props.Incr [ 1N/10N ]
//|> Order.printPrescriptions "dopamin"
//|> List.iteri (fun i s -> printfn "%i\t%s" (i + 1) s)
|> Order.calcScenarios
|> List.sort
//|> List.length
|> List.iteri (fun i o ->
    o
    |> Order.printPrescription ["dopamin"]
    |> printfn "%i\t%s" (i + 1)
)




// gentamicin
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "gentamicin"
        Quantities = [ ]
        Unit = "ml"
        TimeUnit = "day"
        Shape = "infusion fluid"
        Route = "iv"
        Products = 
            [
                { 
                    DrugOrder.productComponent with
                        Name = "gentamicin"
                        Quantities = [ 2N; 10N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "gentamicin"
                                        Concentrations = [ 10N; 40N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]

                }
                { 
                    DrugOrder.productComponent with
                        Name = "saline"
                        Quantities = [ 5000N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sodium"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "chloride"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                            ]

                }

            ]
        OrderType = DrugOrder.Timed
    }
|> DrugOrder.create
|> Order.Dto.toDto
//|> Order.Dto.setToTimed
|> Order.Dto.fromDto
|> DrugOrder.setAdjust "gentamicin" 10N
|> DrugOrder.setConstraints Constraints.constraints
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "gentamicin"
            SubstanceName = "gentamicin"
            Frequencies = [ 1N ]
            MinDoseTotalAdjust = Some 5N
            MaxDoseTotalAdjust = Some 8N
    }
|> DrugOrder.setSolutionLimits 
    {
        DrugOrder.solutionLimits with
            Name = "gentamicin"
            Component = "gentamicin"
            MinConcentration = Some (1N)
            MaxConcentration = Some (2N)
            DoseQuantityCount = Some (1N)

    }
//|> Order.solve "gentamicin" 
//               Order.Mapping.ComponentOrderableQty 
//               Solver.Props.Incr [1N / 10N]
|> Order.toString
|> List.iteri (fun i s -> printfn "%i\t%s" (i + 1) s)
//|> Order.calcScenarios
//|> List.sort
////|> List.length
//|> List.iteri (fun i o ->
//    o
//    |> Order.printPrescription ["gentamicin"]
//    |> printfn "%i\t%s" (i + 1)
//)
