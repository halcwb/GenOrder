
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Utils.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Logger.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Variable.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Equation.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Solver.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Constraint.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Api.fs"

#load "../DateTime.fs"
#load "../WrappedString.fs"
#load "../List.fs"
#load "../Logger.fs"
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

    module Variable = Informedica.GenSolver.Lib.Variable
    module ValueRange = Variable.ValueRange
    module Frequency = VariableUnit.Frequency
    module Time = VariableUnit.Time
    module Rate = VariableUnit.Rate
    module Quantity = VariableUnit.Quantity
    module TotalAdjust = VariableUnit.TotalAdjust
    module Dose = VariableUnit.Dose
    module DoseAdjust = VariableUnit.DoseAdjust
    module RateAdjust = VariableUnit.RateAdjust
    module Concentration = VariableUnit.Concentration
    module Name = WrappedString.Name
    module Props = Informedica.GenSolver.Lib.Props
    module Solver = Informedica.GenOrder.Lib.Solver

    type Component = Orderable.Component.Component

    let calcScenarios log (o : Order) =

        let solve n v o =
            try 
                o
                |> Order.toEqs
                |> function 
                | (prod, sum) ->
                    prod 
                    |> List.map Solver.productEq
                    |> List.append (sum |> List.map Solver.sumEq)
    
                |> Solver.solve log None n (v |> Set.singleton |> Props.Vals)
                |> Order.fromEqs o
                |> Some
            with
            | e -> 
                e.ToString()
                |> sprintf "could not solve %A: %A\n%s" v n 
                |> failwith
                None

        let smallest o =
            o
            |> Order.toEqs
            |> function
            | (vrus1, vrus2) ->
                vrus1
                |> List.append vrus2
                |> List.collect id
                |> List.filter (fun vru ->
                    vru
                    |> VariableUnit.getBaseValues
                    |> Seq.length > 1
                )
            |> List.map (fun vru ->
                vru.Variable.Name, vru |> VariableUnit.getUnitValues 
            )
            |> function 
            | [] -> None
            | xs ->
                xs
                |> List.sortBy (fun (_, vs) -> vs |> Seq.length)
                |> List.tryHead

        // To do add logger    
        let rec calc os sc =
            
            match sc with
            | None         -> 
                os
            | Some (n, vs) ->
                let msg = 
                    (vs |> Seq.map BigRational.toString |> String.concat ",")
                    |> sprintf "scenario: %A, with %A" n

                Logger.Scenario
                |> Logger.createMessage msg
                |> Logger.logInfo log

                [
                    for v in vs do
                        for o in os do
                            if o |> Order.contains n v then
                                Logger.ScenerioValue
                                |> Logger.createMessage (o, v)
                                |> Logger.logInfo log

                                let o =
                                    o
                                    |> solve n v

                                if o |> Option.isSome then 
                                    o
                                    |> Option.get

                ]     
                |> List.map (fun o ->
                    o
                    |> smallest
                    |> calc [o]
                )
                |> List.collect id
                |> List.distinct

        o
        |> smallest
        |> calc [ o ]


    let printPrescription sn (o : Order) =
        let on = o.Orderable.Name |> Name.toString

        let printItemConc (c : Component) =
            c.Items
            |> Seq.collect (fun i ->
                i.ComponentConcentration
                |> Concentration.toValueUnitStringList None
                |> Seq.map (fun (_, s) ->
                    i.Name
                    |> Name.toString
                    |> sprintf "%s %s" s
                )
            )
            |> String.concat " + "


        let printCmpQuantity o =
            o.Orderable.Components
            |> Seq.map (fun c ->
                c.OrderableQuantity
                |> Quantity.toValueUnitStringList None
                |> Seq.map (fun (_, q) ->
                    c 
                    |> printItemConc
                    |> sprintf "%s %s (%s)" q (c.Name |> Name.toString)
                )
                |> String.concat ""
            ) |> String.concat " + "


        let printItem get unt o =
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
                |> printItem 
                    (fun i -> i.Dose |> Dose.get |> (fun (dq, _, _) -> dq))
                    (VariableUnit.Quantity.toValueUnitStringList None)

            let dt =
                o
                |> printItem 
                    (fun i -> i.DoseAdjust |> DoseAdjust.get |> (fun (_, dt, _) -> dt))
                    (VariableUnit.TotalAdjust.toValueUnitStringList None)

            let p =
                sprintf "%s %s %s = (%s)" (o.Orderable.Name |> Name.toString) fr dq dt

            let d =
                o
                |> printCmpQuantity
                |> sprintf "%s" 

            p, d

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

            let it =
                o
                |> printItem
                    (fun i -> i.OrderableQuantity)
                    (Quantity.toValueUnitStringList None)

            let dr =
                o
                |> printItem 
                    (fun i -> i.DoseAdjust |> DoseAdjust.get |> (fun (_, _, dr) -> dr))
                    (VariableUnit.RateAdjust.toValueUnitStringList (Some 2))
            let p =
                sprintf "%s %s in %s %s = %s" on it oq rt dr

            let d =
                o
                |> printCmpQuantity
                |> sprintf "%s" 
            
            p, d

        | Prescription.Timed (fr, tme) ->

            // frequencies
            let fr =
                fr
                |> Frequency.toValueUnitStringList None
                |> Seq.map snd
                |> String.concat ";"

            let tme =
                tme
                |> Time.toValueUnitStringList (Some 2)
                |> Seq.map snd
                |> String.concat ""

            let dq =
                o
                |> printItem 
                    (fun i -> i.Dose |> Dose.get |> (fun (dq, _, _) -> dq))
                    (VariableUnit.Quantity.toValueUnitStringList None)

            let od =
                o.Orderable.Dose
                |> Dose.get
                |> fun (q, _, _) ->
                    q
                    |> Quantity.toValueUnitStringList None
                    |> Seq.map snd
                    |> String.concat ""

            let dt =
                o
                |> printItem 
                    (fun i -> i.DoseAdjust |> DoseAdjust.get |> (fun (_, dt, _) -> dt))
                    (VariableUnit.TotalAdjust.toValueUnitStringList None)

            let p =
                sprintf "%s %s %s = (%s) in %s in %s %s"  
                    (o.Orderable.Name |> Name.toString) fr dq dt od tme o.Route
            let d =
                o
                |> printCmpQuantity
                |> sprintf "%s" 

            (p, d)


        | Prescription.Process ->
            let p =
                o.Orderable.Name
                |> Name.toString
                |> sprintf "%s"  
            p, ""


// Creating a drug order
module DrugOrder =

        open Informedica.GenUtils.Lib

        module OrderType =
            type Order = Order.Order
            
            type OrderType =
                | Any
                | Process
                | Continuous
                | Discontinuous
                | Timed

            let map (o : Order) =
                match o.Prescription with
                | Prescription.Process -> Process
                | Prescription.Discontinuous _ -> Discontinuous
                | Prescription.Continuous -> Continuous
                | Prescription.Timed (_, _) -> Timed

        module RouteShape =

            type RouteShape =
                | Any
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


        module DrugConstraint =

            module Name = WrappedString.Name
            module Mapping = Order.Mapping
            module Props = Informedica.GenSolver.Lib.Props
            module Constraint = Informedica.GenSolver.Lib.Constraint

            type Name = Name.Name
            type Mapping = Order.Mapping.Map
            type RouteShape = RouteShape.RouteShape
            type OrderType = OrderType.OrderType
            type Order = Order.Order
            type Constraint = Informedica.GenSolver.Lib.Constraint.Constraint
            type Property = Props.Property
            type Limit = Constraint.Limit
            type Logger = Informedica.GenSolver.Lib.Logger.Logger

            type DrugConstraint =
                {
                    Name : string
                    Mapping : Mapping
                    Property : Property
                    Limit : Limit
                    RouteShape : RouteShape
                    OrderType : OrderType
                }

            let create n m p l rs ps = 
                {
                    Name = n
                    Mapping = m
                    Property = p
                    Limit = l
                    RouteShape = rs
                    OrderType = ps
                }

            let mapToConstraint o (dc : DrugConstraint) = 
                {
                    Constraint.Name = 
                        Order.mapName dc.Name dc.Mapping o
                    Constraint.Property = dc.Property
                    Constraint.Limit = dc.Limit
                }

            let toString (c : DrugConstraint) =
                sprintf "%A %A %A %A" c.Name c.Mapping c.Property

            let constraints n =
                let dr =
                    [(1N/10N)..(1N/10N)..10N] 
                    |> List.append [11N..1N..100N]
                    |> List.append [105N..5N..1000N]
                    |> Set.ofList

                let c m p vs rs ps =
                    create n m p vs rs ps
                // list of general orderable constraints
                [
                    // ALL
                    c Mapping.AdjustQty
                      (Props.MaxIncl 650N) Constraint.NoLimit
                      RouteShape.Any OrderType.Any

                    c Mapping.AdjustQty 
                      (Props.MinIncl (250N/1000N)) Constraint.NoLimit
                      RouteShape.Any OrderType.Any

                    c Mapping.AdjustQty
                      ((50N/1000N) |> Set.singleton |> Props.Increment) Constraint.NoLimit
                      RouteShape.Any OrderType.Any

                    // == Oral Solid ==
                    // == Discontinuous ==
                    // give max 10 pieces each time
                    c Mapping.OrderableDoseQty 
                      (Props.MaxIncl 10N) Constraint.NoLimit
                      RouteShape.OralSolid OrderType.Discontinuous

                    // == Rectal Solid ==
                    // == Discontinuous ==
                    // Give 1 piece each time
                    c Mapping.OrderableDoseQty 
                      (Props.MaxIncl 1N)  Constraint.NoLimit
                      RouteShape.RectalSolid OrderType.Discontinuous

                    // == Oral Fluid ==
                    // == Discontinuous ==
                    // give the total orderable quantity each time
                    c Mapping.OrderableDoseCount 
                      (1N |> Set.singleton |> Props.Vals) Constraint.NoLimit
                      RouteShape.OralFluid OrderType.Discontinuous
                    // give max 500 ml each time
                    c Mapping.OrderableDoseQty 
                      (Props.MaxIncl 500N) Constraint.NoLimit
                      RouteShape.OralFluid OrderType.Discontinuous
                    // give max 10 ml/kg each time
                    c Mapping.OrderableDoseAdjustQtyAdjust 
                      (Props.MaxIncl 10N) Constraint.NoLimit
                      RouteShape.OralFluid OrderType.Discontinuous
                    // give max 500 ml each time

                    // == Oral Fluid ==
                    // == Timed ==
                    // Give max 500 ml each time
                    c Mapping.OrderableDoseQty 
                      (Props.MaxIncl 500N) Constraint.NoLimit
                      RouteShape.OralFluid OrderType.Timed
                    // give max 10 ml/kg each time
                    c Mapping.OrderableDoseAdjustQtyAdjust 
                      (Props.MaxIncl 10N) Constraint.NoLimit 
                      RouteShape.OralFluid OrderType.Timed

                    // == Oral Fluid ==
                    // == Continuous ==
                    // Max dose rate is 200 ml/hour
                    c Mapping.OrderableDoseRate 
                      (Props.MaxIncl 200N) Constraint.NoLimit
                      RouteShape.OralFluid OrderType.Continuous
                    // Max dose rate per kg is 5 ml/kg/hour
                    c Mapping.OrderableDoseRate 
                      (Props.MaxIncl 5N) Constraint.NoLimit
                      RouteShape.OralFluid OrderType.Continuous
                    // Set dose rate values
                    c Mapping.OrderableDoseRate 
                      (Props.Vals dr) Constraint.NoLimit
                      RouteShape.OralFluid OrderType.Continuous

                    // == Intravenuous Fluid ==
                    // == Discontinuous ==
                    // Give max 1000 ml each time
                    c Mapping.OrderableDoseQty 
                      (Props.MaxIncl 1000N) Constraint.NoLimit
                      RouteShape.IntravenousFluid OrderType.Discontinuous
                    // Give max 20 ml/kg each time
                    c Mapping.OrderableDoseAdjustQtyAdjust 
                      (Props.MaxIncl 20N) Constraint.NoLimit
                      RouteShape.IntravenousFluid OrderType.Discontinuous

                    // == Intravenuous Fluid ==
                    // == Timed ==
                    // Give max 1000 ml each time
                    c Mapping.OrderableDoseQty 
                      (Props.MaxIncl 1000N) Constraint.NoLimit
                      RouteShape.IntravenousFluid OrderType.Timed
                    // Give max 20 ml/kg each time
                    c Mapping.OrderableDoseAdjustQtyAdjust 
                      (Props.MaxIncl 20N) Constraint.NoLimit
                      RouteShape.IntravenousFluid OrderType.Timed
                    // Select 1 possible value from dose rates
                    c Mapping.OrderableDoseRate 
                      (Props.Vals dr) (Constraint.MaxLim 1)
                      RouteShape.IntravenousFluid OrderType.Timed

                    // == Intravenuous Fluid ==
                    // == Continuous ==
                    // Max dose rate is 200 ml/hour
                    c Mapping.OrderableDoseRate 
                      (Props.MaxIncl 200N) Constraint.NoLimit
                      RouteShape.IntravenousFluid OrderType.Continuous
                    // Max dose rate per kg is 5 ml/kg/hour
                    c Mapping.OrderableDoseRate 
                      (Props.MaxIncl 5N) Constraint.NoLimit
                      RouteShape.IntravenousFluid OrderType.Continuous
                    // Set dose rate values
                    c Mapping.OrderableDoseRate 
                      (Props.Vals dr) Constraint.NoLimit
                      RouteShape.IntravenousFluid OrderType.Continuous
                ]



            let apply cs (o : Order) =
                let rs = RouteShape.map o.Route o.Orderable.Shape
                let ot = o |> OrderType.map


                let log =
                    let timer = System.Diagnostics.Stopwatch.StartNew()
                    
                    {
                        Logger.LogMessage = 
                            fun level msg ->
                                printfn "- %f" timer.Elapsed.TotalSeconds
                                //printfn "received msg: %f" timer.Elapsed.TotalSeconds
                    }


                let propHasVals = function
                | Props.Vals vs
                | Props.Increment vs -> vs |> Set.isEmpty |> not
                | _ -> true


                let filter cs =
                    cs
                    |> List.filter(fun c ->
                        (c.Property |> propHasVals) &&
                        (c.RouteShape = RouteShape.Any || c.RouteShape = rs) &&
                        (c.OrderType =  OrderType.Any  || c.OrderType = ot)
                    )

                let cs = 
                    cs 
                    |> filter
                    |> List.map (mapToConstraint o)

                o
                |> Order.solveUnits log
                |> Order.solveConstraints log cs
                |> fun o -> 
                    Order.calcScenarios log o


        module Item = Orderable.Item
        module IDto = Item.Dto
        module Component = Orderable.Component
        module CDto = Component.Dto
        module ODto = Orderable.Dto

        module Mapping = Order.Mapping
        module Props = Informedica.GenSolver.Lib.Props
        module Constraint = Informedica.GenSolver.Lib.Constraint
        module Name = WrappedString.Name

        type OrderType = OrderType.OrderType
        type Order = Order.Order
                
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

        type ConstrainedOrder = (DrugConstraint.DrugConstraint list * Order)

        let (>|>) (cs, o) c = (c |> List.append cs, o)

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
                OrderType = OrderType.Process
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
                [ 
                    ud.Abbreviation.Dut
                    ud.Abbreviation.Eng
                    ud.Name.Dut
                    ud.Name.Eng
                ]
                |> List.append ud.Synonyms
                |> List.exists((=) u)
            )
            |> function 
            | Some ud -> 
                ud.Group 
                |> ValueUnit.Group.toString 
            | None -> "General"
            |> sprintf "%s[%s]" u
            

        let create (d : DrugOrder) : ConstrainedOrder =
            let ou = d.Unit |> unitGroup
            let odto = ODto.dto d.Id d.Name d.Shape

            odto.OrderableQuantity.Unit <- ou
            odto.OrderQuantity.Unit <- ou
            
            match d.OrderType with
            | OrderType.Any
            | OrderType.Process -> ()

            | OrderType.Continuous ->                
                odto.DoseRate.Unit <- 
                    d.RateUnit
                    |> unitGroup
                    |> sprintf "%s/%s" ou
                odto.DoseRateAdjust.Unit <-
                    d.RateUnit
                    |> unitGroup 
                    |> sprintf "%s/kg[Weight]/%s" ou

            | OrderType.Discontinuous ->                
                odto.DoseTotal.Unit <-
                    d.TimeUnit
                    |> unitGroup
                    |> sprintf "%s/%s" ou

            | OrderType.Timed ->                
                odto.DoseTotal.Unit <-
                    d.TimeUnit
                    |> unitGroup
                    |> sprintf "%s/%s" ou
                odto.DoseRate.Unit <- 
                    d.RateUnit
                    |> unitGroup
                    |> sprintf "%s/%s" ou
                odto.DoseRateAdjust.Unit <-
                    d.RateUnit
                    |> unitGroup 
                    |> sprintf "%s/kg[Weight]/%s" ou

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
                                | OrderType.Any -> ()
                                | OrderType.Process -> ()
                                | OrderType.Continuous ->
                                    idto.DoseRateAdjust.Unit <- 
                                        sprintf "%s/kg[Weight]/%s" du tu
                                | OrderType.Discontinuous ->
                                    idto.DoseQuantity.Unit <- du
                                    idto.DoseTotalAdjust.Unit <- 
                                        p.TimeUnit
                                        |> unitGroup
                                        |> sprintf "%s/kg[Weight]/%s" du 
                                | OrderType.Timed ->
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
                | OrderType.Any -> 
                    "the order type cannot by 'Any'" 
                    |> failwith
                | OrderType.Process ->
                    Order.Dto.``process`` d.Id d.Name d.Shape d.Route
                | OrderType.Continuous ->
                    Order.Dto.continuous d.Id d.Name d.Shape d.Route
                | OrderType.Discontinuous ->
                    Order.Dto.discontinuous d.Id d.Name d.Shape d.Route
                | OrderType.Timed ->
                    Order.Dto.timed d.Id d.Name d.Shape d.Route

            dto.Orderable <- odto

            dto.Prescription.Frequency.Unit <- 
                sprintf "x[Count]/%s" (d.TimeUnit |> unitGroup)
            dto.Adjust.Unit <- "kg[Weight]"

            let cstr m p vs rs ot = 
                DrugConstraint.create d.Name m p vs rs ot

            dto
            |> Order.Dto.fromDto
            |> fun o ->
                // first add all general orderable constraints
                let co = (DrugConstraint.constraints (o.Orderable.Name |> Name.toString), o)
                // adding orderable constraints
                co 
                >|> [ 
                        // ALL set possible orderable quantities
                        cstr Mapping.OrderableOrderableQty 
                            (d.Quantities |> Set.ofList |> Props.Vals) 
                            Constraint.NoLimit
                            RouteShape.Any OrderType.Any

                        // RECTAL SOLID give max 1 piece from rectal solid 
                        cstr Mapping.OrderableDoseQty 
                            (1N |> Set.singleton |> Props.Vals) 
                            Constraint.NoLimit
                            RouteShape.RectalSolid OrderType.Discontinuous

                        // ORAL SOLID give max 10 pieces from oral solid
                        cstr Mapping.OrderableDoseQty 
                            ([ 1N / d.Divisible.. 1N / d.Divisible ..10N ]
                             |> Set.ofList |> Props.Vals)
                            Constraint.NoLimit
                            RouteShape.OralSolid OrderType.Discontinuous

                        // ORAL FLUID increment
                        cstr Mapping.OrderableDoseQty 
                            ((1N/d.Divisible) |> Set.singleton 
                                              |> Props.Increment)
                            Constraint.NoLimit
                            RouteShape.OralFluid OrderType.Discontinuous
                        cstr Mapping.OrderableDoseQty 
                            ((1N/d.Divisible) |> Set.singleton 
                                              |> Props.Increment)
                            Constraint.NoLimit
                            RouteShape.OralFluid OrderType.Timed
                        cstr Mapping.OrderableDoseRate 
                            ((1N/d.Divisible) |> Set.singleton 
                                              |> Props.Increment)
                            Constraint.NoLimit
                            RouteShape.OralFluid OrderType.Timed
                        cstr Mapping.OrderableDoseRate 
                            ((1N/d.Divisible) |> Set.singleton 
                                              |> Props.Increment)
                            Constraint.NoLimit
                            RouteShape.OralFluid OrderType.Continuous

                        // INTRAVENUOUS FLUID increment
                        cstr Mapping.OrderableDoseQty 
                            ((1N/d.Divisible) |> Set.singleton 
                                              |> Props.Increment)
                            Constraint.NoLimit
                            RouteShape.IntravenousFluid OrderType.Discontinuous
                        cstr Mapping.OrderableDoseQty 
                            ((1N/d.Divisible) |> Set.singleton 
                                              |> Props.Increment)
                            Constraint.NoLimit
                            RouteShape.IntravenousFluid OrderType.Timed
                        cstr Mapping.OrderableDoseRate 
                            ((1N/d.Divisible) |> Set.singleton 
                                              |> Props.Increment)
                            Constraint.NoLimit
                            RouteShape.IntravenousFluid OrderType.Timed
                        cstr Mapping.OrderableDoseRate 
                            ((1N/d.Divisible) |> Set.singleton 
                                              |> Props.Increment)
                            Constraint.NoLimit
                            RouteShape.IntravenousFluid OrderType.Continuous

                    ]
            |> fun co ->
                d.Products
                |> Seq.fold (fun co p ->
                    let n = p.Name
                    // adding component constraints
                    let co =
                        co
                        >|> [ 
                                // ALL set possible component quantities
                                DrugConstraint.create n 
                                    Mapping.ComponentComponentQty 
                                    (p.Quantities |> Set.ofList |> Props.Vals) 
                                    Constraint.NoLimit
                                    RouteShape.Any OrderType.Any
                                // give max 10 solid oral each time
                                //DrugConstraint.create n 
                                //    Mapping.ComponentOrderableQty 
                                //    ([ 1N / d.Divisible.. 1N / d.Divisible ..10N ]
                                //     |> Set.ofList |> Props.Vals)
                                //    Constraint.NoLimit
                                //    RouteShape.OralSolid OrderType.Discontinuous
                                // give max 

                                // ORAL FLUID
                                DrugConstraint.create n 
                                    Mapping.ComponentOrderableQty 
                                    ([ 1N / d.Divisible.. 1N / d.Divisible ..250N ]
                                     |> Set.ofList |> Props.Vals)
                                    Constraint.NoLimit
                                    RouteShape.OralFluid OrderType.Any
                                DrugConstraint.create n 
                                    Mapping.ComponentOrderableQty 
                                    ([ 1N / d.Divisible]
                                     |> Set.ofList |> Props.Increment)
                                    Constraint.NoLimit
                                    RouteShape.OralFluid OrderType.Any
                                DrugConstraint.create n 
                                    Mapping.ComponentDoseQty 
                                    ([ 1N / d.Divisible ] |> Set.ofList |> Props.Increment)
                                    Constraint.NoLimit
                                    RouteShape.OralFluid OrderType.Discontinuous
                                DrugConstraint.create n 
                                    Mapping.ComponentDoseQty 
                                    ([ 1N / d.Divisible ] |> Set.ofList |> Props.Increment)
                                    Constraint.NoLimit
                                    RouteShape.OralFluid OrderType.Timed
                                //DrugConstraint.create n 
                                //    Mapping.ComponentDoseRate 
                                //    ([ 1N / d.Divisible ] |> Set.ofList |> Props.Increment)
                                //    Constraint.NoLimit
                                //    RouteShape.OralFluid OrderType.Timed
                                //DrugConstraint.create n 
                                //    Mapping.ComponentDoseRate 
                                //    ([ 1N / d.Divisible ] |> Set.ofList |> Props.Increment)
                                //    Constraint.NoLimit
                                //    RouteShape.OralFluid OrderType.Continuous

                                // INRAVENOUS FLUID
                                DrugConstraint.create n 
                                    Mapping.ComponentOrderableQty 
                                    ([ 1N / d.Divisible.. 1N / d.Divisible ..500N ]
                                     |> Set.ofList |> Props.Vals)
                                    Constraint.NoLimit
                                    RouteShape.IntravenousFluid OrderType.Any
                                DrugConstraint.create n 
                                    Mapping.ComponentOrderableQty 
                                    ([ 1N / d.Divisible ]
                                     |> Set.ofList |> Props.Increment)
                                    Constraint.NoLimit
                                    RouteShape.IntravenousFluid OrderType.Any
                                DrugConstraint.create n 
                                    Mapping.ComponentDoseQty 
                                    ([ 1N / d.Divisible ] |> Set.ofList |> Props.Increment)
                                    Constraint.NoLimit
                                    RouteShape.IntravenousFluid OrderType.Discontinuous
                                DrugConstraint.create n 
                                    Mapping.ComponentDoseQty 
                                    ([ 1N / d.Divisible ] |> Set.ofList |> Props.Increment)
                                    Constraint.NoLimit
                                    RouteShape.IntravenousFluid OrderType.Timed
                                //DrugConstraint.create n 
                                //    Mapping.ComponentDoseRate 
                                //    ([ 1N / d.Divisible ] |> Set.ofList |> Props.Increment)
                                //    Constraint.NoLimit
                                //    RouteShape.IntravenousFluid OrderType.Timed
                                //DrugConstraint.create n 
                                //    Mapping.ComponentDoseRate 
                                //    ([ 1N / d.Divisible ] |> Set.ofList |> Props.Increment)
                                //    Constraint.NoLimit
                                //    RouteShape.IntravenousFluid OrderType.Continuous

                                // RECTAL SOLID
                                DrugConstraint.create n 
                                    Mapping.ComponentOrderableQty 
                                    (1N |> Set.singleton |> Props.Vals)  
                                    Constraint.NoLimit
                                    RouteShape.RectalSolid OrderType.Discontinuous

                                // SINGLE COMPONENT
                                if d.Products |> List.length = 1 then
                                    DrugConstraint.create n
                                        Mapping.ComponentOrderableConc
                                        (1N |> Set.singleton |> Props.Vals)  
                                        Constraint.NoLimit
                                        RouteShape.Any OrderType.Any
                            ]

                    p.Substances 
                    |> Seq.fold (fun co s ->
                        let n = s.Name
                        // adding item constraints
                        co
                        >|> [ 
                                // ALL set concentrations and quanties
                                DrugConstraint.create n 
                                    Mapping.ItemComponentConc 
                                    (s.Concentrations |> Set.ofList |> Props.Vals) 
                                    Constraint.NoLimit
                                    RouteShape.Any OrderType.Any
                                DrugConstraint.create n 
                                    Mapping.ItemOrderableQty 
                                    (s.OrderableQuantities |> Set.ofList |> Props.Vals) 
                                    Constraint.NoLimit
                                    RouteShape.Any OrderType.Any
                                if d.Products |> List.length = 1 then
                                    DrugConstraint.create n
                                        Mapping.ItemOrderableConc
                                        (s.Concentrations |> Set.ofList |> Props.Vals) 
                                        Constraint.NoLimit
                                        RouteShape.Any OrderType.Any
                                    
                            ]
                    ) co
                ) co
                

        type DoseLimits =
            {
                Name : string
                Frequencies : BigRational list
                Rates : BigRational list
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
                Rates = []
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
                DoseCount : BigRational option
                MinTime : BigRational option
                MaxTime : BigRational option
            }

        let solutionLimits =
            {
                Name = ""
                Component = ""
                MinConcentration = None
                MaxConcentration = None
                DoseCount = Some 1N
                MinTime = None
                MaxTime = None
            }

        let setDoseLimits (dl : DoseLimits) (co : ConstrainedOrder) : ConstrainedOrder =
            let sn = dl.SubstanceName

            let cr m c v co =
                match v with
                | Some v -> 
                    co
                    >|> [ DrugConstraint.create sn m (c v) Constraint.NoLimit RouteShape.Any OrderType.Any ]
                | None -> co
                    
            co
            |> function
            | (cs, o) ->
                if dl.Rates |> List.isEmpty then (cs, o)
                else
                    let drc =
                        DrugConstraint.create dl.Name 
                            Mapping.OrderableDoseRate  
                            (dl.Rates |> Set.ofList |> Props.Vals)
                            Constraint.NoLimit RouteShape.Any OrderType.Continuous 
                        
                    cs 
                    |> List.replace (fun c -> c.Mapping = Mapping.OrderableDoseRate &&
                                              c.OrderType = OrderType.Continuous) drc
                    , o
            >|> [ 
                    DrugConstraint.create dl.Name
                        Mapping.Freq 
                        (dl.Frequencies |> Set.ofList |> Props.Vals) 
                        Constraint.NoLimit RouteShape.Any OrderType.Discontinuous 
                    DrugConstraint.create dl.Name 
                        Mapping.Freq  
                        (dl.Frequencies |> Set.ofList |> Props.Vals)
                        Constraint.NoLimit RouteShape.Any OrderType.Timed 
                ]
            |> cr Mapping.ItemDoseQty Props.MaxIncl dl.MaxDoseQuantity
            |> cr Mapping.ItemDoseQty Props.MinIncl dl.MinDoseQuantity
            |> cr Mapping.ItemDoseAdjustQtyAdjust Props.MaxIncl dl.MaxDoseQuantityAdjust
            |> cr Mapping.ItemDoseAdjustQtyAdjust Props.MinIncl dl.MinDoseQuantityAdjust
            |> cr Mapping.ItemDoseTotal Props.MaxIncl dl.MaxDoseTotal
            |> cr Mapping.ItemDoseTotal Props.MinIncl dl.MinDoseTotal
            |> cr Mapping.ItemDoseAdjustTotalAdjust Props.MaxIncl dl.MaxDoseTotalAdjust
            |> cr Mapping.ItemDoseAdjustTotalAdjust Props.MinIncl dl.MinDoseTotalAdjust
            |> cr Mapping.ItemDoseRate Props.MaxIncl dl.MaxDoseRate
            |> cr Mapping.ItemDoseRate Props.MinIncl dl.MinDoseRate
            |> cr Mapping.ItemDoseAdjustRateAdjust Props.MaxIncl dl.MaxDoseRateAdjust
            |> cr Mapping.ItemDoseAdjustRateAdjust Props.MinIncl dl.MinDoseRateAdjust

        let setSolutionLimits (sl : SolutionLimits) 
                              (co : ConstrainedOrder) : ConstrainedOrder =
            let (_, o) = co
            let set n m c v co =
                match v with
                | Some v -> 
                    co
                    >|> [ DrugConstraint.create n m (c v) Constraint.NoLimit RouteShape.Any OrderType.Any ]
                | None -> co

            co
            >|> [
                    if sl.DoseCount |> Option.isSome then
                        DrugConstraint.create 
                            sl.Name 
                            Mapping.OrderableDoseCount 
                            (sl.DoseCount |> Option.get |> Set.singleton |> Props.Vals) 
                            Constraint.NoLimit RouteShape.Any OrderType.Any
                ]
            |> set sl.Name Mapping.ItemOrderableConc Props.MinIncl sl.MinConcentration
            |> set sl.Name Mapping.ItemOrderableConc Props.MaxIncl sl.MaxConcentration
            |> set sl.Name Mapping.Time Props.MinIncl sl.MinTime
            |> set sl.Name Mapping.Time Props.MaxIncl sl.MaxTime



        let setAdjust n a (co : ConstrainedOrder) : ConstrainedOrder =
            co
            >|> [ 
                    DrugConstraint.create 
                        n 
                        Mapping.AdjustQty 
                        (a |> Set.singleton |> Props.Vals) 
                        Constraint.NoLimit
                        RouteShape.Any OrderType.Any 
                ]


        let evaluate (co : ConstrainedOrder) =
            let (cs, o) = co

            DrugConstraint.apply cs o


module DrugConstraint = DrugOrder.DrugConstraint


let printScenarios v n sc =
    printfn "\n\n=== SCENARIOS ==="
    sc
    |> List.iteri (fun i o ->
        o
        |> Order.printPrescription n
        |> fun (p, d) ->
            printfn "%i\t%s" (i + 1) p
            printfn "  \t%s" d
        

        if v then
            o
            |> Order.toString
            |> List.iteri (fun i s -> printfn "%i\t%s" (i + 1) s)
    )



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
        OrderType = DrugOrder.OrderType.Discontinuous
}
|> DrugOrder.create
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
|> DrugOrder.evaluate
|> printScenarios false ["paracetamol"]
//|> List.iter (fun o ->
//    o
//    |> Order.toString
//    |> List.iteri (fun i s -> printfn "%i\t%s" (i + 1) s)
//)



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
        OrderType = DrugOrder.OrderType.Discontinuous
}
|> DrugOrder.create
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
|> DrugOrder.evaluate
//|> Order.calcScenarios2
//|> List.length
|> printScenarios false ["sulfamethoxazol"; "trimethoprim"]



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
        OrderType = DrugOrder.OrderType.Discontinuous
}
|> DrugOrder.create
|> DrugOrder.setAdjust "paracetamol" 20N
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "paracetamol"
            Frequencies = [ 2N .. 4N ]
            SubstanceName = "paracetamol"
            MaxDoseQuantity = Some 1000N
            MaxDoseTotal = Some 4000N
            MinDoseTotalAdjust = Some 60N
            MaxDoseTotalAdjust = Some 90N
    }
|> DrugOrder.evaluate
//|> Order.calcScenarios2
//|> List.length
|> printScenarios false ["paracetamol"]



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
        OrderType = DrugOrder.OrderType.Discontinuous
}
|> DrugOrder.create
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
|> DrugOrder.evaluate
//|> Order.calcScenarios2
//|> List.length
|> printScenarios false ["sulfamethoxazol"; "trimethoprim"]



// Dopamin infusion calculate scenario's 
// with a number of standard solutions
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "dopamin infusion"
        Quantities = [ 50N ]
        Divisible = 2N
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
        OrderType = DrugOrder.OrderType.Continuous
}
|> DrugOrder.create
|> DrugOrder.setAdjust "dopamin infusion" 10N
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "dopamin infusion"
            SubstanceName = "dopamin"
            MinDoseRateAdjust = Some 2N
            MaxDoseRateAdjust = Some 20N
    }
//|> Order.printPrescriptions "dopamin"
//|> List.iteri (fun i s -> printfn "%i\t%s" (i + 1) s)
|> DrugOrder.evaluate
//|> Order.calcScenarios2
|> printScenarios false ["dopamin"]




// Dopamin infusion calculate scenario's 
// with a a fixed infusion - dose rate
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "dopamin infusion"
        Quantities = [ 50N ]
        Divisible = 10N
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
        OrderType = DrugOrder.OrderType.Continuous
}
|> DrugOrder.create
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "dopamin infusion"
            Rates = [ 1N ]
            SubstanceName = "dopamin"
            MinDoseRateAdjust = Some 2N
            MaxDoseRateAdjust = Some 20N
    }
|> DrugOrder.setAdjust "dopamin infusion" 10N
//|> Order.printPrescriptions "dopamin"
//|> List.iteri (fun i s -> printfn "%i\t%s" (i + 1) s)
|> DrugOrder.evaluate
//|> Order.calcScenarios2
|> printScenarios false ["dopamin"]



// gentamicin
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "gentamicin"
        Quantities = [ ]
        Divisible = 10N 
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
        OrderType = DrugOrder.OrderType.Timed
    }
|> DrugOrder.create
|> DrugOrder.setAdjust "gentamicin" (800N/1000N)
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "gentamicin"
            SubstanceName = "gentamicin"
            Frequencies = [ 1N ]
            MinDoseTotalAdjust = Some (5N * (9N/10N))
            MaxDoseTotalAdjust = Some (5N * (10N/9N))
    }
|> DrugOrder.setSolutionLimits 
    {
        DrugOrder.solutionLimits with
            Name = "gentamicin"
            Component = "gentamicin"
//            MinConcentration = Some (1N)
            MaxConcentration = Some (2N)
            DoseCount = Some (2N)
            MinTime = (Some (1N/2N))
            MaxTime = (Some 1N)

    }
|> DrugOrder.evaluate
//|> Order.calcScenarios2
|> printScenarios false ["gentamicin"]


