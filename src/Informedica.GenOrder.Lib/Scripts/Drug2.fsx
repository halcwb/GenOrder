﻿
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Utils.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Variable.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Equation.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Solver.fs"
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
    module Time = VariableUnit.Time
    module Rate = VariableUnit.Rate
    module Quantity = VariableUnit.Quantity
    module TotalAdjust = VariableUnit.TotalAdjust
    module Dose = VariableUnit.Dose
    module DoseAdjust = VariableUnit.DoseAdjust
    module RateAdjust = VariableUnit.RateAdjust
    module Name = WrappedString.Name
    module Props = Solver.Props
    module Solver = Informedica.GenOrder.Lib.Solver


    let validScenario (o: Order) =
        o
        |> Order.toEqs
        |> function
        | (vrus1, vrus2) ->
            vrus1
            |> List.append vrus2
            |> List.collect id
            |> List.exists (fun vru ->
                vru 
                |> VariableUnit.getBaseValues
                |> List.length > 1
            )
            |> not


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
    
                |> Solver.solve log None n Props.Vals [v]
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
                    |> List.length > 1
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

            
        let rec calc os sc =

            match sc with
            | None         -> 
                os
            | Some (n, vs) ->
                (vs |> Seq.map BigRational.toString |> String.concat ",")
                |> printfn "scenario: %A, with %A" n 
                [
                    for v in vs do
                        for o in os do
                            if o |> Order.contains n v then
                                o
                                |> Order.toString
                                |> String.concat "\n"
                                |> printfn "setting: %Ato\n%s" v
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

        | Prescription.Timed (fr, tme) ->

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

            let oq =
                o.Orderable.OrderableQuantity
                |> Quantity.toValueUnitStringList None
                |> Seq.map snd
                |> String.concat ""


            let dt =
                o
                |> printItmDose 
                    (fun i -> i.DoseAdjust |> DoseAdjust.get |> (fun (_, dt, _) -> dt))
                    (VariableUnit.TotalAdjust.toValueUnitStringList None)

            //if oq = "1.8 ml" then 
            //    printfn "Oops:\n%A" o

            sprintf "%s %s %s = (%s) in %s" 
                (o.Orderable.Name |> Name.toString) fr dq dt oq


        | Prescription.Process ->
            o.Orderable.Name
            |> Name.toString
            |> sprintf "%s"  


// Creating a drug order
module DrugOrder =

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


        module Constraint =

            module Name = WrappedString.Name
            module Mapping = Order.Mapping
            module Props = Solver.Props

            type Name = Name.Name
            type Mapping = Order.Mapping.Map
            type Prop = Props.Prop
            type RouteShape = RouteShape.RouteShape
            type OrderType = OrderType.OrderType
            type Order = Order.Order

            type Constraint =
                {
                    Name : Name
                    Mapping : Mapping
                    Prop : Prop
                    Values : BigRational list
                    Limit : int option
                    RouteShape : RouteShape
                    OrderType : OrderType
                }

            let create n m p vs l rs ps = 
                {
                    Name = n
                    Mapping = m
                    Prop = p
                    Values = vs
                    Limit = l
                    RouteShape = rs
                    OrderType = ps
                }


            let toString (c : Constraint) =
                sprintf "%A %A %A %A" c.Name c.Mapping c.Prop c.Values

            let constraints n =
                let dr =
                    [(1N/10N)..(1N/10N)..10N] 
                    |> List.append [11N..1N..100N]
                    |> List.append [105N..5N..1000N]
                    |> List.sort
                let c m p vs rs ps =
                    create n m p vs rs ps
                // list of general orderable constraints
                [
                    // == Oral Solid ==
                    // == Discontinuous ==
                    // give max 10 pieces each time
                    c Mapping.OrderableDoseQty 
                      Props.MaxIncl [10N] None
                      RouteShape.OralSolid OrderType.Discontinuous

                    // == Rectal Solid ==
                    // == Discontinuous ==
                    // Give 1 piece each time
                    c Mapping.OrderableDoseQty 
                      Props.MaxIncl [1N]  None
                      RouteShape.RectalSolid OrderType.Discontinuous

                    // == Oral Fluid ==
                    // == Discontinuous ==
                    // give the total orderable quantity each time
                    c Mapping.OrderableDoseCount 
                      Props.Vals [1N] None
                      RouteShape.OralFluid OrderType.Discontinuous
                    // give max 500 ml each time
                    c Mapping.OrderableDoseQty 
                      Props.MaxIncl [500N] None
                      RouteShape.OralFluid OrderType.Discontinuous
                    // give max 10 ml/kg each time
                    c Mapping.OrderableDoseAdjustQtyAdjust 
                      Props.MaxIncl [10N] None
                      RouteShape.OralFluid OrderType.Discontinuous
                    // give max 500 ml each time

                    // == Oral Fluid ==
                    // == Timed ==
                    // Give max 500 ml each time
                    c Mapping.OrderableDoseQty 
                      Props.MaxIncl [500N] None
                      RouteShape.OralFluid OrderType.Timed
                    // give max 10 ml/kg each time
                    c Mapping.OrderableDoseAdjustQtyAdjust 
                      Props.MaxIncl [10N] None 
                      RouteShape.OralFluid OrderType.Timed

                    // == Oral Fluid ==
                    // == Continuous ==
                    // Max dose rate is 200 ml/hour
                    c Mapping.OrderableDoseRate 
                      Props.MaxIncl [200N] None
                      RouteShape.OralFluid OrderType.Continuous
                    // Max dose rate per kg is 5 ml/kg/hour
                    c Mapping.OrderableDoseRate 
                      Props.MaxIncl [5N] None
                      RouteShape.OralFluid OrderType.Continuous
                    // Set dose rate values
                    c Mapping.OrderableDoseRate 
                      Props.Vals dr None
                      RouteShape.OralFluid OrderType.Continuous

                    // == Intravenuous Fluid ==
                    // == Discontinuous ==
                    // Give max 1000 ml each time
                    c Mapping.OrderableDoseQty 
                      Props.MaxIncl [1000N] None
                      RouteShape.IntravenousFluid OrderType.Discontinuous
                    // Give max 20 ml/kg each time
                    c Mapping.OrderableDoseAdjustQtyAdjust 
                      Props.MaxIncl [20N] None
                      RouteShape.IntravenousFluid OrderType.Discontinuous

                    // == Intravenuous Fluid ==
                    // == Timed ==
                    // Give max 1000 ml each time
                    c Mapping.OrderableDoseQty 
                      Props.MaxIncl [1000N] None
                      RouteShape.IntravenousFluid OrderType.Timed
                    // Give max 20 ml/kg each time
                    c Mapping.OrderableDoseAdjustQtyAdjust 
                      Props.MaxIncl [20N] None
                      RouteShape.IntravenousFluid OrderType.Timed
                    // Select 1 possible value from dose rates
                    c Mapping.OrderableDoseRate 
                      Props.Vals dr (Some 1)
                      RouteShape.IntravenousFluid OrderType.Timed

                    // == Intravenuous Fluid ==
                    // == Continuous ==
                    // Max dose rate is 200 ml/hour
                    c Mapping.OrderableDoseRate 
                      Props.MaxIncl [200N] None
                      RouteShape.IntravenousFluid OrderType.Continuous
                    // Max dose rate per kg is 5 ml/kg/hour
                    c Mapping.OrderableDoseRate 
                      Props.MaxIncl [5N] None
                      RouteShape.IntravenousFluid OrderType.Continuous
                    // Set dose rate values
                    c Mapping.OrderableDoseRate 
                      Props.Vals dr None
                      RouteShape.IntravenousFluid OrderType.Continuous
                ]

            /// get the order of applying constraints
            /// 1. Min or Max
            /// 2. Vals
            /// 3. Incr
            let orderMap (c : Constraint) =
                match c.Prop with
                | _ when c.Limit |> Option.isSome 
                    -> System.Int32.MaxValue
                | Props.Vals  -> c.Values |> List.length
                | _ -> -1


            let apply cs (o : Order) =
                let rs = RouteShape.map o.Route o.Orderable.Shape
                let ot = o |> OrderType.map


                let memPrint (timer: System.Diagnostics.Stopwatch) f =
                    let cache = ref Map.empty

                    fun s ->
                        match (!cache).TryFind(s) with
                        | Some _ -> ()
                        | None ->
                            let s =
                                s
                                |> sprintf "%i: %s" timer.Elapsed.Seconds
                            let r = f s
                            cache := (!cache).Add(s, r)
                            ()

                let log =
                    let timer = System.Diagnostics.Stopwatch.StartNew()
                    memPrint timer (printfn "%s")


                let filter cs =
                    cs
                    |> List.filter(fun c ->
                        c.Values |> List.length > 0 &&
                        (c.RouteShape = RouteShape.Any || c.RouteShape = rs) &&
                        (c.OrderType =  OrderType.Any  || c.OrderType = ot)
                    )

                cs
                |> filter
                |> List.sortBy orderMap
                // apply the constraints in the most effective order
                |> fun cs ->
                    printfn "going to apply the following constraints in order:"
                    cs
                    |> List.iteri (fun i c ->
                        c
                        |> toString
                        |> printfn "%i.\t%s" i 
                    )
                    cs
                // calculate for each scenario the constraints
                |> List.fold (fun acc c ->
                    let i, o = acc

                    c
                    |> toString
                    |> printfn "calc no %i\t%s" i

                    let n = c.Name |> Name.toString

                    i + 1, o |> Order.solve log c.Limit n c.Mapping c.Prop c.Values 
                ) (0, o |> Order.solveUnits)
                |> snd
                |> fun o -> 
                    printfn "---- running scenarios for"
                    Order.calcScenarios log o


        module Item = Orderable.Item
        module IDto = Item.Dto
        module Component = Orderable.Component
        module CDto = Component.Dto
        module ODto = Orderable.Dto

        module Mapping = Order.Mapping
        module Props = Solver.Props
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

        type ConstrainedOrder = (Constraint.Constraint list * Order)

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
                Constraint.create ([d.Name] |> Name.create) m p vs rs ot

            dto
            |> Order.Dto.fromDto
            |> fun o ->
                // first add all general orderable constraints
                let co = (Constraint.constraints o.Orderable.Name, o)
                // adding orderable constraints
                co 
                >|> [ 
                        // Set possible orderable quantities
                        cstr Mapping.OrderableOrderableQty 
                             Props.Vals d.Quantities None
                             RouteShape.Any OrderType.Any
                        // give max 1 piece from rectal solid 
                        cstr Mapping.OrderableDoseQty 
                             Props.Vals [ 1N ] None
                             RouteShape.RectalSolid OrderType.Discontinuous
                        // give max 10 pieces from oral solid
                        cstr Mapping.OrderableDoseQty 
                             Props.Vals [ 1N / d.Divisible.. 1N / d.Divisible ..10N ]
                             None
                             RouteShape.OralSolid OrderType.Discontinuous
                        // give max 500 ml from fluid
                        cstr Mapping.OrderableDoseQty 
                             Props.Vals [ 1N / d.Divisible .. 1N /d.Divisible ..500N ]
                             None
                             RouteShape.OralFluid OrderType.Any
                        cstr Mapping.OrderableDoseQty 
                             Props.Vals [ 1N / d.Divisible .. 1N /d.Divisible ..500N ]
                             None
                             RouteShape.IntravenousFluid OrderType.Any
                        // give max 500 ml from fluid
                        cstr Mapping.ComponentOrderableQty 
                            Props.Vals [ 1N / d.Divisible .. 1N /d.Divisible ..500N ]
                            None
                            RouteShape.OralFluid OrderType.Any
                        cstr Mapping.ComponentOrderableQty 
                            Props.Vals [ 1N / d.Divisible .. 1N /d.Divisible ..500N ]
                            None
                            RouteShape.IntravenousFluid OrderType.Any

                    ]
            |> fun co ->
                d.Products
                |> Seq.fold (fun co p ->
                    let n = [ p.Name ] |> Name.create
                    // adding component constraints
                    let co =
                        co
                        >|> [ 
                                Constraint.create n 
                                    Mapping.ComponentComponentQty 
                                    Props.Vals p.Quantities None
                                    RouteShape.Any OrderType.Any
                                // give max 10 solid oral each time
                                Constraint.create n 
                                    Mapping.ComponentOrderableQty 
                                    Props.Vals [ 1N / d.Divisible.. 1N / d.Divisible ..10N ] 
                                    None
                                    RouteShape.OralSolid OrderType.Discontinuous
                                // give max 
                                Constraint.create n 
                                    Mapping.ComponentOrderableQty 
                                    Props.Vals [ 1N / d.Divisible .. 1N /d.Divisible ..250N ]
                                    None
                                    RouteShape.OralFluid OrderType.Any
                                Constraint.create n 
                                    Mapping.ComponentOrderableQty 
                                    Props.Vals [ 1N ] 
                                    None
                                    RouteShape.RectalSolid OrderType.Discontinuous
                                if d.Products |> List.length = 1 then
                                    Constraint.create n
                                        Mapping.ComponentOrderableConc
                                        Props.Vals [1N]
                                        None
                                        RouteShape.Any OrderType.Any
                            ]

                    p.Substances 
                    |> Seq.fold (fun co s ->
                        let n = [ s.Name ] |> Name.create
                        // adding item constraints
                        co
                        >|> [ 
                                Constraint.create n 
                                    Mapping.ItemComponentConc 
                                    Props.Vals s.Concentrations 
                                    None
                                    RouteShape.Any OrderType.Any
                                Constraint.create n 
                                    Mapping.ItemOrderableQty 
                                    Props.Vals s.OrderableQuantities 
                                    None
                                    RouteShape.Any OrderType.Any
                                Constraint.create n 
                                    Mapping.ItemOrderableQty 
                                    Props.Vals s.Concentrations  
                                    None
                                    RouteShape.OralSolid OrderType.Any 
                                Constraint.create n 
                                    Mapping.ItemOrderableQty 
                                    Props.Vals s.Concentrations  
                                    None
                                    RouteShape.RectalSolid OrderType.Any 
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
            let sn = [ dl.SubstanceName ] |> Name.create

            let cr m p l co =
                match l with
                | Some l -> 
                    co
                    >|> [ Constraint.create sn m p [ l ] None RouteShape.Any OrderType.Any ]
                | None -> co
                    
            co
            >|> [ 
                    Constraint.create ([dl.Name] |> Name.create) 
                        Mapping.Freq Props.Vals 
                        dl.Frequencies None RouteShape.Any OrderType.Discontinuous 
                    Constraint.create ([dl.Name] |> Name.create) 
                        Mapping.Freq Props.Vals 
                        dl.Frequencies None RouteShape.Any OrderType.Timed 
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
            let set n m p l co =
                let n = [ n ] |> Name.create
                match l with
                | Some l -> 
                    co
                    >|> [ Constraint.create n m p [ l ] None RouteShape.Any OrderType.Any ]
                | None -> co

            co
            |> set sl.Name Mapping.OrderableDoseCount Props.Vals sl.DoseCount
            |> set sl.Name Mapping.ItemOrderableConc Props.MinIncl sl.MinConcentration
            |> set sl.Name Mapping.ItemOrderableConc Props.MaxIncl sl.MaxConcentration
            |> set sl.Name Mapping.Time Props.MinIncl sl.MinTime
            |> set sl.Name Mapping.Time Props.MaxIncl sl.MaxTime



        let setAdjust n a (co : ConstrainedOrder) : ConstrainedOrder =
            let (_, o) = co

            co
            >|> [ Constraint.create ([n] |> Name.create) 
                                    Mapping.AdjustQty Props.Vals [a] None
                                    RouteShape.Any OrderType.Any ]

        let evaluate (co : ConstrainedOrder) =
            let (cs, o) = co

            Constraint.apply cs o


module Constraint = DrugOrder.Constraint


let printScenarios v n sc =
    sc
    |> List.iteri (fun i o ->
        o
        |> Order.printPrescription [n]
        |> printfn "%i\t%s" (i + 1)

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
|> printScenarios false "paracetamol"
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
|> printScenarios false "paracetamol"



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
|> printScenarios false "dopamin"




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
|> printScenarios false "dopamin"



// gentamicin
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "gentamicin"
        Quantities = [ ]
        Divisible = 1N 
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
|> DrugOrder.setAdjust "gentamicin" (800N / 1000N)
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
            DoseCount = Some (1N)
            MinTime = (Some (10N/60N))
            MaxTime = (Some 1N)

    }
|> DrugOrder.evaluate
//|> Order.calcScenarios2
|> printScenarios false "gentamicin"
