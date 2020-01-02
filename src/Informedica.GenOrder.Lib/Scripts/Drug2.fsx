
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


    let inValidScenario (o: Order) =
        o
        |> Order.toEqs
        |> function
        | (vrus1, vrus2) ->
            vrus1
            |> List.append vrus2
            |> List.collect id
            |> List.exists (fun vru ->
                let b =
                    vru 
                    |> VariableUnit.getVar
                    |> Variable.getValueRange
                    |> Variable.ValueRange.isEmpty 

                if b then 
                    o
                    |> Order.toString
                    |> String.concat ","
                    |> printfn "invalid scenario:\n%s"

                b
            )


    let calcScenarios solveE (o : Order) =

        let solve n v o =
            o
            |> Order.toEqs
            |> function 
            | (prod, sum) ->
                prod 
                |> List.map Solver.productEq
                |> List.append (sum |> List.map Solver.sumEq)
    
            |> Solver.solve solveE n Props.Vals [v]
            |> Order.fromEqs o

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
            | None         -> os
            | Some (n, vs) ->
                (vs |> Seq.map BigRational.toString |> String.concat ",")
                |> printfn "scenario: %A, with %A" n 
                [
                    for v in vs do
                        for o in os do
                            o
                            |> solve n v
                ]     
                |> List.filter (inValidScenario >> not)
                |> List.map (fun o ->
                    o
                    |> smallest
                    |> calc [o]
                )
                |> List.collect id

        o
        |> smallest
        |> calc [o]


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
                    RouteShape : RouteShape
                    OrderType : OrderType
                }

            let create n m p vs rs ps = 
                {
                    Name = n
                    Mapping = m
                    Prop = p
                    Values = vs
                    RouteShape = rs
                    OrderType = ps
                }


            let toString (c : Constraint) =
                sprintf "%A %A %A %A" c.Name c.Mapping c.Prop c.Values

            let constraints n =
                let c m p vs rs ps =
                    create n m p vs rs ps
                // list of general orderable constraints
                [
                    c Mapping.OrderableDoseQty 
                      Props.MaxIncl [10N] 
                      RouteShape.OralSolid OrderType.Discontinuous
                    c Mapping.OrderableDoseQty 
                      Props.MaxIncl [1N]  
                      RouteShape.RectalSolid OrderType.Discontinuous
                    c Mapping.OrderableDoseQty 
                      Props.MaxIncl [30N] 
                      RouteShape.IntravenousFluid OrderType.Discontinuous
                    c Mapping.OrderableDoseAdjustQtyAdjust 
                      Props.MaxIncl [5N] 
                      RouteShape.IntravenousFluid OrderType.Discontinuous
                    c Mapping.OrderableDoseQty 
                      Props.MaxIncl [30N] 
                      RouteShape.IntravenousFluid OrderType.Timed
                    c Mapping.OrderableDoseAdjustQtyAdjust 
                      Props.MaxIncl [5N] 
                      RouteShape.IntravenousFluid OrderType.Timed
                    c Mapping.OrderableDoseRate 
                      Props.MaxIncl [20N] 
                      RouteShape.IntravenousFluid OrderType.Continuous
                    c Mapping.OrderableDoseRate 
                      Props.Incr [1N/10N] 
                      RouteShape.IntravenousFluid OrderType.Continuous
                ]

            /// get the order of applying constraints
            /// 1. Min or Max
            /// 2. Vals
            /// 3. Incr
            let orderMap (c : Constraint) =
                match c.Prop with
                | Props.Incr -> -1 //System.Int32.MaxValue
                | Props.Vals -> c.Values |> List.length
                | Props.MinIncl 
                | Props.MinExcl -> System.Int32.MaxValue - 1                    
                | _             -> System.Int32.MaxValue //-1


            let apply cs (o : Order) =
                let rs = RouteShape.map o.Route o.Orderable.Shape
                let ot = o |> OrderType.map
                let mutable i = 0

                let solveE =
                    Informedica.GenSolver.Lib.Solver.memSolve 
                        Informedica.GenSolver.Lib.Solver.solveEquation

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
                    c
                    |> toString
                    |> printfn "calc no %i\t%s" i
                    i <- i + 1

                    let n = c.Name |> Name.toString
                    
                    [
                        for o in acc do

                            if c.Prop = Props.Vals then

                                for v in c.Values do

                                    if o |> Order.hasUnitValue n c.Mapping v then
                                        o
                                        |> Order.solve solveE n c.Mapping c.Prop [v]
                                
                            else 
                                o 
                                |> Order.solve solveE n c.Mapping c.Prop c.Values
                    ]

                ) [o]
                |> fun os -> os |> List.length |> printfn "start running scenarios for %i orders"; os
                |> List.map (fun o ->
                    async {
                        return Order.calcScenarios solveE o
                    }
                )
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.toList
                |> List.collect id


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
                        cstr Mapping.OrderableOrderableQty 
                             Props.Vals d.Quantities
                             RouteShape.Any OrderType.Any
                        cstr Mapping.OrderableDoseQty 
                             Props.Incr [ 1N / d.Divisible ] 
                             RouteShape.OralSolid OrderType.Discontinuous
                        cstr Mapping.OrderableDoseQty 
                             Props.Incr [ 1N / d.Divisible ] 
                             RouteShape.OralFluid OrderType.Any
                        cstr Mapping.OrderableDoseQty 
                             Props.Incr [ 1N / d.Divisible ] 
                             RouteShape.IntravenousFluid OrderType.Any
                        cstr Mapping.OrderableDoseQty 
                             Props.Incr [ 1N / d.Divisible ] 
                             RouteShape.RectalSolid OrderType.Discontinuous
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
                                    Props.Vals p.Quantities 
                                    RouteShape.Any OrderType.Any
                                Constraint.create n 
                                    Mapping.ComponentOrderableQty 
                                    Props.Incr [ 1N / d.Divisible ]
                                    RouteShape.Any OrderType.Any
                                Constraint.create n 
                                    Mapping.ComponentDoseQty 
                                    Props.Incr [ 1N / d.Divisible ]
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
                                    RouteShape.Any OrderType.Any
                                Constraint.create n 
                                    Mapping.ItemOrderableQty 
                                    Props.Vals s.OrderableQuantities 
                                    RouteShape.Any OrderType.Any
                                Constraint.create n 
                                    Mapping.ItemOrderableQty 
                                    Props.Vals s.Concentrations  
                                    RouteShape.OralSolid OrderType.Any 
                                Constraint.create n 
                                    Mapping.ItemOrderableQty 
                                    Props.Vals s.Concentrations  
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
                    >|> [ Constraint.create sn m p [ l ] RouteShape.Any OrderType.Any ]
                | None -> co
                    
            co
            >|> [ 
                    Constraint.create ([dl.Name] |> Name.create) 
                        Mapping.Freq Props.Vals 
                        dl.Frequencies RouteShape.Any OrderType.Discontinuous 
                    Constraint.create ([dl.Name] |> Name.create) 
                        Mapping.OrderableDoseRate Props.Vals 
                        dl.Rates RouteShape.Any OrderType.Continuous 
                    Constraint.create ([dl.Name] |> Name.create) 
                        Mapping.OrderableDoseRate Props.Vals 
                        dl.Rates RouteShape.Any OrderType.Timed 
                    Constraint.create ([dl.Name] |> Name.create) 
                        Mapping.Freq Props.Vals 
                        dl.Frequencies RouteShape.Any OrderType.Timed 
                    Constraint.create ([dl.Name] |> Name.create) 
                        Mapping.Freq Props.Vals 
                        dl.Frequencies RouteShape.Any OrderType.Timed 
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
                    >|> [ Constraint.create n m p [ l ] RouteShape.Any OrderType.Any ]
                | None -> co

            co
            |> set sl.Name Mapping.OrderableDoseCount Props.Vals sl.DoseCount
            |> set sl.Name Mapping.ItemOrderableConc Props.MinIncl sl.MinConcentration
            |> set sl.Name Mapping.ItemOrderableConc Props.MaxIncl sl.MaxConcentration
            |> set sl.Name Mapping.Time Props.MinIncl sl.MinTime
            |> set sl.Name Mapping.Time Props.MaxIncl sl.MaxTime
            |> fun co ->

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
                    co
                    >|> [ Constraint.create n 
                            Mapping.ComponentOrderableQty 
                            Props.Vals (dq |> List.map snd) 
                            RouteShape.Any OrderType.Any ]
                | _ -> co
            |> fun co ->
                let dq =
                    o.Orderable.Dose
                    |> VariableUnit.Dose.getQuantity
                    |> VariableUnit.Quantity.getUnitValues
                    |> Seq.toList

                co
                >|> [ Constraint.create o.Orderable.Name 
                                        Mapping.OrderableOrderableQty Props.Vals dq 
                                        RouteShape.Any OrderType.Any ]



        let setAdjust n a (co : ConstrainedOrder) : ConstrainedOrder =
            let (_, o) = co

            co
            >|> [ Constraint.create ([n] |> Name.create) 
                                    Mapping.AdjustQty Props.Vals [a] 
                                    RouteShape.Any OrderType.Any ]

        let evaluate (co : ConstrainedOrder) =
            let (cs, o) = co

            Constraint.apply cs o


module Constraint = DrugOrder.Constraint


let printScenarios n sc =
    sc
    |> List.iteri (fun i o ->
        o
        |> Order.printPrescription [n]
        |> printfn "%i\t%s" (i + 1)

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
|> printScenarios "paracetamol"
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
|> printScenarios "paracetamol"



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
|> printScenarios "dopamin"




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
|> printScenarios "dopamin"




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
//            MaxTime = (Some 10N)

    }
|> DrugOrder.evaluate
//|> Order.calcScenarios2
|> printScenarios "gentamicin"




let product xs =
    xs 
    |> List.fold (fun acc (n, vs) ->
        let xs =
            vs
            |> List.map (fun v -> 
                (n, v)
            )
        if acc |> List.length = 0 then [ xs ]
        else
            [
                for x in acc do
                    for nv in xs do
                        nv::x
            ]
    ) []

[
    ("1", [1..4])
    ("2", [5..7])
    ("3", [8..20])
]
|> product
|> List.length


[
    for x in [1N..250N] do
        for y in [1N..250N] do
            x * y
]
|> List.length
