﻿namespace Informedica.GenOrder.Lib

/// Types and functions that deal with an order.
/// An `Order` models the `Prescription` of an 
/// `Orderable` with a `StartStop` start date and
/// stop date.
module Order =

    open Informedica.GenUtils.Lib.BCL

    /// Contains literals used
    /// to generate `Variable` names
    module Literals =

        [<Literal>]
        let adjust = "Adjust"

    /// Utitlity functions to
    /// enable mapping of a `Variable`s
    /// to an `Order`
    module Mapping =

        type Map =
            | Freq
            | Time
            | ItemComponentQty
            | ItemOrderableQty
            | ItemComponentConc
            | ItemOrderableConc
            | ItemDoseQty
            | ItemDoseTotal
            | ItemDoseRate
            | ItemDoseAdjustQtyAdjust
            | ItemDoseAdjustTotalAdjust
            | ItemDoseAdjustRateAdjust
            | ComponentComponentQty
            | ComponentOrderableQty
            | ComponentOrderableCount
            | ComponentOrderCount
            | ComponentOrderableConc
            | ComponentDoseQty
            | ComponentDoseTotal
            | ComponentDoseRate
            | ComponentDoseAdjustQtyAdjust
            | ComponentDoseAdjustTotalAdjust
            | ComponentDoseAdjustRateAdjust
            | OrderableOrderableQty
            | OrderableOrderQty
            | OrderableOrderCount
            | OrderableDoseCount
            | OrderableDoseQty
            | OrderableDoseTotal
            | OrderableDoseRate
            | OrderableDoseAdjustQtyAdjust
            | OrderableDoseAdjustTotalAdjust
            | OrderableDoseAdjustRateAdjust
            | AdjustQty

        let map = function
            | Freq -> "Freq"
            | Time -> "Time"
            | ItemComponentQty -> "Item.Component.Qty"
            | ItemOrderableQty -> "Item.Orderable.Qty"
            | ItemComponentConc -> "Item.Component.Conc"
            | ItemOrderableConc -> "Item.Orderable.Conc"
            | ItemDoseQty -> "Item.Dose.Qty"
            | ItemDoseTotal -> "Item.Dose.Total"
            | ItemDoseRate -> "Item.Dose.Rate"
            | ItemDoseAdjustQtyAdjust -> "Item.DoseAdjust.QtyAdjust"
            | ItemDoseAdjustTotalAdjust -> "Item.DoseAdjust.TotalAdjust"
            | ItemDoseAdjustRateAdjust -> "Item.DoseAdjust.RateAdjust"
            | ComponentComponentQty -> "Component.Component.Qty"
            | ComponentOrderableQty -> "Component.Orderable.Qty"
            | ComponentOrderableCount -> "Component.Orderable.Count"
            | ComponentOrderCount -> "Component.Order.Count"
            | ComponentOrderableConc -> "Component.Orderable.Conc"
            | ComponentDoseQty -> "Component.Dose.Qty"
            | ComponentDoseTotal -> "Component.Dose.Total"
            | ComponentDoseRate -> "Component.Dose.Rate"
            | ComponentDoseAdjustQtyAdjust -> "Component.DoseAdjust.QtyAdjust"
            | ComponentDoseAdjustTotalAdjust -> "Component.DoseAdjust.TotalAdjust"
            | ComponentDoseAdjustRateAdjust -> "Component.DoseAdjust.RateAdjust"
            | OrderableOrderableQty -> "Orderable.Orderable.Qty"
            | OrderableOrderQty -> "Orderable.Order.Qty"
            | OrderableOrderCount -> "Orderable.Order.Count"
            | OrderableDoseCount -> "Orderable.Dose.Count"
            | OrderableDoseQty -> "Orderable.Dose.Qty"
            | OrderableDoseTotal -> "Orderable.Dose.Total"
            | OrderableDoseRate -> "Orderable.Dose.Rate"
            | OrderableDoseAdjustQtyAdjust -> "Orderable.DoseAdjust.QtyAdjust"
            | OrderableDoseAdjustTotalAdjust -> "Orderable.DoseAdjust.TotalAdjust"
            | OrderableDoseAdjustRateAdjust -> "Orderable.DoseAdjust.RateAdjust"
            | AdjustQty -> "Adjust.Qty"



    /// Types and functions that
    /// model a start and stop date time
    /// of an `Order`
    module StartStop =
        
        open System

        /// There is always a `Start` or
        /// both a `StartStop`
        type StartStop =
            | Start of DateTime
            | StartStop of DateTime * DateTime

        let toString stst =
            match stst with
            | Start dt ->
                dt
                |> DateTime.formattedString "dd-MM-yy"
                |> sprintf "%s"
            | StartStop (start, stop) ->
                stop
                |> DateTime.formattedString "dd-MM-yy"
                |> sprintf "%s - %s" (start |> DateTime.formattedString "dd-MM-yy")

    open System

    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib
    open WrappedString

    module ValueRange = Informedica.GenSolver.Lib.Variable.ValueRange
    module Equation = Informedica.GenSolver.Lib.Equation
    module Props = Informedica.GenSolver.Lib.Props
    module Quantity = VariableUnit.Quantity
    module Frequency = VariableUnit.Frequency
    module Time = VariableUnit.Time
    module Units = ValueUnit.Units

    type Id = Id.Id
    type Quantity = Quantity.Quantity
    type Orderable = Orderable.Orderable
    type Prescription = Prescription.Prescription
    type StartStop = StartStop.StartStop

    /// Models an order
    type Order = 
        {
            /// The id of an order
            Id: Id
            /// Used to adjust doses
            Adjust: Quantity
            /// That what can be ordered
            Orderable: Orderable
            /// How the orderable is prescribed
            Prescription: Prescription
            /// The route of administration of the order
            Route: string // Route.T
            /// The start stop date of the order
            StartStop: StartStop
        }


    /// Apply `f` to `Order` `ord`
    let apply f (ord: Order) = ord |> f

    /// Utilty function to facilitate type inference
    let get = apply id

    /// Get the order id
    let getId ord = (ord |> get).Id

    /// Create an `Order` with
    ///
    /// * id: the id of the order
    /// * adj: by which doses are adjusted
    /// * orb: the `Orderable`
    /// * prs: `Prescription`, how the orderable is prescribed
    /// * rte: the route of administration of the orderable
    let create id adj_qty orb prs rte sts =
        {
            Id = id
            Adjust = adj_qty 
            Orderable = orb
            Prescription = prs
            Route = rte
            StartStop = sts
        }

    /// Create a new orderable using:
    ///
    /// * id: the unique id in an `OrderSet` of an `Order`
    /// * nm: the name of the `Orderable`
    /// * cil: the names for the `Component`s, each with a list of name, string tuples for `Item`s
    /// * orb_un: the unit name for the `Orderable` and its `Component`s
    /// * cmp_un: the unit for the `Component`
    /// * adj_un: the unit used to adjust doses
    /// * tme_un: the unit for time
    /// * str_prs: a function that takes in a list of strings that will generate the names and returns a `Prescription`
    /// * route: the route of administration
    let createNew id n shape str_prs route = 
        let orb = Orderable.createNew id n shape
        let nm  = orb |> Orderable.getName
        let adj = Quantity.quantity [ id |> Id.toString; n |> Name.toString; Literals.adjust ] ValueUnit.NoUnit
        let prs = [id |> Id.toString; nm |> Name.toString] |> str_prs
        let sts = DateTime.Now  |> StartStop.Start

        create id adj orb prs route sts

    let getAdjust ord = (ord |> get).Adjust

    let getName ord = 
        ord 
        |> (getAdjust >> Quantity.toVarUnt >> VariableUnit.getName)
        |> Name.toString
        |> String.split "."
        |> List.take 2
        |> String.concat "."


    let mapName n m o =   
        let dls = "."

        match m with
        | Mapping.Freq 
        | Mapping.AdjustQty ->
            [ (o |> getName) + dls + (m |> Mapping.map)] |> Name.create
        | _ -> 
            [ (o.Id |> Id.toString) + dls + n + dls + (m |> Mapping.map)] 
            |> Name.create


    let getOrderable ord = (ord |> get).Orderable

    /// Map an `Order` *ord* to 
    /// a list of `VariableUnit` lists
    let toEqs (ord: Order) =
        let orb = ord.Orderable
        let adj = ord.Adjust |> Quantity.toVarUnt
        let frq, tme = ord.Prescription |> Prescription.toEqs
        let hasRate = 
            ord.Prescription |> Prescription.isContinuous || 
            ord.Prescription |> Prescription.isTimed
        
        orb |> Orderable.toEqs hasRate adj frq tme

    /// Map a list of `VariableUnit` lists
    /// to an `Order` *ord*
    let fromEqs ord eqs =        
        let orb = ord.Orderable |> Orderable.fromEqs eqs
        let adj = ord.Adjust    |> Quantity.fromVar eqs
        let prs = ord.Prescription |> Prescription.fromEqs eqs

        {
            ord with
                Adjust = adj
                Orderable = orb
                Prescription = prs
        }


    /// Turn an order into a list of string
    /// representing variable name, valuerange 
    /// and unit group
    let toString (ord: Order) =
        [ Literals.adjust; ord.Adjust |> Quantity.toString ]
        |> List.append (Orderable.Literals.orderable::(ord.Orderable |> Orderable.toString))
        |> List.append ("Prescription"::(ord.Prescription |> Prescription.toString))
        |> List.append ("Route"::[(ord.Route)])
        |> List.filter (String.isNullOrWhiteSpace >> not)


    let getVariableUnit n m o =
        let dls = "."

        let n =
            match m with
            | Mapping.Freq 
            | Mapping.AdjustQty ->
                [ (o |> getName) + dls + (m |> Mapping.map)] |> Name.create
            | _ -> 
                [ (o.Id |> Id.toString) + dls + n + dls + (m |> Mapping.map)] 
                |> Name.create

        let prod, sum = o |> toEqs

        sum @ prod 
        |> List.collect id
        |> List.tryFind (VariableUnit.getName >> ((=) n))


    let setVariableUnit o vru =

        let prod, sum = o |> toEqs

        sum @ prod 
        |> List.collect id
        |> List.map (fun x ->
            if vru |> VariableUnit.getName = x.Variable.Name  then vru else x
        )
        |> List.singleton
        |> fromEqs o


    let hasUnitValue n m v o =
        getVariableUnit n m o
        |> function 
        | Some vru -> 
            vru
            |> VariableUnit.containsUnitValue v
        | None -> false
    
    let contains n v o =
        o
        |> toEqs
        |> fun (prod, sum) ->
            let find =
                List.tryFind (fun vru ->
                    vru 
                    |> VariableUnit.getName 
                    |> ((=) n)
                )

            match prod @ sum |> List.collect id |> find with
            | Some vru -> vru |> VariableUnit.containsUnitValue v
            | None -> false


    //let isEmpty n m o =
    //    getVariableUnit n m o
    //    |> function 
    //    | Some vru -> 
    //        vru.Variable.Values
    //        |> ValueRange.isEmpty
    //    | None -> false

            
    let solveUnits o =
        // return eqs 
        let toEql prod sum =

            prod 
            |> List.map Solver.productEq
            |> List.append (sum |> List.map Solver.sumEq)

        let prod, sum = o |> toEqs
    
        let eqs = toEql prod sum
    
        eqs
        |> Solver.solveUnits
        |> List.map (fun e ->
            match e with 
            | Solver.ProductEquation (vru, vrus)
            | Solver.SumEquation (vru, vrus) -> vru::vrus
        )
        |> fromEqs o
        
        
    /// Solve an `Order` *ord* with
    /// 
    /// * n: the name of the variable to be set
    /// * m: the mapping for the field of the order
    /// * p: the property of the variable to be set
    /// * vs: the values to be set
    let solve log lim n m p o =
        let n =
            mapName n m o

        // return eqs 
        let toEql prod sum =

            prod 
            |> List.map Solver.productEq
            |> List.append (sum |> List.map Solver.sumEq)


        let prod, sum = o |> toEqs
        
        let eqs = toEql prod sum
        
        eqs
        |> Solver.solve log lim n p
        |> fromEqs o
        |> fun o ->
            o
            |> toString
            |> List.fold (fun acc x ->
                let i, s = acc
                i + 1, sprintf "%s\n%i\t%s" s i x
            ) (1, "")
            |> snd
            |> log

            o

    let solveConstraints log cs o =
        // return eqs 
        let toEql prod sum =

            prod 
            |> List.map Solver.productEq
            |> List.append (sum |> List.map Solver.sumEq)

        let prod, sum = o |> toEqs
    
        let eqs = toEql prod sum
    
        eqs
        |> Solver.solveConstraints log cs
        |> fromEqs o
        |> fun o ->
            o
            |> toString
            |> List.fold (fun acc x ->
                let i, s = acc
                i + 1, sprintf "%s\n%i\t%s" s i x
            ) (1, "")
            |> snd
            |> log

            o


    module Dto =
        
        type Dto (id , n, shape) =
            member val Id = id with get, set
            member val Adjust = VariableUnit.Dto.dto () with get, set
            member val Orderable = Orderable.Dto.dto id n shape with get, set
            member val Prescription = Prescription.Dto.dto n with get, set
            member val Route = "" with get, set
            member val Start = DateTime.now () with get, set
            member val Stop : DateTime option = None with get, set

        let fromDto (dto : Dto) =
            let id = dto.Id |> Id.create
            let adj_qty = dto.Adjust |> Quantity.fromDto
            let orb = dto.Orderable |> Orderable.Dto.fromDto
            let prs = dto.Prescription |> Prescription.Dto.fromDto
            let sts =
                match dto.Stop with
                | Some dt -> (dto.Start, dt) |> StartStop.StartStop
                | None -> dto.Start |> StartStop.Start

            create id adj_qty orb prs dto.Route sts


        let toDto (ord : Order) =
            let id = ord.Id |> Id.toString
            let n = ord.Orderable.Name |> Name.toString
            let dto = Dto (id, n, ord.Orderable.Shape)

            dto.Adjust <- ord.Adjust |> Quantity.toDto
            dto.Orderable <- ord.Orderable |> Orderable.Dto.toDto
            dto.Prescription <- ord.Prescription |> Prescription.Dto.toDto
            dto.Route <- ord.Route
            let (start, stop) =
                match ord.StartStop with
                | StartStop.Start dt -> (dt, None)
                | StartStop.StartStop(start, stop) -> (start, stop |> Some) 
            dto.Start <- start
            dto.Stop <- stop
            
            dto

        let ``process`` id n shape rte = 
            let id = id |> Id.create
            let n = [ n ] |> Name.create
            let str_prs = 
                fun _ -> Prescription.``process`` 
            
            createNew id n shape str_prs rte
            |> toDto

        let continuous id n shape rte = 
            let id = id |> Id.create
            let n = [ n ] |> Name.create
            let str_prs = 
                Prescription.continuous ValueUnit.NoUnit ValueUnit.NoUnit
    
            createNew id n shape str_prs rte
            |> toDto

        let discontinuous id n shape rte = 
            let id = id |> Id.create
            let n = [ n ] |> Name.create
            let str_prs = 
                Prescription.discontinuous ValueUnit.NoUnit ValueUnit.NoUnit
    
            createNew id n shape str_prs rte
            |> toDto

        let timed id n shape rte = 
            let id = id |> Id.create
            let n = [ n ] |> Name.create
            let str_prs = 
                Prescription.timed ValueUnit.NoUnit ValueUnit.NoUnit
    
            createNew id n shape str_prs rte
            |> toDto

        let setToProcess (dto : Dto) =
            dto.Prescription <-
                dto.Prescription 
                |> Prescription.Dto.setToProcess 
            dto

        let setToContinuous (dto : Dto) =
            dto.Prescription <-
                dto.Prescription 
                |> Prescription.Dto.setToContinuous 
            dto

        let setToDiscontinuous (dto : Dto) =
            dto.Prescription <-
                dto.Prescription 
                |> Prescription.Dto.setToDiscontinuous 
            dto

        let setToTimed (dto : Dto) =
            dto.Prescription <-
                dto.Prescription 
                |> Prescription.Dto.setToTimed 
            dto