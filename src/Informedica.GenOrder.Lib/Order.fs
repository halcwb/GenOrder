namespace Informedica.GenOrder.Lib

/// Types and functions that deal with an order.
/// An `Order` models the `Prescription` of an 
/// `Orderable` with a `StartStop` start date and
/// stop date.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Order =

    /// Contains literals used
    /// to generate `Variable` names
    module Literals =

        [<Literal>]
        let adjust = "Adjust"

    /// Utitlity functions to
    /// enable mapping of a `Variable`s
    /// to an `Order`
    module Mapping =

        module UN = Unit

        type Map =
            | Freq
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
            | OrderableDoseQty
            | OrderableDoseTotal
            | OrderableDoseRate
            | OrderableDoseAdjustQtyAdjust
            | OrderableDoseAdjustTotalAdjust
            | OrderableDoseAdjustRateAdjust
            | AdjustQty

        let map = function
            | Freq -> "Freq"
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

    open System

    open Informedica.GenUtils.Lib.BCL

    module LT = Literals
    module CS = Informedica.GenUnits.Lib.Constants
    module CU = Informedica.GenUnits.Lib.CombiUnit
    module UG = Informedica.GenUnits.Lib.UnitGroup
    module EQ = Informedica.GenSolver.Lib.Equation
    module OR = Orderable
    module PR = Prescription
    module DT = StartStop
    module SV = Solver
    module VU = VariableUnit
    module NM = VU.Name
    module QT = VU.Quantity
    module FR = VU.Frequency
    module TM = VU.Time

    /// Models an order
    type Order = 
        {
            /// Used to adjust doses
            Adjust: QT.Quantity
            /// That what can be ordered
            Orderable: OR.Orderable
            /// How the orderable is prescribed
            Prescription: PR.Prescription
            /// The route of administration of the order
            Route: string // Route.T
            /// The start stop date of the order
            StartStop: DT.StartStop
        }


    /// Apply `f` to `Order` `ord`
    let apply f (ord: Order) = ord |> f

    /// Utilty function to facilitate type inference
    let get = apply id

    /// Create an `Order` with
    ///
    /// * adj: by which doses are adjusted
    /// * orb: the `Orderable`
    /// * prs: `Prescription`, how the orderable is prescribed
    /// * rot: the route of administration of the orderable
    let create adj_qty orb prs rot sts =
        {
            Adjust = adj_qty 
            Orderable = orb
            Prescription = prs
            Route = rot
            StartStop = sts
        }

    /// Create a new orderable using:
    ///
    /// * adj_unt: the unit group by which doses are adjusted
    /// * orb: the orderable being ordered
    /// * str_prs: a function that takes in a list of strings 
    /// that will generate the names and returns a `Prescription`
    /// * rot: the route of administration
    let createNew cil shp_ung adj_ung str_prs rot = 
        let orb = OR.createNew cil shp_ung adj_ung
        let nm  = orb |> OR.getName
        let prs = [nm] |> str_prs
        let adj = adj_ung |> QT.quantity [nm; LT.adjust]
        let sts = DateTime.Now  |> DT.Start
        create adj orb prs rot sts

    let getAdjust ord = (ord |> get).Adjust

    let getName ord = 
        ord 
        |> (getAdjust >> QT.toVarUnt >> VU.getName)
        |> NM.toString
        |> String.split "."
        |> List.head

    /// Create a `ProductEquation` from
    /// a `VariableUnit` list
    let toProd xs = 
        match xs with
        | h::tail -> VU.toProdEq id (fun m -> printfn "%A" m; failwith "Oops") h tail |> EQ.nonZeroOrNegative
        | _ -> failwith "Not a valid equation"

    /// Create a `SumEquation` from
    /// a `VariableUnit` list
    let toSum xs =
        match xs with
        | h::tail -> VU.toSumEq  id (fun _ -> failwith "Oops") h tail |> EQ.nonZeroOrNegative
        | _ -> failwith "Not a valid equation"

    /// Map an `Order` *ord* to 
    /// a list of `VariableUnit` lists
    let toEqs (ord: Order) =
        let orb = ord.Orderable
        let adj = ord.Adjust |> QT.toVarUnt
        let frq, tme = ord.Prescription |> PR.toEqs
        let hasRate = 
            ord.Prescription |> PR.isContinuous || 
            ord.Prescription |> PR.isTimed
        
        orb |> OR.toEqs hasRate adj frq tme

    /// Map a list of `VariableUnit` lists
    /// to an `Order` *ord*
    let fromEqs ord vus =        
        let orb = ord.Orderable |> OR.fromEqs vus
        let adj = ord.Adjust |> QT.fromVar vus
        let prs = ord.Prescription |> PR.fromEqs vus

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
        [ LT.adjust; ord.Adjust |> QT.toString ]
        |> List.append (OR.Literals.orderable::(ord.Orderable |> OR.toString))
        |> List.append ("Prescription"::(ord.Prescription |> PR.toString))
        |> List.append ("Route"::[(ord.Route)])
        
    /// Solve an `Order` *ord* with
    /// 
    /// * n: the name of the variable to be set
    /// * m: the mapping for the field of the order
    /// * p: the property of the variable to be set
    /// * v: the values to be set
    let solve n m p v u o =
        let toEql (prod, sum) =
            prod 
            |> List.map toProd
            |> List.append [sum |> toSum]

        let toVars eqs = eqs |> List.map(fun e -> 
            match e with | EQ.ProductEquation(y, xs) | EQ.SumEquation(y, xs) -> y::xs)

        let toUnit n u (p, s) = 
            let us = u |> String.split "/"
            match s::p |> VU.tryFindVarUnt n with
            | Some vu -> 
                vu
                |> VU.getUnitGroup 
                |> UG.toString
                |> String.split CS.divs
                |> List.fold2 (fun s u ug -> 
                    if s = "" then u + CS.openBr + ug + CS.closBr
                    else s + CS.divs + u + CS.openBr + ug + CS.closBr) "" us
                |> (fun s -> printfn "Created: %s" s; s)
                |> CU.fromString
            | None -> sprintf "Cannot find VariableUnit %A" n |> failwith

        let dls = "."
        let n =
            match m with
            | Mapping.Freq -> [ (o |> getName) + dls + (m |> Mapping.map)] |> NM.create
            | _ -> [n + dls + (m |> Mapping.map)] |> NM.create
        let vus = o |> toEqs
        let cu = vus |> toUnit n u 

        vus 
        |> toEql
        |> SV.solve n p v cu
        |> toVars
        |> fromEqs o
