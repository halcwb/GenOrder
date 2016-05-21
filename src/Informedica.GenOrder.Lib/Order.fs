namespace Informedica.GenOrder.Lib

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Order =

    module Literals =

        [<Literal>]
        let adjust = "Adjust"

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
            | ComponentOrderableConc
            | ComponentDoseQty
            | ComponentDoseTotal
            | ComponentDoseRate
            | ComponentDoseAdjustQtyAdjust
            | ComponentDoseAdjustTotalAdjust
            | ComponentDoseAdjustRateAdjust
            | OrderableOrderableQty
            | OrderableOrderQty
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
            | ComponentOrderableConc -> "Component.Orderable.Conc"
            | ComponentDoseQty -> "Component.Dose.Qty"
            | ComponentDoseTotal -> "Component.Dose.Total"
            | ComponentDoseRate -> "Component.Dose.Rate"
            | ComponentDoseAdjustQtyAdjust -> "Component.DoseAdjust.QtyAdjust"
            | ComponentDoseAdjustTotalAdjust -> "Component.DoseAdjust.TotalAdjust"
            | ComponentDoseAdjustRateAdjust -> "Component.DoseAdjust.RateAdjust"
            | OrderableOrderableQty -> "Orderable.Orderable.Qty"
            | OrderableOrderQty -> "Orderable.Order.Qty"
            | OrderableDoseQty -> "Orderable.Dose.Qty"
            | OrderableDoseTotal -> "Orderable.Dose.Total"
            | OrderableDoseRate -> "Orderable.Dose.Rate"
            | OrderableDoseAdjustQtyAdjust -> "Orderable.DoseAdjust.QtyAdjust"
            | OrderableDoseAdjustTotalAdjust -> "Orderable.DoseAdjust.TotalAdjust"
            | OrderableDoseAdjustRateAdjust -> "Orderable.DoseAdjust.RateAdjust"
            | AdjustQty -> "Adjust.Qty"

    module StartStop =
        
        open System

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

    type Order = 
        {
            Adjust: QT.Quantity
            Orderable: OR.Orderable
            Prescription: PR.Prescription
            Route: string // Route.T
            StartStop: DT.StartStop
        }


    let apply f (o: Order) = o |> f

    let get = apply id

    let create adj ord pre rte sts =
        {
            Adjust = adj 
            Orderable = ord
            Prescription = pre
            Route = rte
            StartStop = sts
        }

    let createNew adju ord pre rte = 
        let adj = adju |> QT.quantity [LT.adjust]
        let sts = DateTime.Now  |> DT.Start
        create adj ord pre rte sts

    let toProd xs = 
        match xs with
        | h::tail -> VU.toProdEq id (fun m -> printfn "%A" m; failwith "Oops") h tail |> EQ.nonZeroOrNegative
        | _ -> failwith "Not a valid equation"

    let toSum xs =
        match xs with
        | h::tail -> VU.toSumEq  id (fun _ -> failwith "Oops") h tail |> EQ.nonZeroOrNegative
        | _ -> failwith "Not a valid equation"

    let toEqs (o: Order) =
        let ord = o.Orderable
        let adj = o.Adjust |> QT.toVar
        let frq, tme = o.Prescription |> PR.toVarUns
        
        ord |> OR.toVarUns adj frq tme

    let fromEqs o vus =
        let frq, tme =
            match (o |> get).Prescription with
            | PR.Process    -> FR.frequency, TM.time
            | PR.Continuous -> FR.frequency, TM.time
            | PR.Discontinuous (frq) -> frq, TM.time
            | PR.Timed(frq, tme)     -> frq, tme
        
        let ord = o.Orderable |> OR.fromVarUns vus
        let adj = o.Adjust |> QT.fromVar vus
        let prs = o.Prescription |> PR.fromVarUns vus

        {
            o with
                Adjust = adj
                Orderable = ord
                Prescription = prs
        }

    let toString (ord: Order) =
        [ LT.adjust; ord.Adjust |> QT.toString ]
        |> List.append (OR.Literals.orderable::(ord.Orderable |> OR.toString))
        |> List.append ("Prescription"::(ord.Prescription |> PR.toString))
        |> List.append ("Route"::[(ord.Route)])
        

    let solve n m p v u o =
        let toEql (prod, sum) =
            prod 
            |> List.map toProd
            |> List.append [sum |> toSum]

        let toVars eqs = eqs |> List.map(fun e -> 
            match e with | EQ.ProductEquation(y, xs) | EQ.SumEquation(y, xs) -> y::xs)

        let toUnit n u (p, s) = 
            let us = u |> String.split "/"
            s::p 
            |> VU.findVarUnit n 
            |> VU.getUnitGroup 
            |> UG.toString
            |> String.split CS.divs
            |> List.fold2 (fun s u ug -> 
                if s = "" then u + CS.openBr + ug + CS.closBr
                else s + CS.divs + u + CS.openBr + ug + CS.closBr) "" us
            |> (fun s -> printfn "Created: %s" s; s)
            |> CU.fromString

        let dls = if n |> String.isNullOrWhiteSpace then "" else "."
        let n = [n + dls + (m |> Mapping.map)] |> NM.create
        let vus = o |> toEqs
        let cu = vus |> toUnit n u 

        vus 
        |> toEql
        |> SV.solve n p v cu
        |> toVars
        |> fromEqs o
