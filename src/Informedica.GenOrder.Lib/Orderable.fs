﻿namespace Informedica.GenOrder.Lib


/// Types and functions to deal 
/// with an `Orderable`, i.e. something
/// that can be ordered.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Orderable =

    open Informedica.GenUtils.Lib.BCL
    
    /// Contains string constants
    /// to create `Variable` names
    module Literals =

        [<Literal>]
        let item = "Item"
        [<Literal>]
        let ``component`` = "Component"
        [<Literal>]
        let orderable = "Orderable"
        [<Literal>]
        let order = "Order"
        [<Literal>]
        let dose = "Dose"
        [<Literal>]
        let doseAdjust = "DoseAdjust"

    /// Type and functions that models an
    /// `Order` `Item` that is contained in 
    /// a `Component`
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Item =

        module LT = Literals
        module VU = VariableUnit
        module NM = VU.Name
        module QT = VU.Quantity
        module CN = VU.Concentration
        module TL = VU.Total
        module RT = VU.Rate
        module DS = VU.Dose
        module DA = VU.DoseAdjust

        /// Models an `Item` in a `Component`
        type Item = 
            {
                /// The quantity of an `Item` in a `Component`
                ComponentQuantity: QT.Quantity 
                /// The quantity of an `Item` in an `Orderable`
                OrderableQuantity: QT.Quantity
                /// The `Item` concentration in a `Component`
                ComponentConcentration: CN.Concentration 
                /// The  `Item` concentration in an `Orderable`
                OrderableConcentration: CN.Concentration 
                /// The `Item` `Dose`, i.e. quanity, total and rate of `Item` administered
                Dose: DS.Dose
                // The `Item` `DoseAdjust`,  i.e. adjusted quanity, total and rate of `Item` administered
                DoseAdjust: DA.DoseAdjust
            }
        
        /// Create an item with
        ///
        /// * `cmp_qty`: the quantity of the item in a component
        /// * `orb_qty`: the quantity of the item in an orderable
        /// * `cmp_cnc`: the item concentration in a component
        /// * `orb_cnc`: the item concentration in an orderable
        /// * `dos`: the item dose
        /// * `dos_adj`: the adjusted item dose
        let create cmp_qty orb_qty cmp_cnc orb_cnc dos dos_adj = 
            { 
                ComponentQuantity = cmp_qty
                OrderableQuantity = orb_qty
                ComponentConcentration = cmp_cnc
                OrderableConcentration = orb_cnc
                Dose = dos
                DoseAdjust = dos_adj 
            }

        /// Create a new item with
        ///
        /// `s`: the name of the item
        /// `u1`: the unit of the item
        /// `u2`: the unit of the component that contains the item
        /// `adj`: the unit to adjust the item dose
        let createNew (s, u1) u2 adj =
            let s = [LT.item] |> List.append [s]
            let cq = let s = [LT.``component``] |> List.append s in QT.quantity s u1
            let oq = let s = [LT.orderable]     |> List.append s in QT.quantity s u1
            let cc = let s = [LT.``component``] |> List.append s in CN.conc s u1 u2
            let oc = let s = [LT.orderable]     |> List.append s in CN.conc s u1 u2
            let ds = let s = [LT.dose]          |> List.append s in DS.dose s u1
            let da = let s = [LT.doseAdjust]    |> List.append s in DA.doseAdjust s u1 adj

            create cq oq cc oc ds da

        /// Aply `f` to an `item`
        let apply f (itm: Item) = itm |> f

        /// Utility method to facilitaite type inference
        let get = apply id

        /// Turn an item to a list of string fields
        let toString item = 
            let cq = (item |> get).ComponentQuantity
            let oq = item.OrderableQuantity
            let cc = item.ComponentConcentration
            let oc = item.OrderableConcentration
            let dq, dt, dr = item.Dose |> DS.toVar
            let aq, at, ar = item.DoseAdjust |> DA.toVar

            [cq |> QT.toVar; oq |> QT.toVar; cc |> CN.toVar; oc |> CN.toVar; dq; dt; dr; aq; at; ar ]
            |> List.map VU.toString

        /// The following equations are generated
        /// depending on prescription type
        ///
        /// *Process*
        ///
        /// * itm\_cmp\_qty = itm\_cmp\_cnc \* cmp\_cmp\_qty
        /// * itm\_orb\_qty = itm\_orb\_cnc \* orb\_orb\_qty
        /// * itm\_orb\_qty = itm\_cmp\_cnc \* cmp\_orb\_qty
        ///
        /// *Discontinuous Timed*
        ///
        /// * itm\_dos\_tot = itm\_dos\_qty \* frq 
        /// * itm\_dos\_qty = itm\_dos\_rte \* tme
        /// * itm\_dos\_qty = itm\_orb\_cnc \* qty
        /// * itm\_dos\_tot = itm\_orb\_cnc \* tot
        /// * itm\_dos\_rte = itm\_orb\_cnc \* rte
        /// * itm\_dos\_qty = itm\_dos\_qty\_adj \* adj
        /// * itm\_dos\_tot = itm\_dos\_tot\_adj \* adj
        /// * itm\_dos\_rte = itm\_dos\_rte\_adj \* adj
        ///
        /// *Discontinuous*
        ///
        /// * itm\_dos\_tot = itm\_dos\_qty \* frq 
        /// * itm\_dos\_qty = itm\_orb\_cnc \* qty
        /// * itm\_dos\_tot = itm\_orb\_cnc \* tot
        /// * itm\_dos\_qty = itm\_dos\_qty\_adj \* adj
        /// * itm\_dos\_tot = itm\_dos\_tot\_adj \* adj
        ///
        /// *Continuous*
        ///
        /// * itm\_dos\_rte = itm\_orb\_cnc \* rte
        /// * itm\_dos\_rte = itm\_dos\_rte\_adj \* adj
        let toEqs adj frq qty tot tme rte cmp_cmp_qty cmp_orb_qty orb_orb_qty item =
            let itm_cmp_qty = (item |> get).ComponentQuantity |> QT.toVar
            let itm_orb_qty = item.OrderableQuantity          |> QT.toVar
            let itm_cmp_cnc = item.ComponentConcentration     |> CN.toVar
            let itm_orb_cnc = item.OrderableConcentration     |> CN.toVar

            let itm_dos_qty, itm_dos_tot, itm_dos_rte = item.Dose                   |> DS.toVar
            let itm_dos_qty_adj, itm_dos_tot_adj, itm_dos_rte_adj = item.DoseAdjust |> DA.toVar

            let eqs =
                [
                    [ itm_cmp_qty; itm_cmp_cnc; cmp_cmp_qty ]
                    [ itm_orb_qty; itm_orb_cnc; orb_orb_qty ]
                    [ itm_orb_qty; itm_cmp_cnc; cmp_orb_qty ]
                ]

            match rte, frq, tme with
            // Discontinuous timed
            | Some rte, Some frq, Some tme ->
                [
                    [ itm_dos_tot; itm_dos_qty;     frq ] 
                    [ itm_dos_qty; itm_dos_rte;     tme ]
                    [ itm_dos_qty; itm_orb_cnc;     qty ]
                    [ itm_dos_tot; itm_orb_cnc;     tot ]
                    [ itm_dos_rte; itm_orb_cnc;     rte ]
                    [ itm_dos_qty; itm_dos_qty_adj; adj ]
                    [ itm_dos_tot; itm_dos_tot_adj; adj ]
                    [ itm_dos_rte; itm_dos_rte_adj; adj ]
                ] |> List.append eqs
            // Discontinuous
            | None, Some frq, None   ->
                [
                    [ itm_dos_tot; itm_dos_qty;     frq ] 
                    [ itm_dos_qty; itm_orb_cnc;     qty ]
                    [ itm_dos_tot; itm_orb_cnc;     tot ]
                    [ itm_dos_qty; itm_dos_qty_adj; adj ]
                    [ itm_dos_tot; itm_dos_tot_adj; adj ]
                ] |> List.append eqs
            // Continuous
            | Some rte, _, _ ->
                [
                    [ itm_dos_rte; itm_orb_cnc;     rte ]
                    [ itm_dos_rte; itm_dos_rte_adj; adj ]
                ] |> List.append eqs   
            // Process             
            | _ -> eqs

        /// create an item from `eqs`, a list of
        /// variable lists
        let fromEqs eqs (itm: Item) =
            {
                itm with
                    ComponentQuantity = QT.fromVar eqs itm.ComponentQuantity
                    OrderableQuantity = QT.fromVar eqs itm.OrderableQuantity             
                    ComponentConcentration = CN.fromVar eqs itm.ComponentConcentration
                    OrderableConcentration = CN.fromVar eqs itm.OrderableConcentration
                    Dose = DS.fromVar eqs itm.Dose
                    DoseAdjust = DA.fromVar eqs itm.DoseAdjust
            }    

    /// Types and functions to model a 
    /// `Component` in an `Orderable`. 
    /// A `Component` contains a list 
    /// of `Item`s
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Component =

        module LT = Literals
        module VU = VariableUnit
        module QT = VU.Quantity
        module CN = VU.Concentration
        module TL = VU.Total
        module RT = VU.Rate
        module DS = VU.Dose
        module DA = VU.DoseAdjust
        module IT = Item

        /// Models in a `Component` in and `Orderable`
        type Component = 
            {
                /// The quantity of a `Component`
                ComponentQuantity: QT.Quantity
                /// The quantity of a `Component` in an `Orderable`
                OrderableQuantity: QT.Quantity
                /// The quantity of a `Component` in an `Order`
                OrderQuantity: QT.Quantity
                /// The concentration of a `Component` in an `Orderable`
                OrderableConcentration: CN.Concentration
                // The `Component` `Dose`,  
                /// i.e. quanity, total and rate of `Component` administered
                Dose: DS.Dose
                // The `Component` `DoseAdjust`,  
                /// i.e. adjusted quanity, total and rate of `Component` administered
                DoseAdjust: DA.DoseAdjust
                /// The `Item`s in a `Component`
                Items: IT.Item * IT.Item list
            }
        
        /// Create a component with
        ///
        /// * `cmp_qty`: quantity of component
        /// * `orb_qty`: quantity of component in orderable
        /// * `ord_qty`: quantity of component in order
        /// * `orb_cnc`: concentration of component in orderble
        /// * `dos`: component dose
        /// * `dos_adj`: adjusted dose of component
        /// * `ii`: list of `Item`s in a component
        let create cmp_qty orb_qty ord_qty orb_cnc dos dos_adj ii = 
            match ii with
            | h::tail ->
                { 
                    ComponentQuantity = cmp_qty
                    OrderableQuantity = orb_qty
                    OrderQuantity = ord_qty
                    OrderableConcentration = orb_cnc
                    Dose = dos
                    DoseAdjust = dos_adj 
                    Items = h, tail
                }
            | _ -> failwith "Not a valid component"

        /// Create a new component with
        ///
        /// * `sl`: list of item name and unit name tuples
        /// * `u1`: unit name of component
        /// * `adj`: adjust unit to adjust the dose
        let createNew sl u adj =
            let s = [LT.``component``] |> List.append [sl |> List.map fst |> String.concat "/"] 
            let cq = let s = [LT.``component``] |> List.append s in QT.quantity s u
            let oq = let s = [LT.orderable]     |> List.append s in QT.quantity s u
            let rq = let s = [LT.order]         |> List.append s in QT.quantity s u
            let oc = let s = [LT.orderable]     |> List.append s in CN.conc s u u
            let ds = let s = [LT.dose]          |> List.append s in DS.dose s u
            let da = let s = [LT.doseAdjust]    |> List.append s in DA.doseAdjust s u adj

            let ii = sl |> List.map (fun s -> IT.createNew s u adj)

            create cq oq rq oc ds da ii

        /// Apply `f` to a `Component` `comp` 
        let apply f (comp: Component) = comp |> f

        /// Utility to facilitate type inference
        let get = apply id

        /// Create a string list from a 
        /// component
        let toString comp = 
            let cq = (comp |> get).ComponentQuantity
            let oq = comp.OrderableQuantity
            let oc = comp.OrderableConcentration
            let dq, dt, dr = comp.Dose |> DS.toVar
            let aq, at, ar = comp.DoseAdjust |> DA.toVar

            let i, ii = comp.Items

            [cq |> QT.toVar; oq |> QT.toVar; oc |> CN.toVar; dq; dt; dr; aq; at; ar ]
            |> List.map VU.toString
            |> List.append (i::ii |> List.collect IT.toString )


        /// The following equations are generated:
        ///
        /// *Process*
        ///
        /// * orb\_orb\_qty = cmp\_cmp\_qty \* cmp\_orb\_qty
        /// * cmp\_orb\_qty = cmp\_orb\_cnc \* orb\_orb\_qty
        ///
        /// *Discontinuous Timed*
        ///
        /// * cmp\_dos\_tot = cmp\_dos\_qty \* frq 
        /// * cmp\_dos\_qty = cmp\_dos\_rte \* tme
        /// * cmp\_dos\_qty = cmp\_orb\_cnc \* qty
        /// * cmp\_dos\_tot = cmp\_orb\_cnc \* tot
        /// * cmp\_dos\_rte = cmp\_orb\_cnc \* rte
        /// * cmp\_dos\_qty = cmp\_dos\_qty\_adj \* adj
        /// * cmp\_dos\_tot = cmp\_dos\_tot\_adj \* adj
        /// * cmp\_dos\_rte = cmp\_dos\_rte\_adj \* adj
        ///
        /// *Discontinuous*
        ///
        /// * cmp\_dos\_tot = cmp\_dos\_qty \* frq 
        /// * cmp\_dos\_qty = cmp\_orb\_cnc \* qty
        /// * cmp\_dos\_tot = cmp\_orb\_cnc \* tot
        /// * cmp\_dos\_qty = cmp\_dos\_qty\_adj \* adj
        /// * cmp\_dos\_tot = cmp\_dos\_tot\_adj \* adj
        ///
        /// *Continuous*
        ///
        /// * cmp\_dos\_rte = cmp\_orb\_cnc \* rte
        /// * cmp\_dos\_rte = cmp\_dos\_rte\_adj \* adj
        let toEqs adj frq qty tot tme rte orb_orb_qty cmp =
            let cmp_cmp_qty = (cmp |> get).ComponentQuantity |> QT.toVar
            let cmp_orb_qty = cmp.OrderableQuantity          |> QT.toVar
            let cmp_orb_cnc = cmp.OrderableConcentration     |> CN.toVar

            let cmp_dos_qty,     cmp_dos_tot,     cmp_dos_rte     = cmp.Dose       |> DS.toVar
            let cmp_dos_qty_adj, cmp_dos_tot_adj, cmp_dos_rte_adj = cmp.DoseAdjust |> DA.toVar

            let map = IT.toEqs adj frq qty tot tme rte cmp_cmp_qty cmp_orb_qty orb_orb_qty
            let i, ii = cmp.Items

            let eqs =
                [
                    [ orb_orb_qty; cmp_cmp_qty; cmp_orb_qty ]
                    [ cmp_orb_qty; cmp_orb_cnc; orb_orb_qty ]
                ] 

            match rte, frq, tme with
            // Discontinuous timed
            | Some rte, Some frq, Some tme ->
                [
                    [ cmp_dos_tot; cmp_dos_qty;     frq  ]
                    [ cmp_dos_qty; cmp_dos_rte;     tme ]
                    [ cmp_dos_qty; cmp_orb_cnc;     qty ]
                    [ cmp_dos_tot; cmp_orb_cnc;     tot ]
                    [ cmp_dos_rte; cmp_orb_cnc;     rte ]
                    [ cmp_dos_qty; cmp_dos_qty_adj; adj ]
                    [ cmp_dos_tot; cmp_dos_tot_adj; adj ]
                    [ cmp_dos_rte; cmp_dos_rte_adj; adj ]
                ] |> List.append eqs
            // Discontinuous
            | None, Some frq, None   ->
                [
                    [ cmp_dos_tot; cmp_dos_qty;     frq ]
                    [ cmp_dos_qty; cmp_orb_cnc;     qty ]
                    [ cmp_dos_tot; cmp_orb_cnc;     tot ]
                    [ cmp_dos_qty; cmp_dos_qty_adj; adj ]
                    [ cmp_dos_tot; cmp_dos_tot_adj; adj ]
                ] |> List.append eqs
            // Continuous
            | Some rte, _, _ ->
                [
                    [ cmp_dos_rte; cmp_orb_cnc;     rte ]
                    [ cmp_dos_rte; cmp_dos_rte_adj; adj ]
                ] |> List.append eqs
            // Process
            | _ -> eqs
            |> List.append (i::ii |> List.collect map)

        /// Create a `Component` from a list
        /// of variable list eqs 
        let fromEqs eqs (cmp: Component) =
            let items = 
                (cmp.Items |> fst)::(cmp.Items |> snd)
                |> List.map (IT.fromEqs eqs)
            {
                cmp with
                    ComponentQuantity = QT.fromVar eqs cmp.ComponentQuantity
                    OrderableQuantity = QT.fromVar eqs cmp.OrderableQuantity             
                    OrderableConcentration = CN.fromVar eqs cmp.OrderableConcentration
                    Dose = DS.fromVar eqs cmp.Dose
                    DoseAdjust = DA.fromVar eqs cmp.DoseAdjust
                    Items = items.Head, items.Tail
            }    


    module NM = Informedica.GenSolver.Lib.Variable.Name
    module LT = Literals
    module VU = VariableUnit
    module QT = VU.Quantity
    module CN = VU.Concentration
    module TL = VU.Total
    module RT = VU.Rate
    module DS = VU.Dose
    module DA = VU.DoseAdjust
    module CM = Component

    /// Models an `Orderable` 
    type Orderable = 
        {
            OrderableQuantity: QT.Quantity
            OrderQuantity: QT.Quantity
            Dose: DS.Dose
            DoseAdjust: DA.DoseAdjust
            Components: CM.Component * CM.Component list
        }
        
    /// Create an `Orderable` with an 
    /// `OrderableQuantity` `oq`, an 
    /// `OrderQuantity` `rq`, `Dose` `ds` 
    /// and `DoseAdjust` `da`, with a list
    /// of `Component`s
    let create oq rq ds da cc = 
        match cc with
        | h::tail ->
            { 
                OrderableQuantity = oq
                OrderQuantity = rq
                Dose = ds
                DoseAdjust = da 
                Components = h, tail
            }
        | _ -> failwith "Not a valid Orderable"

    let createNew cl u adj =
        let s = 
            [
                cl 
                |> List.map (List.map fst) 
                |> List.map (String.concat "/")
                |> String.concat " & " ] @ [LT.orderable]

        let oq = let s = [LT.orderable] |> List.append s in QT.quantity s u
        let rq = let s = [LT.order] |> List.append s in QT.quantity s u
        let ds = let s = [LT.dose] |> List.append s in DS.dose s u
        let da = let s = [LT.doseAdjust] |> List.append s in DA.doseAdjust s u adj

        let cc = cl |> List.map (fun c -> CM.createNew c u adj)

        create oq rq ds da cc

    let apply f (ord: Orderable) = ord |> f

    let get = apply id

    let getName = apply (fun o -> 
        o.OrderableQuantity 
        |> QT.toVar 
        |> VU.getName 
        |> NM.toString
        |> String.split "."
        |> List.head)

    let toString ord = 
        let oq = (ord |> get).OrderableQuantity
        let rq = ord.OrderQuantity
        let dq, dt, dr = ord.Dose       |> DS.toVar
        let aq, at, ar = ord.DoseAdjust |> DA.toVar

        let c, cc = ord.Components

        [oq |> QT.toVar; rq |> QT.toVar; dq; dt; dr; aq; at; ar ]
        |> List.map VU.toString
        |> List.append (c::cc |> List.collect CM.toString )


    /// The following equations are generated:
    ///
    /// * ord\_ord\_qty = ord\_ord\_qty \* ord\_ord\_qty
    /// * ord\_ord\_qty = ord\_ord\_cnc \* ord\_ord\_qty
    /// * ord\_dos\_tot = ord\_dos\_qty \* frq 
    /// * ord\_dos\_qty = ord\_dos\_rte \* tme
    /// * ord\_dos\_qty = ord\_ord\_cnc \* qty
    /// * ord\_dos\_tot = ord\_ord\_cnc \* tot
    /// * ord\_dos\_rte = ord\_ord\_cnc \* rte
    /// * ord\_dos\_qty = ord\_dos\_qty\_adj \* adj
    /// * ord\_dos\_tot = ord\_dos\_tot\_adj \* adj
    /// * ord\_dos\_rte = ord\_dos\_rte\_adj \* adj
    let toEqs hasRte adj frq tme ord =
        let ord_der_qty = (ord |> get).OrderQuantity |> QT.toVar
        let ord_ord_qty = ord.OrderableQuantity      |> QT.toVar

        let ord_dos_qty,     ord_dos_tot,     ord_dos_rte     = ord.Dose       |> DS.toVar
        let ord_dos_qty_adj, ord_dos_tot_adj, ord_dos_rte_adj = ord.DoseAdjust |> DA.toVar

        let qty = ord_dos_qty
        let tot = ord_dos_tot
        let rte = if hasRte then ord_dos_rte |> Some else None

        let qty_adj = ord_dos_qty_adj
        let tot_adj = ord_dos_tot_adj
        let rte_adj = ord_dos_rte_adj

        let map = CM.toEqs adj frq qty tot tme rte ord_ord_qty
        let c, cc = ord.Components

        let eqs = 
            [
                [ tot; ord_der_qty; ord_ord_qty ]
            ] 
            

        match rte, frq, tme with
        // Discontinuous timed
        | Some rte, Some frq, Some tme ->
            [
                [tot; qty; frq]
                [ tot_adj; qty_adj; frq ]
            ] |> List.append eqs
        // Discontinuous
        | None, Some frq, None   ->
            [
                [tot; qty; frq]
            ] |> List.append eqs
        // Continuous or Process
        | _ -> eqs
        |> List.append (c::cc |> List.collect map)
        , ord_ord_qty::(c::cc |> List.map (fun c -> c.OrderableQuantity |> QT.toVar))


    let fromEqs eqs (ord: Orderable) =
        let cmps = 
            (ord.Components |> fst)::(ord.Components |> snd)
            |> List.map (CM.fromEqs eqs)
        {
            ord with
                OrderableQuantity = QT.fromVar eqs ord.OrderableQuantity             
                OrderQuantity = QT.fromVar eqs ord.OrderQuantity
                Dose = DS.fromVar eqs ord.Dose
                DoseAdjust = DA.fromVar eqs ord.DoseAdjust
                Components = cmps.Head, cmps.Tail
        }

