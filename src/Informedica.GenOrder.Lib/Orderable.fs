namespace Informedica.GenOrder.Lib


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

        module ID = Primitives.Id
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
                /// The id of the Order
                OrderId: ID.Id
                /// The name of the item
                Name: NM.Name
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
        /// * **id**: the order id
        /// * **nm**: the name of the item
        /// * **cmp_qty**: the quantity of the item in a component
        /// * **orb_qty**: the quantity of the item in an orderable
        /// * **cmp_cnc**: the item concentration in a component
        /// * **orb_cnc**: the item concentration in an orderable
        /// * **dos**: the item dose
        /// * **dos_adj**: the adjusted item dose
        let create id nm cmp_qty orb_qty cmp_cnc orb_cnc dos dos_adj = 
            {   
                OrderId = id
                Name = nm
                ComponentQuantity = cmp_qty
                OrderableQuantity = orb_qty
                ComponentConcentration = cmp_cnc
                OrderableConcentration = orb_cnc
                Dose = dos
                DoseAdjust = dos_adj
            }

        /// Create a new item with
        ///
        /// **id**: the order id
        /// **s**: the string name of the item
        /// **u1**: the unit of the item
        /// **u2**: the unit of the component that contains the item
        /// **adj**: the unit to adjust the item dose
        let createNew id (nm, u1) u2 adj =
            let s = [LT.item] |> List.append [id |> ID.toString; nm |> NM.toString]

            let cmp_qty = let s = [LT.``component``] |> List.append s in QT.quantity s u1
            let orb_qty = let s = [LT.orderable]     |> List.append s in QT.quantity s u1
            let cmp_cnc = let s = [LT.``component``] |> List.append s in CN.conc s u1 u2
            let orb_cnc = let s = [LT.orderable]     |> List.append s in CN.conc s u1 u2
            let dos     = let s = [LT.dose]          |> List.append s in DS.dose s u1
            let dos_adj = let s = [LT.doseAdjust]    |> List.append s in DA.doseAdjust s u1 adj

            create id nm cmp_qty orb_qty cmp_cnc orb_cnc dos dos_adj

        /// Aply **f** to an `item`
        let apply f (itm: Item) = itm |> f

        /// Utility method to facilitaite type inference
        let get = apply id

        /// Get the order id of an `Item`
        let getOrderId itm = (itm |> get).OrderId

        /// Get the `Name` of an `Item`
        let getName itm = (itm |> get).Name

        /// Get the unique id of an `Item`
        let getId itm = 
            ((itm |> get).OrderId |> ID.toString) + "." +
            (itm.Name |> NM.toString)

        /// Get the `Item` dose
        let getDose itm = (itm |> get).Dose

        // Get the `Item` adjusted dose
        let getDoseAdjust itm = (itm |> get).DoseAdjust

        /// Turn an `Item` to `VariableUnit`s
        let toVarUnt itm =
            let itm_cmp_qty = (itm |> get).ComponentQuantity |> QT.toVarUnt
            let itm_orb_qty = itm.OrderableQuantity          |> QT.toVarUnt
            let itm_cmp_cnc = itm.ComponentConcentration     |> CN.toVarUnt
            let itm_orb_cnc = itm.OrderableConcentration     |> CN.toVarUnt

            let itm_dos_qty, itm_dos_tot, itm_dos_rte = itm.Dose                   |> DS.toVarUnt
            let itm_dos_qty_adj, itm_dos_tot_adj, itm_dos_rte_adj = itm.DoseAdjust |> DA.toVarUnt

            (
                itm_cmp_qty, 
                itm_orb_qty, 
                itm_cmp_cnc, 
                itm_orb_cnc, 
                itm_dos_qty, 
                itm_dos_tot, 
                itm_dos_rte,
                itm_dos_qty_adj, 
                itm_dos_tot_adj, 
                itm_dos_rte_adj
            )
            

        /// Turn an `Item` to a list of `string`s,
        /// each string containing the variable
        /// `Name`, `ValueRange` and `UnitGroup`
        let toString itm = 
            let itm_cmp_qty, 
                itm_orb_qty, 
                itm_cmp_cnc, 
                itm_orb_cnc, 
                itm_dos_qty, 
                itm_dos_tot, 
                itm_dos_rte,
                itm_dos_qty_adj, 
                itm_dos_tot_adj, 
                itm_dos_rte_adj = itm |> toVarUnt

            [
                itm_cmp_qty 
                itm_orb_qty 
                itm_cmp_cnc 
                itm_orb_cnc 
                itm_dos_qty 
                itm_dos_tot 
                itm_dos_rte
                itm_dos_qty_adj 
                itm_dos_tot_adj 
                itm_dos_rte_adj
            ] |> List.map VU.toString

        
        /// The following variables are used
        ///
        /// * itm\_cmp\_qty: the quantity of item in a component
        /// * itm\_orb\_qty: the quantity of item in an orderable
        /// * itm\_cmp\_cnc: the concentration of an item in a component
        /// * itm\_orb\_cnc: the concentration of an item in an orderable
        /// * cmp\_cmp\_qty: the component quantity
        /// * orb\_orb\_qty: the orderable quantity
        /// * cmp\_orb\_qty: the quantity of component in an orderable
        /// * itm\_dos\_qty: the item dose quantity
        /// * itm\_dos\_tot: the item dose total
        /// * itm\_dos\_rte: the item dose rate
        /// * itm\_dos\_qty\_adj: the adjusted item dose quantity
        /// * itm\_dos\_tot\_adj: the adjusted item dose total
        /// * itm\_dos\_rte\_adj: the adjusted item dose rate
        /// * frq: the prescription frequency
        /// * tme: the prescription time
        /// * qty: the orderable dose quantity
        /// * tot: the orderable dose total
        /// * rte: the orderable dose rate
        ///
        /// With these variables the following equations are generated
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
        let toEqs adj frq qty tot tme rte cmp_cmp_qty cmp_orb_qty orb_orb_qty itm =
            let itm_cmp_qty, 
                itm_orb_qty, 
                itm_cmp_cnc, 
                itm_orb_cnc, 
                itm_dos_qty, 
                itm_dos_tot, 
                itm_dos_rte,
                itm_dos_qty_adj, 
                itm_dos_tot_adj, 
                itm_dos_rte_adj = itm |> toVarUnt

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

        module ID = Primitives.Id
        module LT = Literals
        module VU = VariableUnit
        module NM = VU.Name
        module QT = VU.Quantity
        module CT = VU.Count
        module CN = VU.Concentration
        module TL = VU.Total
        module RT = VU.Rate
        module DS = VU.Dose
        module DA = VU.DoseAdjust
        module IT = Item

        /// Models in a `Component` in and `Orderable`
        type Component = 
            {
                /// The id of a `Component`
                OrderId: ID.Id
                /// The name of a `Component`
                Name: NM.Name
                /// The quantity of a `Component`
                ComponentQuantity: QT.Quantity
                /// The quantity of a `Component` in an `Orderable`
                OrderableQuantity: QT.Quantity
                /// The count of a `Component` in an `Orderable`
                OrderableCount: CT.Count
                /// The quantity of a `Component` in an `Order`
                OrderQuantity: QT.Quantity
                /// The count of a `Component` in an `Order`
                OrderCount: CT.Count
                /// The concentration of a `Component` in an `Orderable`
                OrderableConcentration: CN.Concentration
                // The `Component` `Dose`,  
                /// i.e. quanity, total and rate of `Component` administered
                Dose: DS.Dose
                // The `Component` `DoseAdjust`,  
                /// i.e. adjusted quanity, total and rate of `Component` administered
                DoseAdjust: DA.DoseAdjust
                /// The `Item`s in a `Component`
                Items: IT.Item list
            }
        
        /// Create a component with
        ///
        /// * `cmp_qty`: quantity of component
        /// * `orb_qty`: quantity of component in orderable
        /// * `orb_cnt`: count of component in orderable
        /// * `ord_qty`: quantity of component in order
        /// * `ord_cnt`: count of component in order
        /// * `orb_cnc`: concentration of component in orderble
        /// * `dos`: component dose
        /// * `dos_adj`: adjusted dose of component
        /// * `ii`: list of `Item`s in a component
        let create id nm cmp_qty orb_qty orb_cnt ord_qty ord_cnt orb_cnc dos dos_adj ii = 
            { 
                OrderId = id
                Name = nm
                ComponentQuantity = cmp_qty
                OrderableQuantity = orb_qty
                OrderableCount = orb_cnt
                OrderQuantity = ord_qty
                OrderCount = ord_cnt
                OrderableConcentration = orb_cnc
                Dose = dos
                DoseAdjust = dos_adj 
                Items = ii
            }

        /// Create a new component with
        ///
        /// * `sl`: list of item name and unit name tuples
        /// * `u1`: unit name of component
        /// * `adj`: adjust unit to adjust the dose
        let createNew id nm sl u adj =
            let s = [id |> ID.toString; nm |> NM.toString; LT.``component``]
            let cmp_qty = let s = [LT.``component``] |> List.append s in QT.quantity s u
            let orb_qty = let s = [LT.orderable]     |> List.append s in QT.quantity s u
            let orb_cnt = let s = [LT.orderable]     |> List.append s in CT.count s
            let ord_qty = let s = [LT.order]         |> List.append s in QT.quantity s u
            let ord_cnt = let s = [LT.order]         |> List.append s in CT.count s
            let orb_cnc = let s = [LT.orderable]     |> List.append s in CN.conc s u u
            let dos = let s = [LT.dose]              |> List.append s in DS.dose s u
            let dos_adj = let s = [LT.doseAdjust]    |> List.append s in DA.doseAdjust s u adj

            let ii = sl |> List.map (fun s -> IT.createNew id s u adj)

            create id nm cmp_qty orb_qty orb_cnt ord_qty ord_cnt orb_cnc dos dos_adj ii

        /// Apply **f** to a `Component` **comp** 
        let apply f (comp: Component) = comp |> f

        /// Utility to facilitate type inference
        let get = apply id

        /// Get the id of a `Component`
        let getId cmp = 
            ((cmp |> get).OrderId |> ID.toString) + "." +
            (cmp.Name |> NM.toString)

        /// Get the name of a `Component`
        let getName cmp = (cmp |> get).Name 

        /// Get the `Item`s in an `Component`
        let getItems cmp = (cmp |> get).Items

        /// Map a `Component` **cmp**
        /// to `VariableUnit`s
        let toVar cmp =
            let cmp_cmp_qty = (cmp |> get).ComponentQuantity |> QT.toVarUnt
            let cmp_orb_qty = cmp.OrderableQuantity          |> QT.toVarUnt
            let cmp_orb_cnt = cmp.OrderableCount             |> CT.toVarUnt
            let cmp_orb_cnc = cmp.OrderableConcentration     |> CN.toVarUnt

            let cmp_dos_qty,     cmp_dos_tot,     cmp_dos_rte     = cmp.Dose       |> DS.toVarUnt
            let cmp_dos_qty_adj, cmp_dos_tot_adj, cmp_dos_rte_adj = cmp.DoseAdjust |> DA.toVarUnt

            (
                cmp_cmp_qty, 
                cmp_orb_qty,
                cmp_orb_cnt,
                cmp_orb_cnc,
                cmp_dos_qty,
                cmp_dos_tot,
                cmp_dos_rte,
                cmp_dos_qty_adj,
                cmp_dos_tot_adj,
                cmp_dos_rte_adj
            )

        /// Create a string list from a 
        /// component where each string is
        /// a variable name with the valuerange
        /// and the unitgroup
        let toString cmp = 
            let cmp_cmp_qty, 
                cmp_orb_qty,
                cmp_orb_cnt,
                cmp_orb_cnc,
                cmp_dos_qty,
                cmp_dos_tot,
                cmp_dos_rte,
                cmp_dos_qty_adj,
                cmp_dos_tot_adj,
                cmp_dos_rte_adj = cmp |> toVar

            let ii = cmp.Items

            [
                cmp_cmp_qty 
                cmp_orb_qty
                cmp_orb_cnt
                cmp_orb_cnc
                cmp_dos_qty
                cmp_dos_tot
                cmp_dos_rte
                cmp_dos_qty_adj
                cmp_dos_tot_adj
                cmp_dos_rte_adj
            ]
            |> List.map VU.toString
            |> List.append (ii |> List.collect IT.toString )


        /// The following variables are used
        ///
        /// * orb\_orb\_qty: the orderable quantity
        /// * cmp\_cmp\_qty: the component quantity
        /// * cmp\_orb\_qty: the quantity of component in an orderable
        /// * cmp\_orb\_cnt: the count of component in an orderable
        /// * cmp\_ord\_qty: the quantity of component in an order
        /// * cmp\_ord\_cnt: the count of component in an order
        /// * cmp\_orb\_cnc: the concentration of an component in an orderable
        /// * cmp\_dos\_qty: the component dose quantity
        /// * cmp\_dos\_tot: the component dose total
        /// * cmp\_dos\_rte: the component dose rate
        /// * cmp\_dos\_qty\_adj: the adjusted component dose quantity
        /// * cmp\_dos\_tot\_adj: the adjusted component dose total
        /// * cmp\_dos\_rte\_adj: the adjusted component dose rate
        /// * frq: the prescription frequency
        /// * tme: the prescription time
        /// * qty: the orderable dose quantity
        /// * tot: the orderable dose total
        /// * rte: the orderable dose rate
        ///
        /// The following equations are generated:
        ///
        /// *Process*
        ///
        /// * cmp\_orb\_qty = cmp\_orb\_cnc \* orb\_orb\_qty
        /// * orb\_orb\_qty = cmp\_cmp\_qty \* cmp\_orb\_cnt
        /// * orb\_ord\_qty = cmp\_cmp\_qty \* cmp\_ord\_cnt
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
            let cmp_cmp_qty, 
                cmp_orb_qty,
                cmp_orb_cnt,
                cmp_orb_cnc,
                cmp_dos_qty,
                cmp_dos_tot,
                cmp_dos_rte,
                cmp_dos_qty_adj,
                cmp_dos_tot_adj,
                cmp_dos_rte_adj = cmp |> toVar

            let map = IT.toEqs adj frq qty tot tme rte cmp_cmp_qty cmp_orb_qty orb_orb_qty
            let ii = cmp.Items

            let eqs =
                [
                    [ cmp_orb_qty; cmp_orb_cnc; orb_orb_qty ]
                    [ orb_orb_qty; cmp_cmp_qty; cmp_orb_cnt ]
//                    [ orb_ord_qty; cmp_cmp_qty; cmp_ord_cnt ]
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
            |> List.append (ii |> List.collect map)

        /// Create a `Component` from a list
        /// of variable list eqs 
        let fromEqs eqs (cmp: Component) =
            let items = 
                cmp.Items
                |> List.map (IT.fromEqs eqs)
            {
                cmp with
                    ComponentQuantity = QT.fromVar eqs cmp.ComponentQuantity
                    OrderableQuantity = QT.fromVar eqs cmp.OrderableQuantity             
                    OrderableConcentration = CN.fromVar eqs cmp.OrderableConcentration
                    Dose = DS.fromVar eqs cmp.Dose
                    DoseAdjust = DA.fromVar eqs cmp.DoseAdjust
                    Items = items
            }    


    module ID = Primitives.Id
    module NM = Informedica.GenSolver.Lib.Variable.Name
    module LT = Literals
    module VU = VariableUnit
    module QT = VU.Quantity
    module CT = VU.Count
    module CN = VU.Concentration
    module TL = VU.Total
    module RT = VU.Rate
    module DS = VU.Dose
    module DA = VU.DoseAdjust
    module CM = Component

    /// Models an `Orderable` 
    type Orderable = 
        {
            /// The order id of 
            OrderId: ID.Id
            /// The name of the orderable
            Name: NM.Name
            /// The quantity of an orderable
            OrderableQuantity: QT.Quantity
            /// The quantity of an orderable in an order
            OrderQuantity: QT.Quantity
            /// The orderable count in an order
            OrderCount: CT.Count
            /// The dose of an orderable
            Dose: DS.Dose
            /// The adjusted dose of an orderable
            DoseAdjust: DA.DoseAdjust
            /// The list of components in an orderable
            Components: CM.Component list
        }
        
    /// Create an `Orderable` with
    ///
    /// * id: the order id
    /// * nm: the name of the orderable
    /// * orb\_qty: quantity of the orderable
    /// * ord\_qty: quantity of orderable in the order
    /// * orb\_cnt: the count of orderable in the order 
    /// * dos: the orderable dose
    /// * dos\_adj: the adjusted orderable dose
    let create id nm orb_qty ord_qty orb_cnt dos dos_ajd cc = 
        { 
            OrderId = id
            Name = nm
            OrderableQuantity = orb_qty
            OrderQuantity = ord_qty
            OrderCount = orb_cnt
            Dose = dos
            DoseAdjust = dos_ajd 
            Components = cc
        }

    /// Create a new `Orderable` with a `Component` list
    /// `cl`, an `Orderable`unit `u` and adjust unit groep `adj`
    let createNew id nm cl u adj =
        let s =  [id |> ID.toString; nm |> NM.toString; LT.orderable]
        let orb_qty = let s = [LT.orderable]  |> List.append s in QT.quantity s u
        let ord_qty = let s = [LT.order]      |> List.append s in QT.quantity s u
        let orb_cnt = let s = [LT.order]      |> List.append s in CT.count s
        let dos     = let s = [LT.dose]       |> List.append s in DS.dose s u
        let dos_ajd = let s = [LT.doseAdjust] |> List.append s in DA.doseAdjust s u adj

        let cc = cl |> List.map (fun (c, sl) -> CM.createNew id c sl u adj)

        create id nm orb_qty ord_qty orb_cnt dos dos_ajd cc

    /// Apply **f** to `Orderable` `ord`
    let apply f (orb: Orderable) = orb |> f

    /// Utility function to facilitate type inference
    let get = apply id

    /// Get the name of the `Orderable` which is 
    /// the sum of the concatenated lists of `Item`s
    let getName orb = (orb |> get).Name

    /// Get the `Component`s in an `Orderable`
    let getComponents orb = (orb |> get).Components

    /// Get the `Orderable` dose
    let getDose orb = (orb |> get).Dose

    // Get the `Orderable` adjusted dose
    let getDoseAdjust orb = (orb |> get).DoseAdjust

    // Get the base `UnitGroup` of an `Orderable`
    let getUnitGroup orb = 
        (orb |> get).OrderableQuantity
        |> QT.toVarUnt
        |> VU.getUnitGroup

    /// Map an `Orderable` **orb** to
    /// `VariableUnit`s
    let toVarUnt orb =
        let ord_qty = (orb |> get).OrderQuantity |> QT.toVarUnt
        let orb_qty = orb.OrderableQuantity      |> QT.toVarUnt
        let ord_cnt = orb.OrderCount             |> CT.toVarUnt

        let dos_qty,     dos_tot,     dos_rte     = orb.Dose       |> DS.toVarUnt
        let dos_qty_adj, dos_tot_adj, dos_rte_adj = orb.DoseAdjust |> DA.toVarUnt

        (
            ord_qty,
            orb_qty,
            ord_cnt,
            dos_qty,
            dos_tot,
            dos_rte,
            dos_qty_adj,
            dos_tot_adj,
            dos_rte_adj
        )


    /// Turn an `Orderable` `ord` into
    /// a list of strings.
    let toString orb = 
        let ord_qty,
            orb_qty,
            ord_cnt,
            dos_qty,
            dos_tot,
            dos_rte,
            dos_qty_adj,
            dos_tot_adj,
            dos_rte_adj = orb |> toVarUnt

        let cc = orb.Components

        [
            ord_qty
            orb_qty
            ord_cnt
            dos_qty
            dos_tot
            dos_rte
            dos_qty_adj
            dos_tot_adj
            dos_rte_adj
        ]
        |> List.map VU.toString
        |> List.append (cc |> List.collect CM.toString )

    /// The following variables are used:
    ///
    /// * ord\_qty: the quantity of orderable in an order
    /// * orb\_qty: the quantity of orderable
    /// * ord\_cnt: the count of orderable in an order
    /// * dos\_qty: the dose quantity of orderable
    /// * dos\_tot: the dose total of orderable
    /// * dos\_rte: the dose rate of orderable
    /// * dos\_qty_adj: the adjustedn dose quantity
    /// * dos\_tot_adj: the adjusted dose total
    /// * dos\_rte_adj: the adjusted dose rate
    /// * frq: frequency
    /// * tme: time
    /// 
    /// The following equations are generated:
    ///
    /// *Process or Continuous*
    ///
    /// * ord\_qty = ord\_cnt \* orb\_qty
    ///
    /// *Discontinuous Timed*
    ///
    /// * dot\_tot = dos\_qty \* frq
    /// * dos\_qty = dos\_rte \* tme
    /// * dot\_tot\_adj = dos\_qty\_adj \* frq
    /// * dos\_qty\_adj = dos\_rte\_adj \* tme
    /// 
    /// *Discontinuous*
    ///
    /// * dot\_tot = dos\_qty \* frq
    /// * dot\_tot\_adj = dos\_qty\_adj \* frq
    let toEqs hasRte adj frq tme orb =
        let ord_qty,
            orb_qty,
            ord_cnt,
            dos_qty,
            dos_tot,
            dos_rte,
            dos_qty_adj,
            dos_tot_adj,
            dos_rte_adj = orb |> toVarUnt

        let rte = if hasRte then dos_rte |> Some else None

        let qty_adj = dos_qty_adj
        let tot_adj = dos_tot_adj
        let rte_adj = dos_rte_adj

        let map = CM.toEqs adj frq dos_qty dos_tot tme rte orb_qty
        let cc = orb.Components

        let eqs = 
            [
                [ ord_qty; ord_cnt; orb_qty ]
            ] 
            

        match rte, frq, tme with
        // Discontinuous timed
        | Some rte, Some frq, Some tme ->
            [
                [dos_tot;     dos_qty;     frq]
                [dos_qty;     dos_rte;     tme]
                [dos_tot_adj; dos_qty_adj; frq]
                [dos_qty_adj; dos_rte_adj; tme]
            ] |> List.append eqs
        // Discontinuous
        | None, Some frq, None   ->
            [
                [dos_tot;     dos_qty;     frq]
                [dos_tot_adj; dos_qty_adj; frq]
            ] |> List.append eqs
        // Continuous or Process
        | _ -> eqs
        |> List.append (cc |> List.collect map)
        , orb_qty::(cc |> List.map (fun c -> c.OrderableQuantity |> QT.toVarUnt))


    let fromEqs eqs (ord: Orderable) =
        let cmps = 
            ord.Components
            |> List.map (CM.fromEqs eqs)
        {
            ord with
                OrderableQuantity = QT.fromVar eqs ord.OrderableQuantity             
                OrderQuantity = QT.fromVar eqs ord.OrderQuantity
                Dose = DS.fromVar eqs ord.Dose
                DoseAdjust = DA.fromVar eqs ord.DoseAdjust
                Components = cmps
        }

