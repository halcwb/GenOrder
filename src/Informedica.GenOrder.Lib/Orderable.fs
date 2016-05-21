namespace Informedica.GenOrder.Lib

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Orderable =

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
    /// `Order` `Item`
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
                ComponentConcentration: CN.Concentration 
                OrderableConcentration: CN.Concentration 
                Dose: DS.Dose
                DoseAdjust: DA.DoseAdjust
            }
        
        let create cq oq cc oc ds da = 
            { 
                ComponentQuantity = cq
                OrderableQuantity = oq
                ComponentConcentration = cc
                OrderableConcentration = oc
                Dose = ds
                DoseAdjust = da 
            }

        let createNew s u1 u2 adj =
            let s = [LT.item] |> List.append [s]
            let cq = let s = [LT.``component``] |> List.append s in QT.quantity s u1
            let oq = let s = [LT.orderable] |> List.append s in QT.quantity s u1
            let cc = let s = [LT.``component``] |> List.append s in CN.conc s u1 u2
            let oc = let s = [LT.orderable] |> List.append s in CN.conc s u1 u2
            let ds = let s = [LT.dose] |> List.append s in DS.dose s u1
            let da = let s = [LT.doseAdjust] |> List.append s in DA.doseAdjust s u1 adj

            create cq oq cc oc ds da

        let apply f (item: Item) = item |> f

        let get = apply id

        let toString item = 
            let cq = (item |> get).ComponentQuantity
            let oq = item.OrderableQuantity
            let cc = item.ComponentConcentration
            let oc = item.OrderableConcentration
            let dq, dt, dr = item.Dose |> DS.toVar
            let aq, at, ar = item.DoseAdjust |> DA.toVar

            [cq |> QT.toVar; oq |> QT.toVar; cc |> CN.toVar; oc |> CN.toVar; dq; dt; dr; aq; at; ar ]
            |> List.map VU.toString

        /// The following equations are generated:
        ///
        /// * itm\_cmp\_qty = itm\_cmp\_cnc \* cmp\_cmp\_qty
        /// * itm\_ord\_qty = itm\_ord\_cnc \* ord\_ord\_qty
        /// * itm\_ord\_qty = itm\_cmp\_cnc \* cmp\_ord\_qty
        /// * itm\_dos\_tot = itm\_dos\_qty \* frq 
        /// * itm\_dos\_qty = itm\_dos\_rte \* tme
        /// * itm\_dos\_qty = itm\_ord\_cnc \* qty
        /// * itm\_dos\_tot = itm\_ord\_cnc \* tot
        /// * itm\_dos\_rte = itm\_ord\_cnc \* rte
        /// * itm\_dos\_qty = itm\_dos\_qty\_adj \* adj
        /// * itm\_dos\_tot = itm\_dos\_tot\_adj \* adj
        /// * itm\_dos\_rte = itm\_dos\_rte\_adj \* adj
        let toVarUns adj frq qty tot tme rte cmp_cmp_qty cmp_ord_qty ord_ord_qty item =
            let itm_cmp_qty = (item |> get).ComponentQuantity |> QT.toVar
            let itm_ord_qty = item.OrderableQuantity          |> QT.toVar
            let itm_cmp_cnc = item.ComponentConcentration     |> CN.toVar
            let itm_ord_cnc = item.OrderableConcentration     |> CN.toVar
            let itm_dos_qty, itm_dos_tot, itm_dos_rte = item.Dose |> DS.toVar
            let itm_dos_qty_adj, itm_dos_tot_adj, itm_dos_rte_adj = item.DoseAdjust |> DA.toVar

            [
                [ itm_cmp_qty; itm_cmp_cnc;     cmp_cmp_qty ]
                [ itm_ord_qty; itm_ord_cnc;     ord_ord_qty ]
                [ itm_ord_qty; itm_cmp_cnc;     cmp_ord_qty ]
                [ itm_dos_tot; itm_dos_qty;     frq ] 
                [ itm_dos_qty; itm_dos_rte;     tme ]
                [ itm_dos_qty; itm_ord_cnc;     qty ]
                [ itm_dos_tot; itm_ord_cnc;     tot ]
                [ itm_dos_rte; itm_ord_cnc;     rte ]
                [ itm_dos_qty; itm_dos_qty_adj; adj ]
                [ itm_dos_tot; itm_dos_tot_adj; adj ]
                [ itm_dos_rte; itm_dos_rte_adj; adj ]
            ]

        let fromVarUns eqs (item: Item) =
            {
                item with
                    ComponentQuantity = QT.fromVar eqs item.ComponentQuantity
                    OrderableQuantity = QT.fromVar eqs item.OrderableQuantity             
                    ComponentConcentration = CN.fromVar eqs item.ComponentConcentration
                    OrderableConcentration = CN.fromVar eqs item.OrderableConcentration
                    Dose = DS.fromVar eqs item.Dose
                    DoseAdjust = DA.fromVar eqs item.DoseAdjust
            }    

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

        type Component = 
            {
                ComponentQuantity: QT.Quantity
                OrderableQuantity: QT.Quantity
                OrderableConcentration: CN.Concentration
                Dose: DS.Dose
                DoseAdjust: DA.DoseAdjust
                Items: IT.Item * IT.Item list
            }
        
        let create cq oq cc oc ds da ii = 
            match ii with
            | h::tail ->
                { 
                    ComponentQuantity = cq
                    OrderableQuantity = oq
                    OrderableConcentration = oc
                    Dose = ds
                    DoseAdjust = da 
                    Items = h, tail
                }
            | _ -> failwith "Not a valid component"

        let createNew sl u1 u2 adj =
            let s = [LT.``component``] |> List.append [sl |> String.concat "/"] 
            let cq = let s = [LT.``component``] |> List.append s in QT.quantity s u2
            let oq = let s = [LT.orderable] |> List.append s in QT.quantity s u2
            let cc = let s = [LT.``component``] |> List.append s in CN.conc s u2 u2
            let oc = let s = [LT.orderable] |> List.append s in CN.conc s u2 u2
            let ds = let s = [LT.dose] |> List.append s in DS.dose s u2
            let da = let s = [LT.doseAdjust] |> List.append s in DA.doseAdjust s u2 adj

            let ii = sl |> List.map (fun s -> IT.createNew s u1 u2 adj)

            create cq oq cc oc ds da ii

        let apply f (comp: Component) = comp |> f

        let get = apply id

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
        /// * ord\_ord\_qty = cmp\_cmp\_qty \* cmp\_ord\_qty
        /// * cmp\_ord\_qty = cmp\_ord\_cnc \* ord\_ord\_qty
        /// * cmp\_dos\_tot = cmp\_dos\_qty \* frq 
        /// * cmp\_dos\_qty = cmp\_dos\_rte \* tme
        /// * cmp\_dos\_qty = cmp\_ord\_cnc \* qty
        /// * cmp\_dos\_tot = cmp\_ord\_cnc \* tot
        /// * cmp\_dos\_rte = cmp\_ord\_cnc \* rte
        /// * cmp\_dos\_qty = cmp\_dos\_qty\_adj \* adj
        /// * cmp\_dos\_tot = cmp\_dos\_tot\_adj \* adj
        /// * cmp\_dos\_rte = cmp\_dos\_rte\_adj \* adj
        let toVarUns adj frq qty tot tme rte ord_ord_qty cmp =
            let cmp_cmp_qty = (cmp |> get).ComponentQuantity |> QT.toVar
            let cmp_ord_qty = cmp.OrderableQuantity          |> QT.toVar
            let cmp_ord_cnc = cmp.OrderableConcentration     |> CN.toVar

            let cmp_dos_qty,     cmp_dos_tot,     cmp_dos_rte     = cmp.Dose       |> DS.toVar
            let cmp_dos_qty_adj, cmp_dos_tot_adj, cmp_dos_rte_adj = cmp.DoseAdjust |> DA.toVar

            let map = IT.toVarUns adj frq qty tot tme rte cmp_cmp_qty cmp_ord_qty ord_ord_qty
            let i, ii = cmp.Items

            [
                [ ord_ord_qty; cmp_cmp_qty;     cmp_ord_qty ]
                [ cmp_ord_qty; cmp_ord_cnc;     ord_ord_qty ]
                [ cmp_dos_tot; cmp_dos_qty;     frq  ]
                [ cmp_dos_qty; cmp_dos_rte;     tme ]
                [ cmp_dos_qty; cmp_ord_cnc;     qty ]
                [ cmp_dos_tot; cmp_ord_cnc;     tot ]
                [ cmp_dos_rte; cmp_ord_cnc;     rte ]
                [ cmp_dos_qty; cmp_dos_qty_adj; adj ]
                [ cmp_dos_tot; cmp_dos_tot_adj; adj ]
                [ cmp_dos_rte; cmp_dos_rte_adj; adj ]
            ] 
            |> List.append (i::ii |> List.collect map)

        let fromVarUns eqs (cmp: Component) =
            let items = 
                (cmp.Items |> fst)::(cmp.Items |> snd)
                |> List.map (IT.fromVarUns eqs)
            {
                cmp with
                    ComponentQuantity = QT.fromVar eqs cmp.ComponentQuantity
                    OrderableQuantity = QT.fromVar eqs cmp.OrderableQuantity             
                    OrderableConcentration = CN.fromVar eqs cmp.OrderableConcentration
                    Dose = DS.fromVar eqs cmp.Dose
                    DoseAdjust = DA.fromVar eqs cmp.DoseAdjust
                    Items = items.Head, items.Tail
            }    


    module LT = Literals
    module VU = VariableUnit
    module QT = VU.Quantity
    module CN = VU.Concentration
    module TL = VU.Total
    module RT = VU.Rate
    module DS = VU.Dose
    module DA = VU.DoseAdjust
    module CM = Component

    type Orderable = 
        {
            OrderableQuantity: QT.Quantity
            OrderQuantity: QT.Quantity
            Dose: DS.Dose
            DoseAdjust: DA.DoseAdjust
            Components: CM.Component * CM.Component list
        }
        
        
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

    let createNew cl u1 u2 adj =
        let s = [cl |> List.map (String.concat "/") |> String.concat " & " ] @ [LT.orderable]
        let oq = let s = [LT.orderable] |> List.append s in QT.quantity s u2
        let rq = let s = [LT.order] |> List.append s in QT.quantity s u2
        let ds = let s = [LT.dose] |> List.append s in DS.dose s u2
        let da = let s = [LT.doseAdjust] |> List.append s in DA.doseAdjust s u2 adj

        let cc = cl |> List.map (fun c -> CM.createNew c u1 u2 adj)

        create oq rq ds da cc

    let apply f (ord: Orderable) = ord |> f

    let get = apply id

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
    let toVarUns adj frq tme ord =
        let ord_der_qty = (ord |> get).OrderQuantity |> QT.toVar
        let ord_ord_qty = ord.OrderableQuantity      |> QT.toVar

        let ord_dos_qty,     ord_dos_tot,     ord_dos_rte     = ord.Dose       |> DS.toVar
        let ord_dos_qty_adj, ord_dos_tot_adj, ord_dos_rte_adj = ord.DoseAdjust |> DA.toVar

        let qty = ord_dos_qty
        let tot = ord_dos_tot
        let rte = ord_dos_rte

        let qty_adj = ord_dos_qty_adj
        let tot_adj = ord_dos_tot_adj
        let rte_adj = ord_dos_rte_adj

        let map = CM.toVarUns adj frq qty tot tme rte ord_ord_qty
        let c, cc = ord.Components

        [
            [ tot; qty; frq ]
            [ qty; rte; tme ]
            [ tot_adj; qty_adj; frq ]
            [ qty_adj; rte_adj; tme ]
            [ tot; ord_der_qty; ord_ord_qty ]
        ] 
        |> List.append (c::cc |> List.collect map)
        , ord_ord_qty::(c::cc |> List.map (fun c -> c.OrderableQuantity |> QT.toVar))


    let fromVarUns eqs (ord: Orderable) =
        let cmps = 
            (ord.Components |> fst)::(ord.Components |> snd)
            |> List.map (CM.fromVarUns eqs)
        {
            ord with
                OrderableQuantity = QT.fromVar eqs ord.OrderableQuantity             
                OrderQuantity = QT.fromVar eqs ord.OrderQuantity
                Dose = DS.fromVar eqs ord.Dose
                DoseAdjust = DA.fromVar eqs ord.DoseAdjust
                Components = cmps.Head, cmps.Tail
        }

