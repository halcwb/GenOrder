namespace Informedica.GenOrder.Lib

/// Types and functions that model a 
/// set of `Order`s. 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OrderSet =

    open Informedica.GenUnits.Lib
    open Informedica.GenUtils.Lib.BCL

    module EQ = Informedica.GenSolver.Lib.Equation

    module Dose = VariableUnit.Dose
    module Name = VariableUnit.Name
    module DoseAdjust = VariableUnit.DoseAdjust

    /// Represents a set of `Order`s
    /// and can map to a calculation model
    /// to calculate totals
    type OrderSet = 
        {
            /// The `Order`s in the `OrderSet`
            Orders: Order.Order list
            OrderableTotals: (Dose.Dose * DoseAdjust.DoseAdjust) list
            ItemTotals: (Dose.Dose * DoseAdjust.DoseAdjust) list
        }

    /// Apply **f** to an `OrderSet` **ors**
    let apply f (ors: OrderSet) = ors |> f

    /// Utitility function to enable type inference
    let get = apply id

    /// Create an `OrderSet` 
    let create ords ord_tot itm_tot =
        {
            Orders = ords
            OrderableTotals = ord_tot
            ItemTotals = itm_tot
        }

    /// The empty `OrderSet`
    let empty = create [] [] []

    /// Get the `Order`s from an `OrderSet`
    let getOrders ors = (ors |> get).Orders

    /// Get the orderable totals from an `OrderSet`
    let getOrderableTotals ors = (ors |> get).OrderableTotals

    /// Get the item totals from an `OrderSet`
    let getItemTotals ors = (ors |> get).ItemTotals

    /// Check if `OrderSet` allready contains 
    /// a total for the `Orderable` **orb**
    let containsOrderableTotal orb ors =
        let ungs =
            ors
            |> getOrderableTotals
            |> List.map (fun (d, _) -> d |> Dose.toVarUnt)
            |> List.map (fun (d, _, _) -> d.UnitGroup |> UnitGroup.getAll |> fst)

        let ung = orb |> Orderable.getUnitGroup |> UnitGroup.getAll |> fst
        ungs |> List.exists ((=) ung)
        
    /// Check if `OrderSet` allready contains 
    /// a total for the `Item` **itme**
    let containsItemTotal itm ors =
        let nms =
            ors
            |> getItemTotals
            |> List.map (fun (d, _) -> d |> Dose.getName)

        let nm = itm |> Orderable.Item.getName |> VariableUnit.Name.toString
        nms |> List.exists ((=) nm)
        
    // Adds an `Orderable` total
    // Note have to add item totals
    let addOrderableTotals orb ors =
        if ors |> containsOrderableTotal orb then ors
        else
            let n = 
                [orb 
                |> Orderable.getUnitGroup 
                |> UnitGroup.getAll |> fst
                |> UnitGroup.nameToString]
                |> VariableUnit.Name.create
            let dos = orb.Dose |> Dose.setName n
            let dsa = orb.DoseAdjust |> DoseAdjust.setName n
            { ors with 
                OrderableTotals = (dos, dsa)::ors.OrderableTotals }


    // Adds an `Orderable` total
    // Note have to add item totals
    let addItemTotals orb ors =
        orb
        |> Orderable.getComponents
        |> List.collect Orderable.Component.getItems
        |> List.fold (fun ors itm -> 
            if ors |> containsItemTotal itm then ors
            else
                let n =   itm |> Orderable.Item.getName
                let dos = itm.Dose       |> Dose.setName n
                let dsa = itm.DoseAdjust |> DoseAdjust.setName n
                { ors with 
                    ItemTotals = (dos, dsa)::ors.ItemTotals }
        ) ors

    /// Get the `Orderable` `Dose` and `DoseAdjust`
    /// properties for a specific `UnitGroup` **ung**
    /// to calculate the totals for that `UnitGroup` in
    /// and `OrderSet`
    let getOrderableVarUnts ung ors =
        ors
        |> getOrders
        |> List.map Order.getOrderable
        |> List.filter (fun orb ->
            orb |> Orderable.getUnitGroup = ung 
        )
        |> List.map (fun orb ->
            (orb.Dose |> Dose.toVarUnt, orb.DoseAdjust |> DoseAdjust.toVarUnt)
        )

    /// Get the `Item` `Dose` and `DoseAdjust`
    /// properties for a specific `Item`  with
    /// name **itm** to calculate the totals 
    /// for that `Item` in an `OrderSet`
    let getItemVarUnts nm ors =
        ors
        |> getOrders
        |> List.map Order.getOrderable
        |> List.collect Orderable.getComponents
        |> List.collect Orderable.Component.getItems
        |> List.filter (fun itm -> 
            match nm |> Name.toString |> String.split "." with
            | n::_ -> itm.Name |> Name.toString = n
            | _ -> false
            )
        |> List.map (fun itm ->
            (itm.Dose |> Dose.toVarUnt, itm.DoseAdjust |> DoseAdjust.toVarUnt)
        )

    /// Add an `Order` to an `OrderSet`
    /// Note: need to check duplicate id check
    /// and calculate the totals
    let add ord ors = 
        { ors with Orders = ord::(ors |> getOrders) }
        |> addOrderableTotals ord.Orderable
        |> addItemTotals ord.Orderable

    /// Map an `OrderSet` to totals `VarUnit` lists
    /// to calculate the sum equations
    let toOrderableEqs ors =
        let eqs = 
            let tots = ors   |> getOrderableTotals
            tots
            |> List.map (fun (ds, da) -> 
                let qd, td, rd = ds |> Dose.toVarUnt
                let qa, ta, ra = da |> DoseAdjust.toVarUnt
                ([qd], [td], [rd]), ([qa], [ta], [ra])
            )

        eqs 
        |> List.map (fun tot -> 
                let ((dq, dt, dr), (ad, at, ar)) = tot
                match dq with
                | vru::_ -> 
                    let ung = vru |> VariableUnit.getUnitGroup
                    let xs =  ors |> getOrderableVarUnts ung
                    xs 
                    |> List.fold (fun acc tot' ->
                            let ((dq', dt', dr'), (ad', at', ar')) = tot'
                            let ((dq, dt, dr), (ad, at, ar)) = acc
                            ((dq @ [dq'], dt @ [dt'], dr @ [dr']), 
                             (ad @ [ad'], at @ [at'], ar @ [ar'])) 
                         ) tot
                | _ -> tot
            )
        |> List.collect (fun tot ->
                let ((dq, dt, dr), (ad, at, ar)) = tot
                [dq; dt; dr; ad; at; ar]
        )

    /// Map a list of `Variable` lists to
    /// an `OrderSet` **ors**
    let fromOrderableEqs eqs ors =
        ors 
        |> getOrderableTotals
        |> List.fold (fun acc tot ->
            let (ds, da) = tot
            let ds' = ds |> Dose.fromVar eqs 
            let da' = da |> DoseAdjust.fromVar eqs
            let tots = 
                ors 
                |> getOrderableTotals
                |> Informedica.GenSolver.Utils.List.replace (fun (ds, da) -> 
                    ds |> Dose.getName = (ds' |> Dose.getName) && 
                    da |> DoseAdjust.getName = (da' |> DoseAdjust.getName)) (ds', da')
            { ors with OrderableTotals = tots }
        ) ors


    /// Map an `OrderSet` to totals `VarUnit` lists
    /// to calculate the sum equations
    let toItemEqs ors =
        let eqs = 
            let tots = ors |> getItemTotals
            tots
            |> List.map (fun (ds, da) -> 
                let qd, td, rd = ds |> Dose.toVarUnt
                let qa, ta, ra = da |> DoseAdjust.toVarUnt
                ([qd], [td], [rd]), ([qa], [ta], [ra])
            )

        eqs 
        |> List.map (fun tot -> 
                let ((dq, dt, dr), (ad, at, ar)) = tot
                match dq with
                | vru::_ -> 
                    let nm = vru |> VariableUnit.getName
                    let xs = ors |> getItemVarUnts nm
                    xs 
                    |> List.fold (fun acc tot' ->
                            let ((dq', dt', dr'), (ad', at', ar')) = tot'
                            let ((dq, dt, dr), (ad, at, ar)) = acc
                            ((dq @ [dq'], dt @ [dt'], dr @ [dr']), 
                             (ad @ [ad'], at @ [at'], ar @ [ar'])) 
                         ) tot
                | _ -> tot
            )
        |> List.collect (fun tot ->
                let ((dq, dt, dr), (ad, at, ar)) = tot
                [dq; dt; dr; ad; at; ar]
        )

    /// Map a list of `Variable` lists to
    /// an `OrderSet` **ors**
    let fromItemEqs eqs ors =
        ors 
        |> getItemTotals
        |> List.fold (fun acc tot ->
            let (ds, da) = tot
            let ds' = ds |> Dose.fromVar eqs 
            let da' = da |> DoseAdjust.fromVar eqs
            let tots = 
                ors 
                |> getItemTotals
                |> Informedica.GenSolver.Utils.List.replace (fun (ds, da) -> 
                    ds |> Dose.getName = (ds' |> Dose.getName) && 
                    da |> DoseAdjust.getName = (da' |> DoseAdjust.getName)) (ds', da')
            { ors with ItemTotals = tots }
        ) ors

    /// Map an `OrderSet` **ors** to a list of
    /// `VariableUnit` list product and
    /// sum eqs
    let toEqs ors =
        let prod, sum = 
            ors
            |> getOrders
            |> List.map Order.toEqs
            |> List.fold (fun (p', s') (p, s) -> 
                p @ p', [s] @ s'
             ) ([],[])

        let sum =
            sum 
            |> List.append (ors |> toOrderableEqs)
            |> List.append (ors |> toItemEqs)

        prod, sum

    /// Map a list of `Variable` lists **eqs**
    /// to an `OrderSet` **ors**
    let fromEqs ors eqs =
        let ors =
            ors
            |> fromOrderableEqs eqs
            |> fromItemEqs eqs
        ors 
        |> getOrders
        |> List.fold (fun acc o ->
                { acc with
                    Orders =
                        let o = eqs |> Order.fromEqs o 
                        acc.Orders
                        |> Informedica.GenSolver.Utils.List.replace (fun o' ->
                                o'.Id = o.Id) o }
        ) ors

    /// Solve an `OrderSet` *ors* with
    /// 
    /// * n: the name of the variable to be set
    /// * m: the mapping for the field of the order
    /// * p: the property of the variable to be set
    /// * v: the values to be set
    let solve n p v u ors =
        let n = [n] |> Name.create

        let toEql (prod, sum) =
            prod 
            |> List.map Order.toProd
            |> List.append (sum |> List.map Order.toSum)

        let toVars eqs = eqs |> List.map(fun e -> 
            match e with 
            | EQ.ProductEquation(y, xs) 
            | EQ.SumEquation(y, xs) -> y::xs)

        let createUnit = Order.createUnit Some (fun _ -> None)

        let prod, sum = ors |> toEqs

        match (sum @ prod) |> createUnit n u with
        | Some cu ->
            (prod, sum) 
            |> toEql
            |> Solver.solve n p v cu
            |> toVars
            |> fromEqs ors
        | None -> ors


