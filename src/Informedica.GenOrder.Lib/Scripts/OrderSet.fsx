#load "load-project-release.fsx"

#time
    
open Informedica.GenOrder.Lib

/// Types and functions that model a 
/// set of `Order`s. 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OrderSet =

    open Informedica.GenUnits.Lib
    
    module Dose = VariableUnit.Dose
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

    let containsOrderableTotal orb ors =
        let ungs =
            ors
            |> getOrderableTotals
            |> List.map (fun (d, _) -> d |> Dose.toVarUnt)
            |> List.map (fun (d, _, _) -> d.UnitGroup |> UnitGroup.getAll |> fst)

        let ung = orb |> Orderable.getUnitGroup |> UnitGroup.getAll |> fst
        ungs |> List.exists ((=) ung)
        
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


    /// Add an `Order` to an `OrderSet`
    /// Note: need to check duplicate id check
    /// and calculate the totals
    let add ord ors = 
        { ors with Orders = ord::(ors |> getOrders) }
        |> addOrderableTotals ord.Orderable
        |> addItemTotals ord.Orderable


module Name = VariableUnit.Name
module Id = Primitives.Id
module Map = Order.Mapping

let lab id frq unt itms =
    let getItmNm itm =
        [id; itm |> Name.toString] |> Name.create |> Name.toString

    let items =
        itms |> List.map (fun i -> ([i] |> Name.create, "Count"))

    let ord =
        Order.createNew 
            (id |> Id.create)
            (["lab"] |> Name.create)
            ([["lab"] |> Name.create, items])
            "Count" 
            "Weight" 
            Prescription.discontinuous
            "None"
    ord

let test1 =
    lab "1" 3N "x/day" ["gluc"; "Na"]  

let test2 =
    lab "2" 3N "x/day" ["gluc"; "Na"]  


let ors = 
    OrderSet.empty
    |> OrderSet.add test1
    |> OrderSet.add test2

ors |> OrderSet.getOrderableTotals
ors |> OrderSet.getItemTotals
ors |> OrderSet.getOrders
