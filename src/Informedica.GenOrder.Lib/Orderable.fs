namespace Informedica.GenOrder.Lib

module Orderable =


    module Literals =

        [<Literal>]
        let componentQuantity = "ComponentQuantity"
        [<Literal>]
        let orderableQuantity = "OrderableQuanity"
        [<Literal>]
        let componentConcentration = "ComponnentConcentration"
        [<Literal>]
        let orderableConcentration = "OrderableConcentration"
        [<Literal>]
        let dose = "Dose"
        [<Literal>]
        let doseAdjust = "DoseAdjust"

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

        [<Literal>]
        let name = "Item"

        type Name = Informedica.GenSolver.Lib.Variable.Name.Name

        type Item = 
            {
                Name: Name
                ComponentQuantity: QT.Quantity 
                OrderableQuantity: QT.Quantity
                ComponentConcentration: CN.Concentration 
                OrderableConcentration: CN.Concentration 
                Dose: DS.Dose
                DoseAdjust: DA.DoseAdjust
            }
        
        let createName s = s |> NM.create

        let create n cq oq cc oc ds da = 
            { 
                Name = n
                ComponentQuantity = cq
                OrderableQuantity = oq
                ComponentConcentration = cc
                OrderableConcentration = oc
                Dose = ds
                DoseAdjust = da 
            }

        let createNew s u1 u2 adj =
            let s = [name] |> List.append [s]
            let n = s |> createName
            let cq = let s = [LT.componentQuantity] |> List.append s in QT.quantity s u1
            let oq = let s = [LT.orderableQuantity] |> List.append s in QT.quantity s u1
            let cc = let s = [LT.componentConcentration] |> List.append s in CN.conc s u1 u2
            let oc = let s = [LT.orderableConcentration] |> List.append s in CN.conc s u1 u2
            let ds = let s = [LT.dose] |> List.append s in DS.dose s u1
            let da = let s = [LT.doseAdjust] |> List.append s in DA.doseAdjust s u1 adj

            create n cq oq cc oc ds da

        let apply f (item: Item) = item |> f

        let get = apply id

    module Component =

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
                Name: string
                ComponentQuantity: QT.Quantity
                OrderableQuantity: QT.Quantity
                OrderableConcentration: CN.Concentration
                Dose: DS.Dose
                DoseAdjust: DA.DoseAdjust
                Items: IT.Item * IT.Item list
            }
        

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
            Name: string
            OrderableQuantity: QT.Quantity
            OrderQuantity: QT.Quantity
            Dose: DS.Dose
            DoseAdjust: DA.DoseAdjust
            Components: CM.Component * CM.Component list
        }
        
