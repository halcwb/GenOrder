namespace Informedica.GenOrder.Lib

module Orderable =

    module Item =

        module VU = VariableUnit
        module QT = VU.Quantity
        module CN = VU.Concentration
        module TL = VU.Total
        module RT = VU.Rate
        module DS = VU.Dose
        module DA = VU.DoseAdjust

        type Item = 
            {
                Name: string
                ComponentQuantity: QT.Quantity 
                OrderableQuantity: QT.Quantity
                ComponentConcentration: CN.Concentration 
                OrderableConcentration: CN.Concentration 
                Dose: DS.Dose
                DoseAdjust: DA.DoseAdjust
            }
        

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
        
