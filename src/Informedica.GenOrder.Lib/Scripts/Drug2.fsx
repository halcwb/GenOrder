
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Utils.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Logger.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Variable.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Equation.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Solver.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Constraint.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Api.fs"

#load "../DateTime.fs"
#load "../WrappedString.fs"
#load "../List.fs"
#load "../Logger.fs"
#load "../ValueUnit.fs"
#load "../ValueRange.fs"
#load "../VariableUnit.fs"
#load "../Solver.fs"
#load "../Orderable.fs"
#load "../Prescription.fs"
#load "../Order.fs"
#load "../DrugOrder.fs"

#time

open MathNet.Numerics

open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

module Units = ValueUnit.Units
module DrugConstraint = DrugOrder.DrugConstraint


let printScenarios v n sc =
    printfn "\n\n=== SCENARIOS ==="
    sc
    |> List.iteri (fun i o ->
        o
        |> Order.printPrescription n
        |> fun (p, a, d) ->
            printfn "%i\tvoorschrift:\t%s" (i + 1) p
            printfn "  \ttoediening:\t\t%s" a
            printfn "  \tbereiding:\t\t%s" d
        
        if v then
            o
            |> Order.toString
            |> List.iteri (fun i s -> printfn "%i\t%s" (i + 1) s)
    )



// Paracetamol supp
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "paracetamol"
        Products = 
            [
                { 
                    DrugOrder.productComponent with 
                        Name = "paracetamol"
                        Quantities = [ 1N ]
                        TimeUnit = "day"
                        Substances =
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "paracetamol"
                                        Concentrations = 
                                            [ 60N; 120N; 240N; 500N; 1000N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]
                }
            ]
        Unit = "piece"
        TimeUnit = "day"
        Shape = "supp"
        Route = "rect"
        OrderType = DrugOrder.OrderType.Discontinuous
}
|> DrugOrder.create
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "paracetamol"
            Frequencies = [ 2N .. 4N ]
            SubstanceName = "paracetamol"
            MaxDoseQuantity = Some 1000N
            MaxDoseTotal = Some 4000N
            MaxDoseTotalAdjust = Some 90N
    }
|> DrugOrder.setAdjust "paracetamol" 10N
|> DrugOrder.evaluate
|> printScenarios false ["paracetamol"]
//|> List.iter (fun o ->
//    o
//    |> Order.toString
//    |> List.iteri (fun i s -> printfn "%i\t%s" (i + 1) s)
//)



// Drug with multiple items
// cotrimoxazol for infection
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "cotrimoxazol"
        Products = 
            [
                { 
                    DrugOrder.productComponent with 
                        Name = "cotrimoxazol"
                        Quantities = [ 1N ]
                        TimeUnit = "day"
                        Substances =
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sulfamethoxazol"
                                        Concentrations = 
                                            [ 100N; 400N; 800N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "trimethoprim"
                                        Concentrations = 
                                            [ 20N; 80N; 160N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]
                }
            ]
        Unit = "piece"
        TimeUnit = "day"
        Shape = "tablet"
        Route = "or"
        OrderType = DrugOrder.OrderType.Discontinuous
}
|> DrugOrder.create
// setting dose limits for infection
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "cotrimoxazol"
            Frequencies = [ 2N ]
            SubstanceName = "sulfamethoxazol"
            MaxDoseTotal = Some 1600N
            MaxDoseTotalAdjust = Some 30N
    }
|> DrugOrder.setAdjust "cotrimoxazol" 10N
// is not be necessary when a single product is chosen
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "cotrimoxazol"
            Frequencies = [ 2N ]
            SubstanceName = "trimethoprim"
            MaxDoseTotal = Some 320N
            MaxDoseTotalAdjust = Some 6N
    }
|> DrugOrder.evaluate
//|> Order.calcScenarios2
|> printScenarios false ["sulfamethoxazol"; "trimethoprim"]



// Paracetamol drink
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "paracetamol"
        Products = 
            [
                { 
                    DrugOrder.productComponent with 
                        Name = "paracetamol"
                        Quantities = [ 100N ]
                        TimeUnit = "day"
                        Substances =
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "paracetamol"
                                        Concentrations = [ 24N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]
                }
            ]
        Unit = "ml"
        TimeUnit = "day"
        Shape = "drink"
        Route = "or"
        OrderType = DrugOrder.OrderType.Discontinuous
}
|> DrugOrder.create
|> DrugOrder.setAdjust "paracetamol" 20N
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "paracetamol"
            Frequencies = [ 2N .. 4N ]
            SubstanceName = "paracetamol"
            MaxDoseQuantity = Some 1000N
            MaxDoseTotal = Some 4000N
            MinDoseTotalAdjust = Some 60N
            MaxDoseTotalAdjust = Some 90N
    }
|> DrugOrder.evaluate
//|> List.length
|> printScenarios false ["paracetamol"]



// Drug with multiple items
// cotrimoxazol drink for infection
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "cotrimoxazol"
        Products = 
            [
                { 
                    DrugOrder.productComponent with 
                        Name = "cotrimoxazol"
                        Quantities = [ 1N ]
                        TimeUnit = "day"
                        Substances =
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sulfamethoxazol"
                                        Concentrations = 
                                            [ 40N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "trimethoprim"
                                        Concentrations = 
                                            [ 8N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]
                }
            ]
        Unit = "ml"
        TimeUnit = "day"
        Shape = "drink"
        Route = "or"
        OrderType = DrugOrder.OrderType.Discontinuous
}
|> DrugOrder.create
// setting dose limits for infection
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "cotrimoxazol"
            Frequencies = [ 2N ]
            SubstanceName = "sulfamethoxazol"
            MaxDoseTotal = Some 1600N
            MaxDoseTotalAdjust = Some 30N
    }
|> DrugOrder.setAdjust "cotrimoxazol" 10N
|> DrugOrder.evaluate
//|> List.length
|> printScenarios false ["sulfamethoxazol"; "trimethoprim"]



// Dopamin infusion calculate scenario's 
// with a number of standard solutions
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "dopamin infusion"
        Quantities = [ 50N ]
        Divisible = 2N
        Unit = "ml"
        TimeUnit = "day"
        Shape = "infusion fluid"
        Route = "iv"
        Products = 
            [
                { 
                    DrugOrder.productComponent with
                        Name = "dopamin"
                        Quantities = [ 5N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "dopamin"
                                        Concentrations = [ 40N ]
                                        OrderableQuantities = [ 80N; 200N; 400N ]
                                        Unit = "mg"
                                        DoseUnit = "mcg"
                                        TimeUnit = "min"
                                }
                            ]

                }
                { 
                    DrugOrder.productComponent with
                        Name = "saline"
                        Quantities = [ 5000N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sodium"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "chloride"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                            ]

                }
            ]
        OrderType = DrugOrder.OrderType.Continuous
}
|> DrugOrder.create
|> DrugOrder.setAdjust "dopamin infusion" 10N
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "dopamin infusion"
            SubstanceName = "dopamin"
            MinDoseRateAdjust = Some 2N
            MaxDoseRateAdjust = Some 20N
    }
|> DrugOrder.evaluate
//|> Order.calcScenarios2
|> printScenarios false ["dopamin"]




// Dopamin infusion calculate scenario's 
// with a a fixed infusion - dose rate
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "dopamin infusion"
        Quantities = [ 50N ]
        Divisible = 10N
        Unit = "ml"
        TimeUnit = "day"
        Shape = "infusion fluid"
        Route = "iv"
        Products = 
            [
                { 
                    DrugOrder.productComponent with
                        Name = "dopamin"
                        Quantities = [ 5N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "dopamin"
                                        Concentrations = [ 40N ]
                                        Unit = "mg"
                                        DoseUnit = "mcg"
                                        TimeUnit = "min"
                                }
                            ]

                }
                { 
                    DrugOrder.productComponent with
                        Name = "saline"
                        Quantities = [ 5000N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sodium"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "chloride"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                            ]

                }
            ]
        OrderType = DrugOrder.OrderType.Continuous
}
|> DrugOrder.create
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "dopamin infusion"
            Rates = [ 1N ]
            SubstanceName = "dopamin"
            MinDoseRateAdjust = Some 2N
            MaxDoseRateAdjust = Some 20N
    }
|> DrugOrder.setAdjust "dopamin infusion" 10N
|> DrugOrder.evaluate
|> printScenarios false ["dopamin"]



// gentamicin
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "gentamicin"
        Quantities = [ ]
        Divisible = 10N 
        Unit = "ml"
        TimeUnit = "day"
        Shape = "infusion fluid"
        Route = "iv"
        Products = 
            [
                { 
                    DrugOrder.productComponent with
                        Name = "gentamicin"
                        Quantities = [ 2N; 10N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "gentamicin"
                                        Concentrations = [ 10N; 40N ]
                                        Unit = "mg"
                                        DoseUnit = "mg"
                                        TimeUnit = "day"
                                }
                            ]

                }
                { 
                    DrugOrder.productComponent with
                        Name = "saline"
                        Quantities = [ 5000N ]
                        TimeUnit = "day"
                        Substances = 
                            [
                                {
                                    DrugOrder.substanceItem with
                                        Name = "sodium"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                                {
                                    DrugOrder.substanceItem with
                                        Name = "chloride"
                                        Concentrations = [ 155N / 1000N ]
                                        Unit = "mmol"
                                        DoseUnit = "mmol"
                                        TimeUnit = "day"
                                }
                            ]

                }

            ]
        OrderType = DrugOrder.OrderType.Timed
    }
|> DrugOrder.create
|> DrugOrder.setAdjust "gentamicin" (800N/1000N)
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "gentamicin"
            SubstanceName = "gentamicin"
            Frequencies = [ 1N ]
            MinDoseTotalAdjust = Some (5N * (9N/10N))
            MaxDoseTotalAdjust = Some (5N * (10N/9N))
    }
|> DrugOrder.setSolutionLimits 
    {
        DrugOrder.solutionLimits with
            Name = "gentamicin"
            Component = "gentamicin"
//            MinConcentration = Some (1N)
            MaxConcentration = Some (2N)
            DoseCount = Some (2N)
            MinTime = (Some (1N/2N))
            MaxTime = (Some 1N)

    }
|> DrugOrder.evaluate
|> printScenarios false ["gentamicin"]


