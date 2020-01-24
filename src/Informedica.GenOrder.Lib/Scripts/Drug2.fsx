

#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Types.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Utils.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Logging.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Variable.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Equation.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Solver.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Constraint.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/Api.fs"
#load "../../../../GenSolver/src/Informedica.GenSolver.Lib/SolverLogging.fs"

#load "../Types.fs"
#load "../DateTime.fs"
#load "../List.fs"
#load "../Logging.fs"
#load "../WrappedString.fs"
#load "../ValueUnit.fs"
#load "../ValueRange.fs"
#load "../VariableUnit.fs"
#load "../Solver.fs"
#load "../Orderable.fs"
#load "../Prescription.fs"
#load "../Order.fs"
#load "../DrugOrder.fs"


#time

open System
open System.Diagnostics
open MathNet.Numerics

open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib

open Types

module Units = ValueUnit.Units
module DrugConstraint = DrugOrder.DrugConstraint
module Quantity = VariableUnit.Quantity

module SolverLogging = Informedica.GenSolver.Lib.SolverLogging

type Logger = Informedica.GenSolver.Lib.Types.Logging.Logger
type SolverMessage = Informedica.GenSolver.Lib.Types.Logging.Message
type OrderMessage = Informedica.GenOrder.Lib.Types.Logging.Message


type Agent<'Msg> = MailboxProcessor<'Msg>
type IMessage = Informedica.GenSolver.Lib.Types.Logging.IMessage
type Level = Informedica.GenSolver.Lib.Types.Logging.Level


let printOrderMsg = function
| Logging.SolverReplaceUnit (n, u) -> ""
| Logging.OrderSolved o -> ""
| Logging.OrderConstraintsSolved (o, cs) -> ""
| Logging.OrderScenario s -> ""
| Logging.OrderScenerioWithNameValue (o, n, br) -> ""


let printMsg (msg : IMessage) = 
    match msg with
    | :? SolverMessage as m -> 
        m 
        |> SolverLogging.printMsg
    | :? OrderMessage  as m ->
        match m with
        | Logging.OrderConstraintsSolved (o, cs) ->
            o
            |> Order.toString
            |> String.concat "\n"
            |> sprintf "=== Order constraints solved ===\n%s"
        | _ -> sprintf ""
    | _ -> 
        sprintf ""


type Message =  
    | Start
    | Received of  (Level * IMessage)
    | Report


type OrderLogger =
    {
        Start : unit -> unit
        Logger: Logger
        Report: unit -> unit
    }


let logger  =

    let loggerAgent : Agent<Message> = 
        Agent.Start <| fun inbox ->
            let msgs = ResizeArray<(float * Level * IMessage)>()

            let rec loop (timer : Stopwatch) msgs =
                async {
                    let! msg = inbox.Receive ()

                    match msg with
                    | Start -> 
                        let timer = Stopwatch.StartNew()
                        return! 
                            ResizeArray<(float * Level * IMessage)>()
                            |> loop timer

                    | Received (l, m) -> 
                        msgs.Add(timer.Elapsed.TotalSeconds, l, m)
                        return! loop timer msgs

                    | Report ->
                        printfn "=== Start Report ===\n"
                        msgs 
                        |> Seq.length
                        |> printfn "Total messages received: %i"
                        
                        msgs 
                        |> Seq.iteri (fun i (t, l, m) ->
                            m
                            |> printMsg
                            |> function 
                            | s when s |> String.IsNullOrEmpty -> ()
                            | s -> printfn "%i. %f: %A\n%s" i t l s
                        )
                        printfn "\n"
                        
                        let timer = Stopwatch.StartNew()
                        return! 
                            ResizeArray<(float * Level * IMessage)>()
                            |> loop timer 
                }
            
            let timer = Stopwatch.StartNew()
            loop timer msgs
        
    {
        Start = 
            fun () ->
                Start
                |> loggerAgent.Post
        Logger = {
            Log =
                fun level msg ->
                    (level, msg)
                    |> Received
                    |> loggerAgent.Post 
        } 
        Report = 
            fun _ ->
                Report
                |> loggerAgent.Post        
    }


let printScenarios v n (sc : Order list) =
    let w =
        match sc with 
        | h::_ -> 
            h.Adjust
            |> Quantity.toValueUnitStringList None
            |> Seq.map snd
            |> Seq.head
        | _ -> ""

    printfn "\n\n=== SCENARIOS for Weight: %s ===" w
    sc
    |> List.iteri (fun i o ->
        o
        |> Order.printPrescription n
        |> fun (p, a, d) ->
            printfn "%i\tprescription:\t%s" (i + 1) p
            printfn "  \tdispensing:\t\t%s" a
            printfn "  \tpreparation:\t%s" d
        
        if v then
            o
            |> Order.toString
            |> List.iteri (fun i s -> printfn "%i\t%s" (i + 1) s)

            printfn "\n"
    )


logger.Start ()

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
        OrderType = DiscontinuousOrder
}
|> DrugOrder.create
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "paracetamol"
            Frequencies = [ 2N..4N ]
            SubstanceName = "paracetamol"
            MaxDoseQuantity = Some 1000N
            MaxDoseTotal = Some 4000N
            MinDoseTotalAdjust = Some 40N
            MaxDoseTotalAdjust = Some 90N
    }
|> DrugOrder.setAdjust "paracetamol" 20N
|> DrugOrder.evaluate logger.Logger
|> printScenarios false ["paracetamol"]

logger.Report ()


logger.Start ()

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
        OrderType = DiscontinuousOrder
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
|> DrugOrder.evaluate logger.Logger
//|> Order.calcScenarios2
|> printScenarios false ["sulfamethoxazol"; "trimethoprim"]


logger.Start ()

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
        OrderType = DiscontinuousOrder
}
|> DrugOrder.create
|> DrugOrder.setAdjust "paracetamol" 8N
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "paracetamol"
            Frequencies = [ 2N ]
            SubstanceName = "paracetamol"
            MaxDoseQuantity = Some 1000N
            MaxDoseTotal = Some 4000N
            MinDoseTotalAdjust = Some 40N
            MaxDoseTotalAdjust = Some 90N
    }
|> DrugOrder.evaluate logger.Logger
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
        OrderType = DiscontinuousOrder
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
|> DrugOrder.evaluate logger.Logger
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
        OrderType = ContinuousOrder
}
|> DrugOrder.create
|> DrugOrder.setAdjust "dopamin infusion" 3N
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "dopamin infusion"
            SubstanceName = "dopamin"
            MinDoseRateAdjust = Some 2N
            MaxDoseRateAdjust = Some 20N
    }
|> DrugOrder.evaluate logger.Logger
//|> Order.calcScenarios2
|> printScenarios false ["dopamin"]




// Dopamin infusion calculate scenario's 
// with a a fixed infusion - dose rate
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "dopamin infusion"
        Quantities = [ 50N ]
        Divisible = 1N
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
        OrderType = ContinuousOrder
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
|> DrugOrder.evaluate logger.Logger
|> printScenarios false ["dopamin"]

let noLogger : Logger = { Log = fun _ -> ignore }

logger.Start ()
// gentamicin
{
    DrugOrder.drugOrder with
        Id = "1"
        Name = "gentamicin"
        Quantities = [ ]
        Divisible = 1N 
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
        OrderType = TimedOrder
    }
|> DrugOrder.create
|> DrugOrder.setAdjust "gentamicin" (4N)
|> DrugOrder.setDoseLimits
    {   DrugOrder.doseLimits with
            Name = "gentamicin"
            SubstanceName = "gentamicin"
            Frequencies = [ 1N ]
            MinDoseTotalAdjust = Some (4N)
            MaxDoseTotalAdjust = Some (6N)
    }
|> DrugOrder.setSolutionLimits 
    {
        DrugOrder.solutionLimits with
            Name = "gentamicin"
            Component = "gentamicin"
//            MinConcentration = Some (1N)
            MaxConcentration = Some (2N)
            DoseCount = Some (1N)
//            MinTime = (Some (1N/2N))
            MaxTime = (Some (1N/2N))

    }
|> DrugOrder.evaluate logger.Logger
|> printScenarios false ["gentamicin"]

logger.Report ()

