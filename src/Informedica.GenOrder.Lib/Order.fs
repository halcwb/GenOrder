namespace Informedica.GenOrder.Lib

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Order =

    open System

    module StartStop =
        
        open System

        type StartStop =
            | Start of DateTime
            | StartStop of DateTime * DateTime

    module EQ = Informedica.GenSolver.Lib.Equation
    module OR = Orderable
    module PR = Prescription
    module DT = StartStop
    module SV = Solver
    module VU = VariableUnit
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
        let adj = adju |> QT.quantity ["Adjust"]
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

    let toEqs o =
        let frq, tme =
            match (o |> get).Prescription with
            | PR.Process    -> FR.frequency, TM.time
            | PR.Continuous -> FR.frequency, TM.time
            | PR.Discontinuous (frq) -> frq, TM.time
            | PR.Timed(frq, tme)     -> frq, tme
        
        let ord = o.Orderable
        let adj = o.Adjust |> QT.toVar
        let frq, tme = frq |> FR.toVar, tme |> TM.toVar
        
        let prod, sum = OR.toEqs adj frq tme ord
        prod 
        |> List.map toProd
        |> List.append [sum |> toSum]