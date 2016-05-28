#load "load-project-release.fsx"

#time
    
open Informedica.GenUtils.Lib
open Informedica.GenOrder.Lib



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

let pcm =
    Order.createNew 
        ("2" |> Id.create)
        (["paracetamol"] |> Name.create)
        ([["paracetamol"] |> Name.create, [
            ["paracetamol"] |> Name.create, "Mass"
        ]])
        "Tablet"
        "Weight"
        Prescription.discontinuous
        "oral"

let pcm2 =
    Order.createNew 
        ("3" |> Id.create)
        (["paracetamol"] |> Name.create)
        ([["paracetamol"] |> Name.create, [
            ["paracetamol"] |> Name.create, "Mass"
        ]])
        "Tablet"
        "Weight"
        Prescription.discontinuous
        "oral"


let ors = 
    OrderSet.empty
    |> OrderSet.add test1
    |> OrderSet.add pcm
    |> OrderSet.add pcm2


let print ors =
    let (prod, sum) = ors |> OrderSet.toEqs 
    printfn "Total Equations: %A" (prod @ sum |> List.length)
    for n in ((prod @ sum) |> List.collect id) do
        printfn "%A" (n |> VariableUnit.getName)

let ors' =
    ors
    |> OrderSet.solve "1.lab.Freq" Solver.Vals [3N] "x/day"
    |> OrderSet.solve "1.paracetamol.Freq" Solver.Vals [3N] "x/day"
    |> OrderSet.solve "1.paracetamol.Item.Dose.Qty" Solver.Vals [120N] "mg"


print ors

let toEqs ors =
    let p, s = ors |> OrderSet.toEqs
    p @ s 

ors 
|> toEqs
|> List.map (List.map VariableUnit.getVar)
|> OrderSet.fromEqs ors
|> print

ors
|> OrderSet.solve "1.lab.Freq" Solver.Vals [3N] "x/day"
|> OrderSet.solve "2.paracetamol.Freq" Solver.Vals [3N] "x/day"
//|> OrderSet.solve "2.paracetamol.Item.Dose.Qty" Solver.Vals [120N] "mg"
|> OrderSet.solve "paracetamol.Total" Solver.MaxIncl [4N] "g/day"
|> print
