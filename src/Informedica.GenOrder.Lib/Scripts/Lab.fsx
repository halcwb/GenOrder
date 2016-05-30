#load "load-project-release.fsx"

#time
    
open Informedica.GenUtils.Lib
open Informedica.GenOrder.Lib


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
