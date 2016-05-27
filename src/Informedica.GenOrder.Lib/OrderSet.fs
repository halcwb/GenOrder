namespace Informedica.GenOrder.Lib

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OrderSet =
    
    module OD = Order

    type OrderSet = OrderSet of OD.Order Set

    let apply f (ors: OrderSet) = ors |> f

    let create set = set |> OrderSet

    let get = apply id

    let empy = Set.empty |> OrderSet

    let add ord (OrderSet set) = set.Add ord |> create

    let solve n m p v u (OrderSet ors) =
        ors
        |> Set.toList
        |> List.map (OD.toEqs)
