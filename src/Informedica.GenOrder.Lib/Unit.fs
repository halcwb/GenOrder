namespace Informedica.GenOrder.Lib

/// Helper functions to facilitate the use 
/// of `Informedica.GenUnits.Lib`
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Unit = 

    open Informedica.GenUtils.Lib.BCL
    
    module UN = Informedica.GenUnits.Lib.CombiUnit

    /// Create a frequency unit
    let freqUnit = "1 X/day" |> UN.fromString
    /// Create a time unit
    let timeUnit = "1 hour"  |> UN.fromString
    /// Create a quantity unit
    let qtyUnit s = s |> UN.fromString
    /// Create a total unit
    let totalUnit s = s + "/day"  |> UN.fromString
    /// Create a rate unit
    let rateUnit s  = s + "/hour" |> UN.fromString
    /// Create a unit `s1` per unit `s2`
    let perUnit s2 s1 = "1 " + s1 + "/1 " + s2 |> UN.fromString
    /// Create a unit `cu` adjusted by unit `s2`
    let adjUnit s2 cu =
        match cu |> UN.toString |> String.split "/" with
        | [u] -> u + "/1 " + s2 
        | [u1;u2] -> u1 + "/1 " + s2 + "/" + u2 
        | _ -> failwith "Not suported"
        |> UN.fromString


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module UnitGroup =

        open Informedica.GenUnits.Lib

        module UN = Unit
        module NM = UN.Name
        module CU = CombiUnit

        type UnitGroup = UnitGroup of NM.Name * (CU.Operator * NM.Name) list

        let create n = (n |> NM.Name, []) |> UnitGroup

        let nameToString (NM.Name n) = n

        let opToString = function
            | CU.Per -> "/"
            | CU.Times -> "*"

        let apply f (ug: UnitGroup) = ug |> f

        let get = apply id

        let getAll (UnitGroup(n, nl)) = n, nl 

        let addGroup o n ug = 
            let g, gl = ug |> getAll
            (g, [(o, n |> NM.Name)] |> List.append gl) |> UnitGroup


        let perGroup = addGroup CU.Per

        let timesGroup = addGroup CU.Times

        let fromUnit cu = 
            let _, u, ul = cu |> CU.get
            (u |> UN.getGroupName, ul |> List.map (fun (op, _, u) -> op, u |> UN.getGroupName))
            |> UnitGroup

        let toString ug =
            let n, nl = ug |> getAll
            (n |> nameToString)::(nl |> List.map (fun (op, n) -> (op |> opToString) + (n |> nameToString) ))
            |> String.concat ""

        let fromString s =
            let dels = "#"
            let mults = "*"
            let divs  = "/"
            let space = " "

            let ofs s =
                match s with
                | _ when s = mults -> CU.Times
                | _ when s = divs  -> CU.Per
                | _ -> failwith "Not a valid operator string"

            let rec parse ul usl =
                match usl with
                | [us] -> 
                    let u = us |> NM.Name
                    (u, ul) |> UnitGroup
                | us::os::rest -> 
                    let u = us |> NM.Name
                    let o = os |> ofs
                    rest |> parse ([ (o, u)] @ ul)
                | _ -> failwith "Cannot parse string list"

            s
            |> String.replace mults (dels + mults + dels)
            |> String.replace divs  (dels + divs + dels)
            |> String.split dels
            |> List.rev
            |> parse []


        let eqs ug u = u |> fromUnit = ug

        let getUnits ug =
            let n, nl = ug |> getAll

            let get n = 
                UN.Units.units
                |> List.find (fun us -> us.Head.Group = n)

            let us, usl = n |> get, nl |> List.map (fun (o, u) -> o, u |> get)
            
            let rec create usl cul =
                match usl with
                | [] -> cul
                | (o, ul)::tail ->
                    let f =  match o with | CU.Per -> CU.per 1N | CU.Times -> CU.times 1N
                    [
                        for cu in cul do
                            for u in ul do
                                yield cu |> f u
                    ] |> create tail
            
            create usl (us |> List.map (fun u -> 1N |> CU.withUnit u))