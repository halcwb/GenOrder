namespace Informedica.GenOrder.Lib

/// Helper functions to 
/// facilitate the use of the
/// `Informedica.GenSolver.Lib`
module Solver =

    open Informedica.GenUtils.Lib
    open Informedica.GenUnits.Lib

    module N = Informedica.GenSolver.Lib.Variable.Name
    module SV = Informedica.GenSolver.Lib.Api
    module VR = Informedica.GenSolver.Lib.Variable
    module VL = VR.ValueRange
    module EQ = Informedica.GenSolver.Lib.Equation

    module Props = Informedica.GenSolver.Lib.Props
    module Constraint = Informedica.GenSolver.Lib.Constraint

    type Constraint = Constraint.Constraint

    type VariableUnit = VariableUnit.VariableUnit
    
    type Equation =
        | ProductEquation of VariableUnit * VariableUnit list
        | SumEquation of VariableUnit * VariableUnit list

    let productEq = function
    | h::tail -> (h, tail) |> ProductEquation
    | _ -> "not a valid product equation" |> failwith

    let sumEq = function
    | h::tail -> (h, tail) |> SumEquation
    | _ -> "not a valid sum equation" |> failwith
    
    [<Literal>]
    let vals = "vals"
    [<Literal>]
    let minincl = "minincl"
    [<Literal>]
    let minexcl = "minexcl"
    [<Literal>]
    let incr = "incr"
    [<Literal>]
    let maxincl = "maxincl"
    [<Literal>]
    let maxexcl = "maxexcl"


    /// Create an `Equation` using a constructor **cr**
    /// a result `VariableUnit` **y** and a list of 
    /// `VariableUnit` list **xs**
    let toEq cr y xs = 
        (y |> VariableUnit.getVar, xs |> List.map VariableUnit.getVar) 
        |> cr

    /// Create a `ProdEquation` from `VariableUnit`s
    let toProdEq succ fail y xs = 
        toEq (EQ.createProductEq succ fail) y xs

    /// Create a `SumEquation` from `VariableUnit`s
    let toSumEq succ fail y xs =
        toEq (EQ.createSumEq succ fail) y xs

    let mapToSolverEqs =
        List.fold (fun acc eq ->
            match eq with 
            | ProductEquation (y, xs) -> toProdEq id (string >> exn >> raise) y xs
            | SumEquation (y, xs)     -> toSumEq id  (string >> exn >> raise) y xs
            |> List.singleton
            |> List.append acc
        ) []

    
    let replaceUnit n u eqs =
        printfn "replacing units for %A with: %s" n (u |> ValueUnit.unitToString)
        let repl c vru vrus =
            if vru |> VariableUnit.getName = n then
                (vru |> VariableUnit.setUnit u, vrus)
            else
                vru,
                vrus
                |> List.map (fun vru ->
                    if vru |> VariableUnit.getName = n then vru |> VariableUnit.setUnit u
                    else vru
                )
            |> c

        eqs
        |> List.map (fun e ->
            match e with
            | SumEquation (vru, vrus) ->
                repl SumEquation vru vrus
            | ProductEquation (vru, vrus) ->
                repl ProductEquation vru vrus
        )


    /// calculate the units for all vrus in
    /// all eqs
    let solveUnits eqs =
        let hasUnit = VariableUnit.hasUnit
        let noUnit = hasUnit >> not

        let rec solve acc eqs =

            match eqs with
            | []      -> acc
            | h::tail ->
                match h with
                | ProductEquation (y, xs) ->
                    if y::xs  |> List.hasExactlyOne noUnit then
                        if y |> noUnit then
                            let y =
                                { y with 
                                    Unit =
                                        xs 
                                        |> List.map VariableUnit.getUnit
                                        |> List.reduce (ValueUnit.calcUnit (*))
                                }
                            let h = (y, xs) |> ProductEquation
                            // start from scratch
                            h::tail 
                            |> List.append acc
                            |> replaceUnit y.Variable.Name y.Unit
                            |> solve []

                        else
                            let xs, n, u =
                                // actually y = x
                                if xs |> List.length = 1 then
                                    let x = xs.Head
                                    [ x |> VariableUnit.setUnit y.Unit ], Some x.Variable.Name, Some y.Unit
                                // y = x1 * x2 ... so 
                                // the x without a unit = y / multiple of all xs with units, i.e. (x1 * x2 .. ) 
                                else
                                    xs
                                    |> List.fold (fun acc x ->
                                        let xs', n, u = acc
                                        if x |> noUnit then // found the x without a unit
                                            let x =
                                                { x with
                                                    Unit =
                                                        xs
                                                        |> List.filter hasUnit
                                                        |> List.map VariableUnit.getUnit
                                                        |> List.reduce (ValueUnit.calcUnit (*))
                                                        |> ValueUnit.calcUnit (/) (y |> VariableUnit.getUnit)
                                                }
                                            (x::xs', (Some x.Variable.Name), (Some x.Unit))
                                        else 
                                            (x::xs', n, u)
                                    ) ([], None, None)

                            let h = (y, xs) |> ProductEquation
                            // start from scratch
                            h::tail 
                            |> List.append acc
                            |> replaceUnit (n |> Option.get) (u |> Option.get)
                            |> solve []

                    else
                        solve (h::acc) tail
                
                | SumEquation (y, xs) ->
                    if y::xs |> List.forall hasUnit ||
                       y::xs |> List.forall noUnit then 
                        solve (h::acc) tail
                    
                    else
                        // get the names of vrus with no unit
                        let ns =
                            y::xs
                            |> List.filter noUnit
                            |> List.map (VariableUnit.getName)
                        // find the vru with a unit
                        let x = 
                            y::xs 
                            |> List.find hasUnit
                        // start from scratch
                        ({ y with Unit = x.Unit }, xs |> List.map (VariableUnit.setUnit x.Unit))
                        |> SumEquation
                        |> List.singleton
                        |> List.append tail
                        |> List.append acc
                        // make sure that all vrus in all eqs get the unit
                        |> (fun eqs ->
                            ns 
                            |> List.fold (fun acc n ->
                                acc |> replaceUnit n x.Unit
                            ) eqs
                        )
                        |> solve []

        solve [] eqs
        

    let toVariableUnits =
        List.map (fun eq ->
            match eq with
            | ProductEquation (y, xs) 
            | SumEquation     (y, xs) -> y::xs
        )


    /// Turn a set of values `vs` to base values 
    let toBase n eqs vs = 

        eqs 
        |> toVariableUnits
        |> List.tryFindInList (VariableUnit.getName >> ((=) n))
        |> function 
        | Some vru ->
            vru
            |> VariableUnit.getUnit
            |> fun u -> 
                vs
                |> Set.map (ValueUnit.create u)
                |> Set.map ValueUnit.toBase

        | None -> 
            sprintf "could not find %A in toBase n eqs vs" n
            |> failwith


    let mapFromSolverEqs orig eqs =
        let vrusl = orig |> toVariableUnits
        let vars = 
            eqs
            |> List.collect EQ.toVars 
            |> List.distinct

        vrusl
        |> List.map (fun vrus ->
            vrus 
            |> List.map (fun vru ->
                { vru with 
                    Variable =
                        vars
                        |> List.tryFind (fun v -> v.Name = vru.Variable.Name)
                        |> function 
                        | Some v -> v
                        | None -> 
                            sprintf "could not find %A" vru.Variable.Name
                            |> failwith
                }
            )
        )



    let setVals lim n p eqs =
        eqs
        |> SV.setVariableValues lim n p
         

    let filterEqsWithUnits = 
        List.filter (fun eq ->
            match eq with
            | ProductEquation(y, xs) 
            | SumEquation (y, xs) ->
                y::xs |> List.forall VariableUnit.hasUnit       
        )


    let propToBase n eqs = function
    | Props.Vals vs -> 
        vs 
        |> toBase n eqs 
        |> Props.Vals
    | Props.Increment vs ->
        vs 
        |> toBase n eqs 
        |> Props.Increment
    | Props.MinIncl v ->
        v
        |> Set.singleton
        |> toBase n eqs
        |> Set.toSeq
        |> Seq.head
        |> Props.MinIncl
    | Props.MinExcl v ->
        v
        |> Set.singleton
        |> toBase n eqs
        |> Set.toSeq
        |> Seq.head
        |> Props.MinExcl
    | Props.MaxIncl v ->
        v
        |> Set.singleton
        |> toBase n eqs
        |> Set.toSeq
        |> Seq.head
        |> Props.MaxIncl
    | Props.MaxExcl v ->
        v
        |> Set.singleton
        |> toBase n eqs
        |> Set.toSeq
        |> Seq.head
        |> Props.MaxExcl 


        
    // Solve a set of equations setting a property `p` with
    // name `n`, to a valueset `vs`.
    let applySolve sortQue log exact lim n p eqs = 
    
        let toBase = propToBase n eqs

        eqs
        // use only eqs with all vrus have units
        |> filterEqsWithUnits
        |> mapToSolverEqs
        |> SV.solve sortQue log exact lim n (p |> toBase)
        |> mapFromSolverEqs eqs


    let solve log lim = 
        let sortQue = Informedica.GenSolver.Lib.Solver.sortQue
        applySolve sortQue log true lim //(printfn "%s") //|> memSolve


    let solveConstraints log (cs : Constraint list) eqs =

        let cs =
            cs 
            |> List.map (fun c ->
                {
                    c with
                        Property = 
                            c.Property 
                            |> propToBase c.Name eqs
                }
            )
        
        eqs
        // use only eqs with all vrus have units
        |> filterEqsWithUnits
        |> mapToSolverEqs
        |> SV.solveConstraints log true cs
        |> mapFromSolverEqs eqs

        