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

    module Props =

        /// The properties that can be set
        /// for a variable.
        type Prop = 
            | Vals
            | MinIncl
            | MinExcl
            | Incr
            | MaxIncl
            | MaxExcl

        /// Return a string for a property `Prop`
        let propToString = function
            | Vals -> vals
            | MinIncl -> minincl
            | MinExcl -> minexcl
            | Incr -> incr
            | MaxIncl -> maxincl
            | MaxExcl -> maxexcl

    open Props

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
        |> List.tryFindInList (VariableUnit.getName >> N.toString >> ((=) n))
        |> function 
        | Some vru ->
            vru
            |> VariableUnit.getUnit
            |> fun u -> 
                vs
                |> List.map (ValueUnit.create u)
                |> List.map ValueUnit.toBase
        | None -> 
            printfn "could not find %A in toBase n eqs vs" n
            []    


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
                            printfn "could not find %A" vru.Variable.Name
                            vru.Variable
                }
            )
        )

    // helper function to prevent setting vs to 
    // empty list when vals have no unit, so tobase 
    // returns an empty list
    let solveVals solveE sortQue lim pf n p vs eqs =
        match vs with
        | [] -> eqs
        | _  ->
            eqs
            |> SV.solve solveE sortQue lim pf n (p |> propToString) vs
         

    let filterEqsWithUnits = 
        List.filter (fun eq ->
            match eq with
            | ProductEquation(y, xs) 
            | SumEquation (y, xs) ->
                y::xs |> List.forall VariableUnit.hasUnit
                
        )
        

    // Solve a set of equations setting a property `p` with
    // name `n`, to a valueset `vs`.
    let solve_ solveE sortQue lim pf (N.Name n) p vs eqs =

        eqs
        // use only eqs with all vrus have units
        |> filterEqsWithUnits
        |> mapToSolverEqs
        |> solveVals solveE sortQue lim pf n p (vs |> toBase n eqs)
        |> mapFromSolverEqs eqs


    let memSolve f =
        let cache = ref Map.empty
        fun n p vs eqs ->
            match (!cache).TryFind(n, p, vs, eqs) with
            | Some r -> r
            | None ->
                let r = f n p vs eqs
                cache := (!cache).Add((n, p, vs, eqs), r)
                r

    let solve solveE pf lim = 
        let sortQue = Informedica.GenSolver.Lib.Solver.sortQue
        solve_ solveE sortQue lim pf //(printfn "%s") //|> memSolve