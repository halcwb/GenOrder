namespace Informedica.GenOrder.Lib

/// Helper functions to 
/// facilitate the use of the
/// `Informedica.GenSolver.Lib`
module Solver =

    open Informedica.GenUtils.Lib
    open Informedica.GenUnits.Lib

    module N = Informedica.GenSolver.Lib.Variable.Name
    module SV = Informedica.GenSolver.Api
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


    let solveUnits eqs =
        let hasUnit = VariableUnit.hasUnit
        let noUnit = hasUnit >> not

        let rec solve acc eqs = 
            match eqs with
            | [] -> acc
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
                            |> solve []
                        else
                            let xs =
                                xs
                                |> List.map (fun x ->
                                    if x |> noUnit then
                                        { x with
                                            Unit =
                                                xs
                                                |> List.filter hasUnit
                                                |> List.map VariableUnit.getUnit
                                                |> List.reduce (ValueUnit.calcUnit (*))
                                                |> ValueUnit.calcUnit (/) (y |> VariableUnit.getUnit)
                                        }
                                    else x
                                )
                            let h = (y, xs) |> ProductEquation
                            // start from scratch
                            h::tail 
                            |> List.append acc
                            |> solve []

                    else
                        solve ((ProductEquation (y, xs))::acc) tail
                
                | SumEquation (y, xs) ->
                    if y::xs |> List.forall hasUnit then 
                        solve (SumEquation (y, xs)::acc) tail
                    
                    else
                        let u = 
                            y::xs 
                            |> List.find hasUnit
                            |> VariableUnit.getUnit
                        // start from scratch
                        ({ y with Unit = u }, xs |> List.map (VariableUnit.setUnit u))
                        |> SumEquation
                        |> List.singleton
                        |> List.append tail
                        |> List.append acc
                        |> solve []

        solve [] eqs
        

    let toVariableUnits =
        List.map (fun eq ->
            match eq with
            | ProductEquation (y, xs) | SumEquation (y, xs) -> y::xs
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
            printf "could not find %A in toBase n eqs vs" n
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

    let solveVals n p vs eqs =
        match vs with
        | [] -> eqs
        | _  ->
            eqs
            |> SV.solve (fun s -> printfn "%s" s) n (p |> propToString) vs
         

    let filterEqsWithUnits = 
        List.filter (fun eq ->
            match eq with
            | ProductEquation(y, xs) 
            | SumEquation (y, xs) ->
                y::xs |> List.forall VariableUnit.hasUnit
                
        )
        

    // Solve a set of equations setting a property `p` with
    // name `n`, to a valueset `vs`.
    let solve (N.Name n) p vs eqs =
        let eqs =
            eqs 
            |> solveUnits

        eqs
        |> filterEqsWithUnits
        |> mapToSolverEqs
        |> solveVals n p (vs |> toBase n eqs)
        |> mapFromSolverEqs eqs

