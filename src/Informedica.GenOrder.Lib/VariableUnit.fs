namespace Informedica.GenOrder.Lib

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VariableUnit =


    module VAR = Informedica.GenSolver.Lib.Variable
    module VR = VAR.ValueRange
    module UN = Informedica.GenUnits.Lib.CombiUnit
    module EQ = Informedica.GenSolver.Lib.Equation

    type VariableUnit =
        {
             Variable:   VAR.Variable
             ValuesUnit: UN.CombiUnit option
             MinUnit:    UN.CombiUnit option
             IncrUnit:   UN.CombiUnit option
             MaxUnit:    UN.CombiUnit option
         }  
         
    let create n vsu minu incru maxu = 
        let var = VAR.createSucc n VR.unrestricted
        { Variable = var; ValuesUnit = vsu; MinUnit = minu; IncrUnit = incru; MaxUnit = maxu } 
        
    let withVar vsu minu incru maxu var =    
        { Variable = var; ValuesUnit = vsu; MinUnit = minu; IncrUnit = incru; MaxUnit = maxu } 

    let apply f (qty: VariableUnit) = qty |> f

    let get = apply id

    let getAll { Variable = var; ValuesUnit = vsu; MinUnit = minu; IncrUnit = incru; MaxUnit = maxu } =
        var, vsu, minu, incru, maxu

    let getName vu = (vu |> get).Variable.Name 

    let getVar = apply (fun qty -> qty.Variable)

    let toEq cr y xs = (y |> getVar, xs |> List.map getVar) |> cr

    let toProdEq succ fail = toEq (EQ.createProductEq succ fail)

    let toSumEq succ fail = toEq (EQ.createSumEq succ fail)

    let setProp vu p vs u eqs = eqs |> Solver.solve (vu |> getName) p vs u

    let setPropWithUnit p u vu vs eqs = 
        match u with
        | Some u' -> eqs |> setProp vu p vs u'
        | None  ->  eqs

    let setVals vu = setPropWithUnit Solver.Vals (vu |> get).ValuesUnit vu

    let setMinIncl vu = setPropWithUnit Solver.MinIncl (vu |> get).MinUnit vu

    let setMinExcl vu = setPropWithUnit Solver.MinExcl (vu |> get).MinUnit vu

    let setIncr vu = setPropWithUnit Solver.Incr (vu |> get).IncrUnit vu

    let setMaxIncl vu = setPropWithUnit Solver.MaxIncl (vu |> get).MaxUnit vu

    let setMaxExcl vu = setPropWithUnit Solver.MaxExcl (vu |> get).MaxUnit vu

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Name =
        
        module N = VAR.Name

        let create ns = ns |> String.concat "." |> N.createExc

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Frequency =

        module N = Name

        let name = "Freq"

        type Frequency = Frequency of VariableUnit

        let toVar (Frequency freq) = freq

        let frequency = 
            let u = Unit.freqUnit |> Some
            let n = [name] |> N.create
            create n u u u u |> Frequency


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Time = 

        module N = Name

        let name = "Time"

        type Time = Time of VariableUnit

        let toVar (Time time) = time

        let time = 
            let u = Unit.timeUnit |> Some
            let n = [name] |> N.create
            create n u u u u |> Time


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Quantity =    
    
        module N = Name

        let name = "Qty" 

        type Quantity = Quantity of VariableUnit

        let toVar (Quantity qty) = qty

        let quantity n u = 
            let u = u |> Unit.qtyUnit |> Some
            let n = [name] |> List.append n |> N.create
            create n u u u u |> Quantity


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Total =
    
        module N = Name

        let name = "Total" 

        type Total = Total of VariableUnit

        let toVar (Total tot) = tot

        let total n u = 
            let u = u |> Unit.totalUnit |> Some
            let n = [name] |> List.append n |> N.create
            create n u u u u |> Total


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Rate =
    
        module N = Name

        let name = "Rate" 

        type Rate = Rate of VariableUnit

        let toVar (Rate rate) = rate

        let rate n u = 
            let u = u |> Unit.rateUnit |> Some
            let n = [name] |> List.append n |> N.create
            create n u u u u |> Rate


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Concentration =
    
        module N = Name

        let name = "Conc" 

        type Concentration = Concentration of VariableUnit

        let toVar (Concentration conc) = conc

        let conc n u1 u2 = 
            let u = u1 |> Unit.perUnit u2 |> Some
            let n = [name] |> List.append n |> N.create
            create n u u u u |> Concentration


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module QuantityAdjust =    
    
        module N = Name

        let name = "QtyAdjust" 

        type QuantityAdjust = QuantityAdjust of VariableUnit

        let toVar (QuantityAdjust qty) = qty

        let quantityAdjust n u1 u2 = 
            let u = u1 |> Unit.qtyUnit |> Unit.adjUnit u2 |> Some
            let n = [name] |> List.append n |> N.create
            create n u u u u |> QuantityAdjust


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TotalAdjust =
    
        module N = Name

        let name = "TotalAdjust" 

        type TotalAdjust = TotalAdjust of VariableUnit

        let toVar (TotalAdjust tot) = tot

        let totalAdjust n u1 u2 = 
            let u = u1 |> Unit.totalUnit |> Unit.adjUnit u2 |> Some
            let n = [name] |> List.append n |> N.create
            create n u u u u |> TotalAdjust


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RateAdjust =
    
        module N = Name

        let name = "RateAdjust" 

        type RateAdjust = RateAdjust of VariableUnit

        let toVar (RateAdjust rate) = rate

        let rateAdjust n u1 u2 = 
            let u = u1 |> Unit.rateUnit |> Unit.adjUnit u2 |> Some
            let n = [name] |> List.append n |> N.create
            create n u u u u |> RateAdjust


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Dose =
    
        module QT = Quantity
        module TL = Total
        module RT = Rate

        type Dose = Dose of QT.Quantity * TL.Total * RT.Rate

        let toVar (Dose(qty, total, rate)) = qty, total, rate

        let dose n u = 
            let qty   = QT.quantity n u
            let total = TL.total    n u
            let rate  = RT.rate     n u

            (qty, total, rate) |> Dose    


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DoseAdjust =

        module QT = QuantityAdjust
        module TL = TotalAdjust
        module RT = RateAdjust

        type DoseAdjust = DoseAdjust of QT.QuantityAdjust * TL.TotalAdjust * RT.RateAdjust

        let toVar (DoseAdjust(qty, total, rate)) = qty, total, rate

        let doseAdjust n u1 u2 = 
            let qty   = QT.quantityAdjust n u1 u2
            let total = TL.totalAdjust    n u1 u2
            let rate  = RT.rateAdjust     n u1 u2

            (qty, total, rate) |> DoseAdjust    