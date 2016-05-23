namespace Informedica.GenOrder.Lib

/// Functions that deal with the `VariableUnit` type
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VariableUnit =


    module VAR = Informedica.GenSolver.Lib.Variable
    module VR = VAR.ValueRange
    module UN = Informedica.GenUnits.Lib.CombiUnit
    module UG = Informedica.GenUnits.Lib.UnitGroup
    module GS = UnitGroups
    module GN = Informedica.GenUnits.Lib.Unit.Name
    module EQ = Informedica.GenSolver.Lib.Equation

    /// A `VariableUnit` is the combination of 
    /// an `Informedica.GenSolver.Lib.Variable` with
    /// an `Informedica.GenUnits.Lib.CombiUnit`
    /// The `Variable` stores the base values according
    /// to the `UnitGroup`
    type VariableUnit =
        {
             Variable:  VAR.Variable
             UnitGroup: UG.UnitGroup
         }  
         
    let create n ug = 
        let var = VAR.createSucc n VR.unrestricted
        { Variable = var; UnitGroup = ug } 
        
    let withVar ug var =    
        { Variable = var; UnitGroup = ug } 

    let apply f (qty: VariableUnit) = qty |> f

    let get = apply id

    let getAll { Variable = var; UnitGroup = ug } =
        var, ug

    let getName vu = (vu |> get).Variable.Name 

    let getVar = apply (fun vu -> vu.Variable)

    let getUnitGroup = apply (fun vu -> vu.UnitGroup)

    let toEq cr y xs = (y |> getVar, xs |> List.map getVar) |> cr

    let toProdEq succ fail = toEq (EQ.createProductEq succ fail)

    let toSumEq succ fail = toEq (EQ.createSumEq succ fail)

    let setProp vu p vs u eqs = eqs |> Solver.solve (vu |> getName) p vs u

    let find getN n vull =
        match vull |> List.filter (fun vl -> vl |> List.exists (fun vu -> vu |> getN = n )) with
        | [] -> None
        | ft ->
            ft
            |> List.head
            |> List.find (fun vu -> vu |> getN = n )
            |> Some   

    let findVarUnit = find getName

    let fromVar toVar c vrll vu = 
        let var, ug = vu |> (toVar >> getAll)
        let n = var |> VAR.getName
        let find = find VAR.getName
        
        match vrll |> find n with
        | Some x -> x |> withVar ug |> c
        | None   -> vu

    let setPropWithUnit p u vu vs eqs = 
        match u with
        | Some u' -> eqs |> setProp vu p vs u'
        | None  ->  eqs

    let setVals u vu =    setPropWithUnit Solver.Vals u vu

    let setMinIncl u vu = setPropWithUnit Solver.MinIncl u vu

    let setMinExcl u vu = setPropWithUnit Solver.MinExcl u vu

    let setIncr u vu =    setPropWithUnit Solver.Incr u vu

    let setMaxIncl u vu = setPropWithUnit Solver.MaxIncl u vu

    let setMaxExcl u vu = setPropWithUnit Solver.MaxExcl u vu

    let toString vu = 
        let (VAR.Name.Name n) = vu |> getName
        let ug = vu.UnitGroup |> UG.toString
        n +
        (vu.Variable 
        |> VAR.getValueRange
        |> VR.toString) + " " + ug
        

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

        let fromVar = fromVar toVar Frequency 
        
        let frequency n = 
            let u = Unit.freqUnit |> UG.fromUnit
            let n = [name] |> List.append n |> N.create
            create n u |> Frequency

        let toString freq = 
            freq |> toVar |> toString


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Time = 

        module N = Name

        let name = "Time"

        type Time = Time of VariableUnit

        let toVar (Time time) = time

        let fromVar = fromVar toVar Time 

        let time n = 
            let u = Unit.timeUnit |> UG.fromUnit
            let n = [name] |> List.append n |> N.create
            create n u |> Time

        let toString tme = 
            tme |> toVar |> toString


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Count =    
    
        module N = Name

        let name = "Count" 

        type Count = Count of VariableUnit

        let toVar (Count qty) = qty

        let fromVar = fromVar toVar Count 

        let count n = 
            let n = [name] |> List.append n |> N.create
            create n GS.count |> Count

        let toString qty = 
            qty |> toVar |> toString


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Quantity =    
    
        module N = Name

        let name = "Qty" 

        type Quantity = Quantity of VariableUnit

        let toVar (Quantity qty) = qty

        let fromVar = fromVar toVar Quantity 

        let quantity n ug = 
            let u = ug |> UG.fromString
            let n = [name] |> List.append n |> N.create
            create n u |> Quantity

        let toString qty = 
            qty |> toVar |> toString


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Total =
    
        module N = Name

        let name = "Total" 

        type Total = Total of VariableUnit

        let toVar (Total tot) = tot

        let fromVar = fromVar toVar Total 
        
        let total n u = 
            let u = (u |> UG.fromString) / GS.time
            let n = [name] |> List.append n |> N.create
            create n u |> Total

        let toString tot = 
            tot |> toVar |> toString


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Rate =
    
        module N = Name

        let name = "Rate" 

        type Rate = Rate of VariableUnit

        let toVar (Rate rate) = rate

        let fromVar = fromVar toVar Rate 
        
        let rate n u = 
            let u = (u |> UG.fromString) / GS.time
            let n = [name] |> List.append n |> N.create
            create n u |> Rate

        let toString rte = 
            rte |> toVar |> toString


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Concentration =
    
        module N = Name

        let name = "Conc" 

        type Concentration = Concentration of VariableUnit

        let toVar (Concentration conc) = conc

        let fromVar = fromVar toVar Concentration 
        
        let conc n u1 u2 = 
            let u = (u1 |> UG.fromString) / (u2 |> UG.fromString)
            let n = [name] |> List.append n |> N.create
            create n u |> Concentration

        let toString cnc = 
            cnc |> toVar |> toString


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module QuantityAdjust =    
    
        module N = Name

        let name = "QtyAdjust" 

        type QuantityAdjust = QuantityAdjust of VariableUnit

        let toVar (QuantityAdjust qty) = qty

        let fromVar = fromVar toVar QuantityAdjust 
        
        let quantityAdjust n u1 u2 = 
            let u = (u1 |> UG.fromString) / (u2 |> UG.fromString)
            let n = [name] |> List.append n |> N.create
            create n u |> QuantityAdjust

        let toString qta = 
            qta |> toVar |> toString


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TotalAdjust =
    
        module N = Name

        let name = "TotalAdjust" 

        type TotalAdjust = TotalAdjust of VariableUnit

        let toVar (TotalAdjust tot) = tot

        let fromVar = fromVar toVar TotalAdjust 
        
        let totalAdjust n u1 u2 = 
            let u = (u1 |> UG.fromString) / (u2 |> UG.fromString) / GS.time
            let n = [name] |> List.append n |> N.create
            create n u |> TotalAdjust

        let toString toa = 
            toa |> toVar |> toString


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RateAdjust =
    
        module N = Name

        let name = "RateAdjust" 

        type RateAdjust = RateAdjust of VariableUnit

        let toVar (RateAdjust rate) = rate

        let fromVar = fromVar toVar RateAdjust 
        
        let rateAdjust n u1 u2 = 
            let u = (u1 |> UG.fromString) / (u2 |> UG.fromString) / GS.time
            let n = [name] |> List.append n |> N.create
            create n u |> RateAdjust

        let toString rta = 
            rta |> toVar |> toString


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Dose =
    
        module QT = Quantity
        module TL = Total
        module RT = Rate

        type Dose = Dose of QT.Quantity * TL.Total * RT.Rate

        let toVar (Dose(qty, total, rate)) = 
            qty |> QT.toVar, total |> TL.toVar, rate |> RT.toVar

        let fromVar eqs (Dose(qty, tot, rte)) = 
            let qty = fromVar QT.toVar QT.Quantity eqs qty
            let tot = fromVar TL.toVar TL.Total    eqs tot
            let rte = fromVar RT.toVar RT.Rate     eqs rte
            (qty, tot, rte) |> Dose

        let dose n u = 
            let qty   = QT.quantity n u
            let total = TL.total    n u
            let rate  = RT.rate     n u

            (qty, total, rate) |> Dose

        let toString (Dose(qty, tot, rte))  =
            [
                qty |> QT.toString
                tot |> TL.toString
                rte |> RT.toString
            ]

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DoseAdjust =

        module QT = QuantityAdjust
        module TL = TotalAdjust
        module RT = RateAdjust

        type DoseAdjust = DoseAdjust of QT.QuantityAdjust * TL.TotalAdjust * RT.RateAdjust

        let toVar (DoseAdjust(qty, total, rate)) = 
            qty |> QT.toVar, total |> TL.toVar, rate |> RT.toVar

        let fromVar eqs (DoseAdjust(qty, tot, rte)) = 
            let qty = fromVar QT.toVar QT.QuantityAdjust eqs qty
            let tot = fromVar TL.toVar TL.TotalAdjust    eqs tot
            let rte = fromVar RT.toVar RT.RateAdjust     eqs rte
            (qty, tot, rte) |> DoseAdjust

        let doseAdjust n u1 u2 = 
            let qty   = QT.quantityAdjust n u1 u2
            let total = TL.totalAdjust    n u1 u2
            let rate  = RT.rateAdjust     n u1 u2

            (qty, total, rate) |> DoseAdjust    

        let toString (DoseAdjust(qty, tot, rte))  =
            [
                qty |> QT.toString
                tot |> TL.toString
                rte |> RT.toString
            ]

