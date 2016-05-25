namespace Informedica.GenOrder.Lib

/// Functions that deal with the `VariableUnit` type
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VariableUnit =


    module VR = Informedica.GenSolver.Lib.Variable
    module EQ = Informedica.GenSolver.Lib.Equation
    module VL = VR.ValueRange
    module CS = Informedica.GenUnits.Lib.Constants
    module UN = Informedica.GenUnits.Lib.CombiUnit
    module GN = Informedica.GenUnits.Lib.Unit.Name
    module UG = Informedica.GenUnits.Lib.UnitGroup
    module GS = UnitGroups

    /// A `VariableUnit` is the combination of 
    /// an `Informedica.GenSolver.Lib.Variable` with
    /// an `Informedica.GenUnits.Lib.CombiUnit`
    /// The `Variable` stores the base values according
    /// to the `UnitGroup`
    type VariableUnit =
        {
             /// Stores the values/range
             Variable:  VR.Variable
             /// Stores the unit group
             UnitGroup: UG.UnitGroup
         }  
         
    /// Create a new `VariableUnit` with
    /// `Name` **nme** and `UnitGroup` **ung**
    let create nme ung = 
        let var = VR.createSucc nme VL.unrestricted
        { Variable = var; UnitGroup = ung } 
        
    /// Create a `VariableUnit` with
    /// `Variable` **var** and `UnitGroup` **ung**
    let withVar ung var =    
        { Variable = var; UnitGroup = ung } 

    /// Apply **f** to `VariableUnit` **vru**
    let apply f (vru: VariableUnit) = vru |> f

    /// Utitlity function to facitlitate type inference
    let get = apply id

    /// Get all record fiels from a `VariableUnit`
    let getAll { Variable = var; UnitGroup = ung } =
        var, ung

    /// Get the `Variable` from a `VariableUnit`
    let getVar = apply (fun vu -> vu.Variable)

    /// Get the `Variable.Name` from a `VariableUnit` **vru**
    let getName vru = (vru |> getVar).Name

    /// Get the `UnitGroup` from a `VariableUnit`
    let getUnitGroup = apply (fun vu -> vu.UnitGroup)

    /// Create an `Equation` using a constructor **cr**
    /// a result `VariableUnit` **y** and a list of 
    /// `VariableUnit` list **xs**
    let toEq cr y xs = (y |> getVar, xs |> List.map getVar) |> cr

    /// Create a `ProdEquation` from `VariableUnit`s
    let toProdEq succ fail = toEq (EQ.createProductEq succ fail)

    /// Create a `SumEquation` from `VariableUnit`s
    let toSumEq succ fail = toEq (EQ.createSumEq succ fail)

    /// Set a property **p** of a `VariableUnit` **vru** 
    /// with values **vs** and `CombinedUnit` **unt** in
    /// a `Equation` list
    let setProp vru p vs unt eqs = eqs |> Solver.solve (vru |> getName) p vs unt

    /// Try find the first element with **n**
    /// in a list of list **xsl**
    /// with a function **get** to
    /// get **n** from an element
    let tryFind get n xsl =
        let pred x = x |> get = n
        match xsl |> List.filter (fun xs -> xs |> List.exists pred) with
        | [] -> None
        | xs::_ -> xs |> List.find pred |> Some   

    /// Try find the first `VariableUnit` with 
    /// a specific `Name` in a list of lists
    let tryFindVarUnt = tryFind getName

    /// Set a specific `VariableUnit` with
    /// a `Variable` from a 
    /// list of `Variable` lists **vrll** 
    /// that has the same name as **vru**.
    /// Return the unmodified **vru** if 
    /// no `Variable` can be found. 
    /// **c** is used to construct the specific
    /// variable and **toVar** to extract the
    /// current variable from **vru**
    let fromVar toVar c vrll vru = 
        let var, ung = vru |> (toVar >> getAll)
        let n = var |> VR.getName
        let find = tryFind VR.getName
        
        match vrll |> find n with
        | Some x -> x |> withVar ung |> c
        | None   -> vru

    /// Set a property **prop** of a `VariableUnit` 
    /// **vru** with values **vs** and unit **unt**
    /// in a set of equations **eqs**. Return the 
    /// unmodified set of **unt** is `None`.
    let setPropWithUnit prop unt vru vs eqs = 
        match unt with
        | Some unt' -> eqs |> setProp vru prop vs unt'
        | None      -> eqs

    /// Set values of `VariableUnit` **vru** with values with
    /// unit **unt** in a list of equations
    let setVals unt vru    = setPropWithUnit Solver.Vals    unt vru

    /// Set minimum inclusive of `VariableUnit` **vru** with values with
    /// unit **unt** in a list of equations
    let setMinIncl unt vru = setPropWithUnit Solver.MinIncl unt vru

    /// Set minimum exclusive of `VariableUnit` **vru** with values with
    /// unit **unt** in a list of equations
    let setMinExcl unt vru = setPropWithUnit Solver.MinExcl unt vru

    /// Set increment of `VariableUnit` **vru** with values with
    /// unit **unt** in a list of equations
    let setIncr unt vru    = setPropWithUnit Solver.Incr    unt vru

    /// Set maximum inclusive of `VariableUnit` **vru** with values with
    /// unit **unt** in a list of equations
    let setMaxIncl unt vru = setPropWithUnit Solver.MaxIncl unt vru

    /// Set maximum exclusive of `VariableUnit` **vru** with values with
    /// unit **unt** in a list of equations
    let setMaxExcl unt vru = setPropWithUnit Solver.MaxExcl unt vru

    /// Get the string representation of a `VariableUnit` **vru**
    let toString vru = 
        let (VR.Name.Name n) = vru |> getName
        let ug = vru.UnitGroup |> UG.toString

        n +
        (vru.Variable 
        |> VR.getValueRange
        |> VL.toString) + " " + ug

    /// Helper functions for `Informedica.GenSolver.Variable.Name` type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Name =
        
        module N = VR.Name

        /// Create a `Name` from a list of strings that 
        let create ns = ns |> String.concat "." |> N.createExc

    /// Type and functions that represent a frequency
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Frequency =

        module N = Name

        /// String representation of the type
        let name = "Freq"

        /// Type that represents a frequency
        type Frequency = Frequency of VariableUnit

        /// Turn `Frequency` in a `VariableUnit`
        let toVarUnt (Frequency freq) = freq

        /// Set a `Frequency` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Frequency 
        
        /// Create a `Frequency` with name **n**
        let frequency n = 
            let u = GS.count |> UG.perGroup CS.timeGroup
            let n = [name] |> List.append n |> N.create
            create n u |> Frequency

        /// Turn a `Frequency` to a string
        let toString freq = 
            freq |> toVarUnt |> toString


    /// Type and functions that represent a time
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Time = 

        module N = Name

        /// String representation of the type
        let name = "Time"

        /// Type that represents a time
        type Time = Time of VariableUnit

        /// Turn `Time` in a `VariableUnit`
        let toVarUnt (Time time) = time

        /// Set a `Time` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Time 

        /// Create a `Time` with name **n**
        let time n = 
            let u = GS.time
            let n = [name] |> List.append n |> N.create
            create n u |> Time

        /// Turn a `Time` to a string
        let toString tme = 
            tme |> toVarUnt |> toString

    /// Type and functions that represent a count
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Count =    
    
        module N = Name

        /// String representation of the type
        let name = "Count" 

        /// Type that represents a count
        type Count = Count of VariableUnit

        /// Turn `Count` in a `VariableUnit`
        let toVarUnt (Count qty) = qty

        /// Set a `Count` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Count 

        /// Create a `Count` with name **n**
        let count n = 
            let n = [name] |> List.append n |> N.create
            create n GS.count |> Count

        /// Turn a `Count` to a string
        let toString qty = 
            qty |> toVarUnt |> toString


    /// Type and functions that represent a quantity
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Quantity =    
    
        module N = Name

        /// String representation of the type
        let name = "Qty" 

        /// Type that represents a quantity
        type Quantity = Quantity of VariableUnit

        /// Turn `Quantity` in a `VariableUnit`
        let toVarUnt (Quantity qty) = qty

        /// Set a `Quantity` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Quantity 

        /// Create a `Quantity` with name **n**
        /// and `UnitGroup` **ung**
        let quantity n ung = 
            let u = ung |> UG.fromString
            let n = [name] |> List.append n |> N.create
            create n u |> Quantity

        /// Turn a `Quantity` to a string
        let toString qty = 
            qty |> toVarUnt |> toString


    /// Type and functions that represent a total,
    /// and a total is a quantity over a time period
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Total =
    
        module N = Name

        /// String representation of the type of the type
        let name = "Total" 

        /// Type that represents a total
        type Total = Total of VariableUnit

        /// Turn `Total` in a `VariableUnit`
        let toVarUnt (Total tot) = tot

        /// Set a `Total` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Total 
        
        /// Create a `Total` with name **n**
        /// and `UnitGroup` **ung**
        let total n ung = 
            let u = ung |> UG.fromString |> UG.perGroup CS.timeGroup
            let n = [name] |> List.append n |> N.create
            create n u |> Total

        /// Turn a `Total` to a string
        let toString tot = 
            tot |> toVarUnt |> toString


    /// Type and functions that represent a rate,
    /// and a rate is a quantity per time
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Rate =
    
        module N = Name

        /// String representation of the type
        let name = "Rate" 

        /// Type that represents a rate
        type Rate = Rate of VariableUnit

        /// Turn `Rate` in a `VariableUnit`
        let toVarUnt (Rate rate) = rate

        /// Set a `Rate` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Rate 
        
        /// Create a `Rate` with name **n**
        /// and `UnitGroup` **ung**
        let rate n ung = 
            let u = ung |> UG.fromString |> UG.perGroup CS.timeGroup
            let n = [name] |> List.append n |> N.create
            create n u |> Rate

        /// Turn a `Rate` to a string
        let toString rte = 
            rte |> toVarUnt |> toString


    /// Type and functions that represent a concentration,
    /// and a concentration is a quantity per time
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Concentration =
    
        module N = Name

        /// String representation of the type
        let name = "Conc" 

        /// Type that represents a concentration
        type Concentration = Concentration of VariableUnit

        /// Turn `Concentration` in a `VariableUnit`
        let toVarUnt (Concentration conc) = conc

        /// Set a `Concentration` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Concentration 
        
        /// Create a `Concentration` with name **n**
        /// and `UnitGroup` **ung1** per **ung2**
        let conc n ung1 ung2 = 
            let u = ung1 |> UG.fromString |> UG.perGroup ung2
            let n = [name] |> List.append n |> N.create
            create n u |> Concentration

        /// Turn a `Concentration` to a string
        let toString cnc = 
            cnc |> toVarUnt |> toString


    /// Type and functions that represent a adjusted quantity,
    /// and a adjusted quantity is a quantity per time
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module QuantityAdjust =    
    
        module N = Name

        /// String representation of the type
        let name = "QtyAdjust" 

        /// Type that represents a adjusted quantity
        type QuantityAdjust = QuantityAdjust of VariableUnit

        /// Turn `QuantityAdjust` in a `VariableUnit`
        let toVarUnt (QuantityAdjust qty) = qty

        /// Set a `QuantityAdjust` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt QuantityAdjust 
        
        /// Create a `QuantityAdjust` with name **n**
        /// and `UnitGroup` **ung1** adjusted by **ung2**
        let quantityAdjust n ung1 ung2 = 
            let u = ung1 |> UG.fromString |> UG.perGroup ung2
            let n = [name] |> List.append n |> N.create
            create n u |> QuantityAdjust

        /// Turn a `QuantityAdjust` to a string
        let toString qta = 
            qta |> toVarUnt |> toString


    /// Type and functions that represent a adjusted total,
    /// and a adjusted total is a quantity per time
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TotalAdjust =
    
        module N = Name

        /// String representation of the type
        let name = "TotalAdjust" 

        /// Type that represents a adjusted total
        type TotalAdjust = TotalAdjust of VariableUnit

        /// Turn `TotalAdjust` in a `VariableUnit`
        let toVarUnt (TotalAdjust tot) = tot

        /// Set a `TotalAdjust` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt TotalAdjust 
        
        /// Create a `TotalAdjust` with name **n**
        /// and `UnitGroup` **ung1** adjusted by **ung2**
        let totalAdjust n ung1 ung2 = 
            let u = ung1 |> UG.fromString |> UG.perGroup ung2 |> UG.perGroup CS.timeGroup
            let n = [name] |> List.append n |> N.create
            create n u |> TotalAdjust

        /// Turn a `TotalAdjust` to a string
        let toString toa = 
            toa |> toVarUnt |> toString


    /// Type and functions that represent a adjusted rate,
    /// and a adjusted rate is a quantity per time unit
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RateAdjust =
    
        module N = Name

        /// String representation of the type
        let name = "RateAdjust" 

        /// Type that represents a adjusted rate
        type RateAdjust = RateAdjust of VariableUnit

        /// Turn `RateAdjust` in a `VariableUnit`
        let toVarUnt (RateAdjust rate) = rate

        /// Set a `RateAdjust` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt RateAdjust 
        
        /// Create a `RateAdjust` with name **n**
        /// and `UnitGroup` **ung1** adjusted by **ung2**
        let rateAdjust n ung1 ung2 = 
            let u = ung1 |> UG.fromString |> UG.perGroup ung2 |> UG.perGroup CS.timeGroup
            let n = [name] |> List.append n |> N.create
            create n u |> RateAdjust

        /// Turn a `RateAdjust` to a string
        let toString rta = 
            rta |> toVarUnt |> toString


    /// Type and functions that represent a dose,
    /// and a dose is a dose quantity, total and rate
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Dose =
    
        module QT = Quantity
        module TL = Total
        module RT = Rate

        /// Type that represents a dose quantity, total and rate
        type Dose = Dose of QT.Quantity * TL.Total * RT.Rate

        /// Turn `Dose` in a dose quantity, total and rate `VariableUnit`
        let toVarUnt (Dose(qty, total, rate)) = 
            qty |> QT.toVarUnt, total |> TL.toVarUnt, rate |> RT.toVarUnt

        /// Set a `Dose` with a quantity, total and rate `Variable` 
        /// in a list of `Variable` lists
        let fromVar eqs (Dose(qty, tot, rte)) = 
            let qty = fromVar QT.toVarUnt QT.Quantity eqs qty
            let tot = fromVar TL.toVarUnt TL.Total    eqs tot
            let rte = fromVar RT.toVarUnt RT.Rate     eqs rte
            (qty, tot, rte) |> Dose

        /// Create a `Dose` with name **n**
        /// and `UnitGroup` **ung**
        let dose n ung = 
            let qty   = QT.quantity n ung
            let total = TL.total    n ung
            let rate  = RT.rate     n ung

            (qty, total, rate) |> Dose

        /// Turn a `Dose` to a string
        let toString (Dose(qty, tot, rte))  =
            [
                qty |> QT.toString
                tot |> TL.toString
                rte |> RT.toString
            ]

    /// Type and functions that represent an adjusted dose,
    /// and a dose is an adjusted dose quantity, total and rate
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DoseAdjust =

        module QT = QuantityAdjust
        module TL = TotalAdjust
        module RT = RateAdjust

        /// Type that represents an adjusted dose quantity, total and rate
        type DoseAdjust = DoseAdjust of QT.QuantityAdjust * TL.TotalAdjust * RT.RateAdjust

        /// Turn `DoseAdjust` in an adjusted quantity, total and rate `VariableUnit`
        let toVarUnt (DoseAdjust(qty, total, rate)) = 
            qty |> QT.toVarUnt, total |> TL.toVarUnt, rate |> RT.toVarUnt

        /// Set a `DoseAdjust` with an adjusted quantity, total and rate `Variable` 
        /// in a list of `Variable` lists
        let fromVar eqs (DoseAdjust(qty, tot, rte)) = 
            let qty = fromVar QT.toVarUnt QT.QuantityAdjust eqs qty
            let tot = fromVar TL.toVarUnt TL.TotalAdjust    eqs tot
            let rte = fromVar RT.toVarUnt RT.RateAdjust     eqs rte
            (qty, tot, rte) |> DoseAdjust

        /// Create a `DoseAdjust` with name **n**
        /// and `UnitGroup` **ung1** adjusted by **ung2**
        let doseAdjust n ung1 ung2 = 
            let qty   = QT.quantityAdjust n ung1 ung2
            let total = TL.totalAdjust    n ung1 ung2
            let rate  = RT.rateAdjust     n ung1 ung2

            (qty, total, rate) |> DoseAdjust    

        /// Turn a `DoseAdjust` to a string
        let toString (DoseAdjust(qty, tot, rte))  =
            [
                qty |> QT.toString
                tot |> TL.toString
                rte |> RT.toString
            ]

