namespace Informedica.GenOrder.Lib

/// Functions that deal with the `VariableUnit` type
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VariableUnit =

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenWrap.Lib.WrappedString
    
    module Variable    = Informedica.GenSolver.Lib.Variable
    module VariableDto = Informedica.GenSolver.Dtos.Variable
    module Equation    = Informedica.GenSolver.Lib.Equation
    module ValueRange  = Variable.ValueRange
    module Constants   = Informedica.GenUnits.Lib.Constants
    module CombiUnit   = Informedica.GenUnits.Lib.CombiUnit
    module UnitName    = Informedica.GenUnits.Lib.Unit.Name
    module UnitGroup   = Informedica.GenUnits.Lib.UnitGroup
    module UnitGroups  = UnitGroups

    /// A `VariableUnit` is the combination of 
    /// an `Informedica.GenSolver.Lib.Variable` with
    /// an `Informedica.GenUnits.Lib.CombiUnit`
    /// The `Variable` stores the base values according
    /// to the `UnitGroup`
    type VariableUnit =
        {
            /// Stores the values/range
            Variable:  Variable.Variable
            /// Stores the unit group
            UnitGroup: UnitGroup.UnitGroup
        }  
         
    /// Create a new `VariableUnit` with
    /// `Name` **nme** and `UnitGroup` **ung**
    let createNew nm ung = 
        let var = 
            Variable.createSucc nm ValueRange.unrestricted
            |> Variable.setNonZeroOrNegative
        { Variable = var; UnitGroup = ung } 
        
    /// Create a `VariableUnit` with preset values
    let create succ fail nm unr vs min incr max ung =
        let succ = Some
        let fail = Option.none

        let vlr = ValueRange.create succ fail unr vs min incr max
        match vlr with
        | Some vlr' ->
            let var = Variable.create id nm vlr'        
            { Variable = var; UnitGroup = ung }
        | None -> createNew nm ung

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
    let toProdEq succ fail = toEq (Equation.createProductEq succ fail)

    /// Create a `SumEquation` from `VariableUnit`s
    let toSumEq succ fail = toEq (Equation.createSumEq succ fail)

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
        let n = var |> Variable.getName
        let find = tryFind Variable.getName
        
        match vrll |> find n with
        | Some x -> x |> withVar ung |> c
        | None   -> vru

    /// Set the 'Name' to the `Variable` of the `VariableUnit`
    let setName nm vru = 
        { vru with
            Variable = vru.Variable |> Variable.setName nm }        


    /// Get the string representation of a `VariableUnit` **vru**
    let toString vru = 
        let (Variable.Name.Name n) = vru |> getName
        let ug = vru.UnitGroup |> UnitGroup.toString

        n +
        (vru.Variable 
        |> Variable.getValueRange
        |> ValueRange.toString) + " " + ug

    /// Type and functions to handle the `Dto` 
    /// data transfer type for a `VariableUnit`
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Dto =

        open Informedica.GenUtils.Lib
                    
        type Dto = 
            {
                Id: string
                Name: string
                Mapping: string
                UnitGroup: string
                Variable: VariableDto.Dto
            }

        let fromDto succ fail (dto: Dto) =
            let map  = Option.map
            let none = Option.none
            let bind = Option.bind
            let var  = dto.Variable

            let nm   = [dto.Id; dto.Name; dto.Mapping] |> Name.create 
            let vals = var.Vals |> Set.ofList
            let min  = var.Min  |> map  (Variable.ValueRange.createMin  var.MinIncl)
            let incr = var.Incr |> bind (Variable.ValueRange.createIncr Some none)
            let max  = var.Max  |> map  (Variable.ValueRange.createMax  var.MaxIncl)

            create succ fail nm dto.Variable.Unr vals min incr max

    /// Type and functions that represent a frequency
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Frequency =

        /// String representation of the type
        let name = "Freq"

        /// The `Frequency` `UnitGroup`
        let unitGroup = UnitGroups.count |> UnitGroup.perGroup Constants.timeGroup

        let unitGroupToString = unitGroup |> UnitGroup.toString

        /// Type that represents a frequency
        type Frequency = Frequency of VariableUnit

        /// Turn `Frequency` in a `VariableUnit`
        let toVarUnt (Frequency freq) = freq

        /// Set a `Frequency` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Frequency 

        /// Create a `Frequency` with a preset `Variable`
        let create succ fail nm unr vs min incr max = 
            create succ fail nm unr vs min incr max unitGroup 
            |> Frequency
        
        /// Create a `Frequency` with name **n**
        let frequency n = 
            let n = [name] |> List.append n |> Name.create
            createNew n unitGroup |> Frequency

        /// Turn a `Frequency` to a string
        let toString freq = 
            freq |> toVarUnt |> toString


    /// Type and functions that represent a time
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Time = 

        /// String representation of the type
        let name = "Time"

        /// The time `UnitGroup`
        let unitGroup = UnitGroups.time

        /// Type that represents a time
        type Time = Time of VariableUnit

        /// Turn `Time` in a `VariableUnit`
        let toVarUnt (Time time) = time

        /// Set a `Time` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Time 

        /// Create a `Time` with a preset `Variable`
        let create succ fail nm unr vs min incr max = 
            create succ fail nm unr vs min incr max unitGroup 
            |> Time
        
        /// Create a `Time` with name **n**
        let time n = 
            let n = [name] |> List.append n |> Name.create
            createNew n unitGroup |> Time

        /// Turn a `Time` to a string
        let toString tme = 
            tme |> toVarUnt |> toString

    /// Type and functions that represent a count
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Count =    
    
        /// String representation of the type
        let name = "Count" 

        /// The count `UnitGroup`
        let unitGroup = UnitGroups.count

        /// Type that represents a count
        type Count = Count of VariableUnit

        /// Turn `Count` in a `VariableUnit`
        let toVarUnt (Count qty) = qty

        /// Set a `Count` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Count 

        /// Create a `Count` with a preset `Variable`
        let create succ fail nm unr vs min incr max = 
            create succ fail nm unr vs min incr max unitGroup 
            |> Count
        
        /// Create a `Count` with name **n**
        let count n = 
            let n = [name] |> List.append n |> Name.create
            createNew n unitGroup |> Count

        /// Turn a `Count` to a string
        let toString qty = 
            qty |> toVarUnt |> toString


    /// Type and functions that represent a quantity
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Quantity =    
    
        /// String representation of the type
        let name = "Qty" 

        /// Type that represents a quantity
        type Quantity = Quantity of VariableUnit

        /// Turn `Quantity` in a `VariableUnit`
        let toVarUnt (Quantity qty) = qty

        /// Set a `Quantity` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Quantity 
        
        /// Create a `Quantity` with a preset `Variable`
        let create succ fail nm unr vs min incr max ung = 
            create succ fail nm unr vs min incr max ung 
            |> Quantity

        /// Create a `Quantity` with name **n**
        /// and `UnitGroup` **ung**
        let quantity n ung = 
            let u = ung |> UnitGroup.fromString
            let n = [name] |> List.append n |> Name.create
            createNew n u |> Quantity

        /// Set the name of the quantity `Variable` to **n**
        let setName n qty =
            let n = [n |> Name.toString; name] |> Name.create
            qty |> toVarUnt |> setName n |> Quantity

        /// Turn a `Quantity` to a string
        let toString qty = 
            qty |> toVarUnt |> toString


    /// Type and functions that represent a total,
    /// and a total is a quantity over a time period
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Total =
    
        /// String representation of the type of the type
        let name = "Total" 

        /// Create a `Total` `UnitGroup`
        let unitGroup ung = ung |> UnitGroup.fromString |> UnitGroup.perGroup Constants.timeGroup

        /// Type that represents a total
        type Total = Total of VariableUnit

        /// Turn `Total` in a `VariableUnit`
        let toVarUnt (Total tot) = tot

        /// Set a `Total` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Total 
        
        /// Create a `Total` with a preset `Variable`
        let create succ fail nm unr vs min incr max ung = 
            create succ fail nm unr vs min incr max (ung |> unitGroup) 
            |> Total
        
        /// Create a `Total` with name **n**
        /// and `UnitGroup` **ung**
        let total n ung = 
            let n = [name] |> List.append n |> Name.create
            ung |> unitGroup |> createNew n |> Total

        /// Set the name of the total `Variable` to **n**
        let setName n tot =
            let n = [n |> Name.toString; name] |> Name.create
            tot |> toVarUnt |> setName n |> Total

        /// Turn a `Total` to a string
        let toString tot = 
            tot |> toVarUnt |> toString


    /// Type and functions that represent a rate,
    /// and a rate is a quantity per time
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Rate =
    
        /// String representation of the type
        let name = "Rate" 

        /// Create a `Rate` `UnitGroup`
        let unitGroup ung = ung |> UnitGroup.fromString |> UnitGroup.perGroup Constants.timeGroup

        /// Type that represents a rate
        type Rate = Rate of VariableUnit

        /// Turn `Rate` in a `VariableUnit`
        let toVarUnt (Rate rate) = rate

        /// Set a `Rate` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Rate 

        /// Create a `Rate` with a preset `Variable`
        let create succ fail nm unr vs min incr max ung = 
            create succ fail nm unr vs min incr max (ung |> unitGroup) 
            |> Rate
                
        /// Create a `Rate` with name **n**
        /// and `UnitGroup` **ung**
        let rate n ung = 
            let n = [name] |> List.append n |> Name.create
            ung |> unitGroup |> createNew n |> Rate

        /// Set the name of the rate `Variable` to **n**
        let setName n rte =
            let n = [n |> Name.toString; name] |> Name.create
            rte |> toVarUnt |> setName n |> Rate

        /// Turn a `Rate` to a string
        let toString rte = 
            rte |> toVarUnt |> toString


    /// Type and functions that represent a concentration,
    /// and a concentration is a quantity per time
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Concentration =
    
        /// String representation of the type
        let name = "Conc" 

        /// Create a `Concentration` `UnitGroup`
        /// **ung1**\/**ung2**
        let unitGroup ung1 ung2 = ung1 |> UnitGroup.fromString |> UnitGroup.perGroup ung2

        /// Type that represents a concentration
        type Concentration = Concentration of VariableUnit

        /// Turn `Concentration` in a `VariableUnit`
        let toVarUnt (Concentration conc) = conc

        /// Set a `Concentration` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Concentration 
        
        /// Create a `Concentration` with a preset `Variable`
        let create succ fail nm unr vs min incr max ung1 ung2 = 
            create succ fail nm unr vs min incr max (unitGroup ung1 ung2) 
            |> Concentration
                
        /// Create a `Concentration` with name **n**
        /// and `UnitGroup` **ung1** per **ung2**
        let conc n ung1 ung2 = 
            let n = [name] |> List.append n |> Name.create
            createNew n (unitGroup ung1 ung2) |> Concentration

        /// Turn a `Concentration` to a string
        let toString cnc = 
            cnc |> toVarUnt |> toString


    /// Type and functions that represent a adjusted quantity,
    /// and a adjusted quantity is a quantity per time
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module QuantityAdjust =    
    
        /// String representation of the type
        let name = "QtyAdjust"

        /// `QuantityAdjust` `UnitGroup` 
        /// **ung* \/ **adj_ung**
        let unitGroup ung adj_ung = ung |> UnitGroup.fromString |> UnitGroup.perGroup adj_ung

        /// Type that represents a adjusted quantity
        type QuantityAdjust = QuantityAdjust of VariableUnit

        /// Turn `QuantityAdjust` in a `VariableUnit`
        let toVarUnt (QuantityAdjust qty) = qty

        /// Set a `QuantityAdjust` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt QuantityAdjust 

        /// Create a `QuantityAdjust` with a preset `Variable`
        let create succ fail nm unr vs min incr max ung adj_ung = 
            create succ fail nm unr vs min incr max (unitGroup ung adj_ung) 
            |> QuantityAdjust
        
        /// Create a `QuantityAdjust` with name **n**
        /// and `UnitGroup` **ung** adjusted by **adj_ung**
        let quantityAdjust n ung adj_ung = 
            let n = [name] |> List.append n |> Name.create
            createNew n (unitGroup ung adj_ung) |> QuantityAdjust

        /// Set the name of the quantity adjust `Variable` to **n**
        let setName n qty =
            let n = [n |> Name.toString; name] |> Name.create
            qty |> toVarUnt |> setName n |> QuantityAdjust

        /// Turn a `QuantityAdjust` to a string
        let toString qta = 
            qta |> toVarUnt |> toString


    /// Type and functions that represent a adjusted total,
    /// and a adjusted total is a quantity per time
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TotalAdjust =
    
        /// String representation of the type
        let name = "TotalAdjust" 

        /// `TotalAdjust` `UnitGroup` 
        /// **ung* \/ **adj_ung**
        let unitGroup ung adj_ung = 
            ung 
            |> UnitGroup.fromString 
            |> UnitGroup.perGroup adj_ung
            |> UnitGroup.perGroup Constants.timeGroup

        /// Type that represents a adjusted total
        type TotalAdjust = TotalAdjust of VariableUnit

        /// Turn `TotalAdjust` in a `VariableUnit`
        let toVarUnt (TotalAdjust tot) = tot

        /// Set a `TotalAdjust` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt TotalAdjust 

        /// Create a `TotalAdjust` with a preset `Variable`
        let create succ fail nm unr vs min incr max ung adj_ung = 
            create succ fail nm unr vs min incr max (unitGroup ung adj_ung) 
            |> TotalAdjust
                
        /// Create a `TotalAdjust` with name **n**
        /// and `UnitGroup` **ung** adjusted by **adj_ung**
        let totalAdjust n ung adj_ung = 
            let n = [name] |> List.append n |> Name.create
            createNew n (unitGroup ung adj_ung) |> TotalAdjust

        /// Set the name of the total adjust `Variable` to **n**
        let setName n tot =
            let n = [n |> Name.toString; name] |> Name.create
            tot |> toVarUnt |> setName n |> TotalAdjust

        /// Turn a `TotalAdjust` to a string
        let toString toa = 
            toa |> toVarUnt |> toString


    /// Type and functions that represent a adjusted rate,
    /// and a adjusted rate is a quantity per time unit
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RateAdjust =

        /// String representation of the type
        let name = "RateAdjust" 

        /// `RateAdjust` `UnitGroup` 
        /// **ung* \/ **adj_ung**
        let unitGroup ung adj_ung = 
            ung 
            |> UnitGroup.fromString 
            |> UnitGroup.perGroup adj_ung
            |> UnitGroup.perGroup Constants.timeGroup

        /// Type that represents a adjusted rate
        type RateAdjust = RateAdjust of VariableUnit

        /// Turn `RateAdjust` in a `VariableUnit`
        let toVarUnt (RateAdjust rate) = rate

        /// Set a `RateAdjust` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt RateAdjust 

        /// Create a `RateAdjust` with a preset `Variable`
        let create succ fail nm unr vs min incr max ung adj_ung = 
            create succ fail nm unr vs min incr max (unitGroup ung adj_ung) 
            |> RateAdjust
        
        /// Create a `RateAdjust` with name **n**
        /// and `UnitGroup` **ung** adjusted by **adj_ung**
        let rateAdjust n ung adj_ung = 
            let n = [name] |> List.append n |> Name.create
            createNew n (unitGroup ung adj_ung) |> RateAdjust

        /// Set the name of the rate adjust `Variable` to **n**
        let setName n rte =
            let n = [n |> Name.toString; name] |> Name.create
            rte |> toVarUnt |> setName n |> RateAdjust

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

        /// Create a `Dose` 
        let create qty tot rte = (qty, tot, rte) |> Dose

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

        /// Get the common name of a `Dose`
        let getName (Dose(qty, _, _)) =
            qty 
            |> QT.toVarUnt 
            |> getName 
            |> Name.toString 
            |> String.split "." 
            |> List.head

        /// Set the `Name` **n** to the dose `Variable`s
        let setName n (Dose(qty, tot, rte)) =
            (
                qty |> QT.setName n,
                tot |> TL.setName n,
                rte |> RT.setName n
            ) |> Dose
            

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

        /// Create a `DoseAdjust` 
        let create qty tot rte = (qty, tot, rte) |> DoseAdjust

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

        /// Get the common name of a `DoseAdjust`
        let getName (DoseAdjust(qty, _, _)) =
            qty 
            |> QT.toVarUnt 
            |> getName 
            |> Name.toString 
            |> String.split "." 
            |> List.head

        /// Set the `Name` **n** to the dose `Variable`s
        let setName n (DoseAdjust(qty, tot, rte)) =
            (
                qty |> QT.setName n,
                tot |> TL.setName n,
                rte |> RT.setName n
            ) |> DoseAdjust

        /// Turn a `DoseAdjust` to a string
        let toString (DoseAdjust(qty, tot, rte))  =
            [
                qty |> QT.toString
                tot |> TL.toString
                rte |> RT.toString
            ]

