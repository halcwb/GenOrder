namespace Informedica.GenOrder.Lib

/// Functions that deal with the `VariableUnit` type
module VariableUnit =

    open MathNet.Numerics

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib
    open WrappedString
    
    module Variable    = Informedica.GenSolver.Lib.Variable
    module VariableDto = Informedica.GenSolver.Lib.Dtos.Variable
    module Equation    = Informedica.GenSolver.Lib.Equation
    module ValueRange  = Variable.ValueRange
    module Units = ValueUnit.Units

    type Variable = Variable.Variable

    /// A `VariableUnit` is the combination of 
    /// an `Informedica.GenSolver.Lib.Variable` with
    /// an `Informedica.GenUnits.Lib.Unit`
    /// The `Variable` stores the base values according
    /// to the `Unit`
    type VariableUnit =
        {
            /// Stores the values/range
            Variable:  Variable
            /// Stores the unit group
            Unit: ValueUnit.Unit
        }  
         
    /// Create a new `VariableUnit` with
    /// `Name` **nm** and `Unit` **un**
    let createNew n un = 
        let var = 
            Variable.createSucc n ValueRange.unrestricted
            |> Variable.setNonZeroOrNegative
        { Variable = var; Unit = un } 
        
    /// Create a `VariableUnit` with preset values
    let create n vs min incr max un =
        let succ = Some
        let fail = Option.none

        ValueRange.create succ fail true vs min incr max
        |> function
        | Some vlr ->
            let var = Variable.create id n vlr        
            { Variable = var; Unit = un }
        | None -> createNew n un

    /// Create a `VariableUnit` with
    /// `Variable` **var** and `Unit` **un**
    let withVar un var =    
        { Variable = var; Unit = un } 

    /// Apply **f** to `VariableUnit` **vru**
    let apply f (vru: VariableUnit) = vru |> f

    /// Utitlity function to facitlitate type inference
    let get = apply id

    /// Get all record fiels from a `VariableUnit`
    let getAll { Variable = var; Unit = un } =
        var, un

    /// Get the `Variable` from a `VariableUnit`
    let getVar = apply (fun vu -> vu.Variable)

    /// Get the `Variable.Name` from a `VariableUnit` **vru**
    let getName vru = (vru |> getVar).Name

    /// Get the `Unit` from a `VariableUnit`
    let getUnit = apply (fun vu -> vu.Unit)

    /// Create an `Equation` using a constructor **cr**
    /// a result `VariableUnit` **y** and a list of 
    /// `VariableUnit` list **xs**
    let toEq cr y xs = (y |> getVar, xs |> List.map getVar) |> cr

    /// Create a `ProdEquation` from `VariableUnit`s
    let toProdEq succ fail = toEq (Equation.createProductEq succ fail)

    /// Create a `SumEquation` from `VariableUnit`s
    let toSumEq succ fail = toEq (Equation.createSumEq succ fail)

    /// Set a property **p** of a `VariableUnit` **vru** 
    /// with values **vs** in a `Equation` list
    let setProp vru p vs eqs = eqs |> Solver.solve (vru |> getName) p vs

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
        let ns = vru |> getName |> Variable.Name.toString
        let us = vru.Unit |> ValueUnit.unitToString

        ns +
        (vru.Variable 
        |> Variable.getValueRange
        |> ValueRange.toString) + " " + us

    let getUnits vu =
        (vu |> get).Unit
        |> ValueUnit.getUnits []

    /// Helper functions for `Informedica.GenSolver.Variable.Name` type
    module Name =
     
        module N = Informedica.GenSolver.Lib.Variable.Name

        type Name = N.Name

        /// Create a `Name` from a list of strings that 
        let create ns = ns |> String.concat "." |> N.createExc

        let toString = N.toString

    /// Type and functions to handle the `Dto` 
    /// data transfer type for a `VariableUnit`
    module Dto =

        module ValueRange = Variable.ValueRange
                    
        type Dto () = 
            member val Name = "" with get, set
            member val Unit = "" with get, set
            member val Vals : BigRational list = [] with get, set
            member val Min : BigRational option = None with get, set
            member val MinIncl = false with get, set
            member val Incr : BigRational option = None with get, set
            member val Max : BigRational option = None with get, set
            member val MaxIncl = false with get, set 

        let dto () = Dto ()

        let fromDto (dto: Dto) =
            let map  = Option.map
            let none = Option.none
            let bind = Option.bind

            let n    = [ dto.Name ] |> Name.create 
            let vals = dto.Vals |> Set.ofList
            let min  = dto.Min  |> map  (ValueRange.createMin  dto.MinIncl)
            let incr = dto.Incr |> bind (ValueRange.createIncr Some none)
            let max  = dto.Max  |> map  (ValueRange.createMax  dto.MaxIncl)

            let un =
                if dto.Unit |> String.isNullOrWhiteSpace then 
                    ValueUnit.NoUnit
                else
                    dto.Unit
                    |> ValueUnit.unitFromString
                    |> function
                    | Some u -> u
                    | None -> ValueUnit.NoUnit

            create n vals min incr max un

        let toDto vu =
            let dto = dto ()
            let vr =
                vu 
                |> getVar 
                |> Variable.getValueRange
            let min, inclMin = 
                vr 
                |> ValueRange.getMin
                |> function 
                | Some m -> 
                    m 
                    |> ValueRange.minToValue
                    |> Some, m |> ValueRange.isMinIncl
                | None -> None, false
            let max, inclMax = 
                vr 
                |> ValueRange.getMax
                |> function 
                | Some m -> 
                    m 
                    |> ValueRange.maxToValue
                    |> Some, 
                    m |> ValueRange.isMaxIncl
                | None -> None, false

            dto.Name <- 
                vu |> getName |> Name.toString
            dto.Unit <-
                vu |> getUnit |> ValueUnit.unitToString
            dto.Vals <-
                vr
                |> ValueRange.getValueSet
                |> Set.toList
            dto.Incr <-
                vr
                |> ValueRange.getIncr
                |> Option.bind (ValueRange.incrToValue >> Some)
            dto.Min <- min
            dto.MinIncl <- inclMin
            dto.Max <- max
            dto.MaxIncl <- inclMax

            dto


    /// Type and functions that represent a frequency
    module Frequency =

        /// String representation of the type
        let name = "Freq"

        /// Type that represents a frequency
        type Frequency = Frequency of VariableUnit

        /// Turn `Frequency` in a `VariableUnit`
        let toVarUnt (Frequency freq) = freq

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits = toVarUnt >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> Frequency

        /// Set a `Frequency` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Frequency 

        /// Create a `Frequency` with a preset `Variable`
        let create n vs min incr max un = 
            create n vs min incr max un 
            |> Frequency
        
        /// Create a `Frequency` with name **n**
        /// with `Unit` time unit **tu**
        let frequency n tu = 
            let n = [name] |> List.append n |> Name.create

            match tu with 
            | ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ -> 
                Units.Count.times
                |> ValueUnit.per tu
            |> createNew n 
            |> Frequency

        /// Turn a `Frequency` to a string
        let toString = toVarUnt >> toString


    /// Type and functions that represent a time
    module Time = 

        /// String representation of the type
        let name = "Time"

        /// Type that represents a time
        type Time = Time of VariableUnit

        /// Turn `Time` in a `VariableUnit`
        let toVarUnt (Time time) = time

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits = toVarUnt >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> Time
        
        /// Set a `Time` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Time 

        /// Create a `Time` with a preset `Variable`
        let create n vs min incr max un = 
            create n vs min incr max un 
            |> Time
            
        /// Create a `Time` with name **n**
        /// with `Unit` **un**
        let time n un = 
            let n = [name] |> List.append n |> Name.create
            
            createNew n un 
            |> Time

        /// Turn a `Time` to a string
        let toString = toVarUnt >> toString

    /// Type and functions that represent a count
    module Count =    
    
        /// String representation of the type
        let name = "Count" 

        /// Type that represents a count
        type Count = Count of VariableUnit

        /// Turn `Count` in a `VariableUnit`
        let toVarUnt (Count qty) = qty

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits = toVarUnt >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> Count
        
        /// Set a `Count` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Count 

        /// Create a `Count` with a preset `Variable`
        let create n vs min incr max un = 
            create n vs min incr max un 
            |> Count
            
        /// Create a `Count` with name **n**
        let count n = 
            let n = [name] |> List.append n |> Name.create
            let un = Units.Count.times
            createNew n un |> Count

        /// Turn a `Count` to a string
        let toString = toVarUnt >> toString


    /// Type and functions that represent a quantity
    module Quantity =    
    
        /// String representation of the type
        let name = "Qty" 

        /// Type that represents a quantity
        type Quantity = Quantity of VariableUnit

        /// Turn `Quantity` in a `VariableUnit`
        let toVarUnt (Quantity qty) = qty

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits
        
        let toDto = toVarUnt >> Dto.toDto
        
        let fromDto dto = dto |> Dto.fromDto |> Quantity

        /// Set a `Quantity` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Quantity 
        
        /// Create a `Quantity` with a preset `Variable`
        let create n vs min incr max un = 
            create n vs min incr max un 
            |> Quantity

        /// Create a `Quantity` with name **n**
        /// and `Unit` **un**
        let quantity n un = 
            let n = [name] |> List.append n |> Name.create
            
            createNew n un 
            |> Quantity 

        /// Set the name of the quantity `Variable` to **n**
        let setName n qty =
            let n = [n |> Name.toString; name] |> Name.create
            qty |> toVarUnt |> setName n |> Quantity

        /// Turn a `Quantity` to a string
        let toString = toVarUnt >> toString


    /// Type and functions that represent a total,
    /// and a total is a quantity over a time period
    module Total =
    
        /// String representation of the type of the type
        let name = "Total" 

        /// Type that represents a total
        type Total = Total of VariableUnit

        /// Turn `Total` in a `VariableUnit`
        let toVarUnt (Total tot) = tot

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> Total

        /// Set a `Total` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Total 
        
        /// Create a `Total` with a preset `Variable`
        let create n vs min incr max un = 
            create n vs min incr max un 
            |> Total
        
        /// Create a `Total` with name **n**
        /// and `Unit` **un** and time unit **tu**
        let total n un tu = 
            let n = [name] |> List.append n |> Name.create
            
            match un with
            | ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un 
                |> ValueUnit.per tu
            |> createNew n 
            |> Total

        /// Set the name of the total `Variable` to **n**
        let setName n tot =
            let n = [n |> Name.toString; name] |> Name.create
            tot |> toVarUnt |> setName n |> Total

        /// Turn a `Total` to a string
        let toString = toVarUnt >> toString


    /// Type and functions that represent a rate,
    /// and a rate is a quantity per time
    module Rate =
    
        /// String representation of the type
        let name = "Rate" 

        /// Type that represents a rate
        type Rate = Rate of VariableUnit

        /// Turn `Rate` in a `VariableUnit`
        let toVarUnt (Rate rate) = rate

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> Rate

        /// Set a `Rate` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Rate 

        /// Create a `Rate` with a preset `Variable`
        let create n vs min incr max un = 
            create n vs min incr max un 
            |> Rate
                
        /// Create a `Rate` with name **n**
        /// and `Unit` **un** per time unit **tu**
        let rate n un1 un2 = 
            let n = [name] |> List.append n |> Name.create
            
            match un1, un2 with
            | ValueUnit.NoUnit, _ 
            | _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un1
                |> ValueUnit.per un2
            |> createNew n 
            |> Rate

        /// Set the name of the rate `Variable` to **n**
        let setName n rte =
            let n = [n |> Name.toString; name] |> Name.create
            rte |> toVarUnt |> setName n |> Rate

        /// Turn a `Rate` to a string
        let toString = toVarUnt >> toString


    /// Type and functions that represent a concentration,
    /// and a concentration is a quantity per time
    module Concentration =
    
        /// String representation of the type
        let name = "Conc" 

        /// Type that represents a concentration
        type Concentration = Concentration of VariableUnit

        /// Turn `Concentration` in a `VariableUnit`
        let toVarUnt (Concentration conc) = conc

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> Concentration

        /// Set a `Concentration` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt Concentration 
        
        /// Create a `Concentration` with a preset `Variable`
        let create n vs min incr max un = 
            un
            |> create n vs min incr max
            |> Concentration
                
        /// Create a `Concentration` with name **n**
        /// and `Unit` **un** per shape unit **su**
        let conc n un su = 
            let n = [name] |> List.append n |> Name.create
            
            match un, su with
            | ValueUnit.NoUnit, _
            | _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per su
            |> createNew n
            |> Concentration

        /// Turn a `Concentration` to a string
        let toString = toVarUnt >> toString


    /// Type and functions that represent a adjusted quantity,
    /// and a adjusted quantity is a quantity per time
    module QuantityAdjust =    
    
        /// String representation of the type
        let name = "QtyAdjust"

        /// Type that represents a adjusted quantity
        type QuantityAdjust = QuantityAdjust of VariableUnit

        /// Turn `QuantityAdjust` in a `VariableUnit`
        let toVarUnt (QuantityAdjust qty) = qty

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> QuantityAdjust

        /// Set a `QuantityAdjust` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt QuantityAdjust 

        /// Create a `QuantityAdjust` with a preset `Variable`
        let create n vs min incr max un = 
            un
            |> create n vs min incr max  
            |> QuantityAdjust
        
        /// Create a `QuantityAdjust` with name **n**
        /// and `Unit` **un** per adjust **adj**
        let quantityAdjust n un adj = 
            let n = [name] |> List.append n |> Name.create

            match un, adj with
            | ValueUnit.NoUnit, _ 
            | _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per adj
            |> createNew n 
            |> QuantityAdjust

        /// Set the name of the quantity adjust `Variable` to **n**
        let setName n qty =
            let n = [n |> Name.toString; name] |> Name.create
            qty |> toVarUnt |> setName n |> QuantityAdjust

        /// Turn a `QuantityAdjust` to a string
        let toString = toVarUnt >> toString


    /// Type and functions that represent a adjusted total,
    /// and a adjusted total is a quantity per time
    module TotalAdjust =
    
        /// String representation of the type
        let name = "TotalAdjust" 

        /// Type that represents a adjusted total
        type TotalAdjust = TotalAdjust of VariableUnit

        /// Turn `TotalAdjust` in a `VariableUnit`
        let toVarUnt (TotalAdjust tot) = tot

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits

        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> TotalAdjust

        /// Set a `TotalAdjust` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt TotalAdjust 

        /// Create a `TotalAdjust` with a preset `Variable`
        let create n vs min incr max un = 
            un 
            |> create n vs min incr max  
            |> TotalAdjust
                
        /// Create a `TotalAdjust` with name **n**
        /// and `Unit` **un** per adjust **adj** per time unit **tu**
        let totalAdjust n un adj tu = 
            let n = [name] |> List.append n |> Name.create

            match un, adj, tu with
            | ValueUnit.NoUnit, _, _ 
            | _, ValueUnit.NoUnit, _ 
            | _, _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per adj
                |> ValueUnit.per tu
            |> createNew n 
            |> TotalAdjust

        /// Set the name of the total adjust `Variable` to **n**
        let setName n tot =
            let n = [n |> Name.toString; name] |> Name.create
            tot |> toVarUnt |> setName n |> TotalAdjust

        /// Turn a `TotalAdjust` to a string
        let toString = toVarUnt >> toString


    /// Type and functions that represent a adjusted rate,
    /// and a adjusted rate is a quantity per time unit
    module RateAdjust =

        /// String representation of the type
        let name = "RateAdjust" 

        /// Type that represents a adjusted rate
        type RateAdjust = RateAdjust of VariableUnit

        /// Turn `RateAdjust` in a `VariableUnit`
        let toVarUnt (RateAdjust rate) = rate

        /// Set a `RateAdjust` with a `Variable`
        /// in a list of `Variable` lists
        let fromVar = fromVar toVarUnt RateAdjust 

        let unitToString =
            toVarUnt
            >> getUnit
            >> ValueUnit.unitToString

        let getUnits =
            toVarUnt
            >> getUnits
        
        let toDto = toVarUnt >> Dto.toDto

        let fromDto dto = dto |> Dto.fromDto |> RateAdjust

        /// Create a `RateAdjust` with a preset `Variable`
        let create n vs min incr max un =
            un
            |> create n vs min incr max
            |> RateAdjust
        
        /// Create a `RateAdjust` with name **n**
        /// and `Unit` **un** per adjust **adj** per time unit **tu**
        let rateAdjust n un adj tu = 
            let n = [name] |> List.append n |> Name.create

            match un, adj, tu with
            | ValueUnit.NoUnit, _, _ 
            | _, ValueUnit.NoUnit, _ 
            | _, _, ValueUnit.NoUnit -> ValueUnit.NoUnit
            | _ ->
                un
                |> ValueUnit.per adj
                |> ValueUnit.per tu
            |> createNew n 
            |> RateAdjust

        /// Set the name of the rate adjust `Variable` to **n**
        let setName n rte =
            let n = [n |> Name.toString; name] |> Name.create
            rte 
            |> toVarUnt 
            |> setName n 
            |> RateAdjust

        /// Turn a `RateAdjust` to a string
        let toString = toVarUnt >> toString


    /// Type and functions that represent a dose,
    /// and a dose is a dose quantity, total and rate
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

        let toDto = 
            toVarUnt
            >> (fun (q, t, r) -> q |> Dto.toDto, t |> Dto.toDto, r |> Dto.toDto)

        let fromDto (q, t, r) =
            (q |> Dto.fromDto |> QT.Quantity ,
             t |> Dto.fromDto |> TL.Total ,
             r |> Dto.fromDto |> RT.Rate) |> Dose

        /// Set a `Dose` with a quantity, total and rate `Variable` 
        /// in a list of `Variable` lists
        let fromVar eqs (Dose(qty, tot, rte)) = 
            let qty = fromVar QT.toVarUnt QT.Quantity eqs qty
            let tot = fromVar TL.toVarUnt TL.Total    eqs tot
            let rte = fromVar RT.toVarUnt RT.Rate     eqs rte
            (qty, tot, rte) |> Dose

        /// Create a `Dose` with name **n**
        /// and `Unit` **un** per time unit **tu**
        let dose n un ttu rtu = 
            let qty   = QT.quantity n un
            let total = TL.total    n un ttu
            let rate  = RT.rate     n un rtu

            (qty, total, rate) 
            |> Dose 

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

        let toDto = 
            toVarUnt
            >> (fun (q, t, r) -> q |> Dto.toDto, t |> Dto.toDto, r |> Dto.toDto)

        let fromDto (q, t, r) =
            (q |> Dto.fromDto |> QT.QuantityAdjust ,
             t |> Dto.fromDto |> TL.TotalAdjust ,
             r |> Dto.fromDto |> RT.RateAdjust) |> DoseAdjust

        /// Set a `DoseAdjust` with an adjusted quantity, total and rate `Variable` 
        /// in a list of `Variable` lists
        let fromVar eqs (DoseAdjust(qty, tot, rte)) = 
            let qty = fromVar QT.toVarUnt QT.QuantityAdjust eqs qty
            let tot = fromVar TL.toVarUnt TL.TotalAdjust    eqs tot
            let rte = fromVar RT.toVarUnt RT.RateAdjust     eqs rte
            (qty, tot, rte) |> DoseAdjust

        /// Create a `DoseAdjust` with name **n**
        /// and `Unit` **un** per adjust **adj** per time unit **tu**
        let doseAdjust n un adj ttu rtu = 
            let qty   = QT.quantityAdjust n un adj
            let total = TL.totalAdjust    n un adj ttu
            let rate  = RT.rateAdjust     n un adj rtu

            (qty, total, rate) 
            |> DoseAdjust

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

