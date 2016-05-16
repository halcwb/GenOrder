#load "load-project-release.fsx"

open Informedica.GenOrder.Lib

#time

module Solver =

    open Informedica.GenUtils.Lib

    module N = Informedica.GenSolver.Lib.Variable.Name
    module SV = Informedica.GenSolver.Api
    module UN = Informedica.GenUnits.Lib.CombiUnit
    
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

    type Prop = 
        | Vals
        | MinIncl
        | MinExcl
        | Incr
        | MaxIncl
        | MaxExcl

    let propToString = function
        | Vals -> vals
        | MinIncl -> minincl
        | MinExcl -> minexcl
        | Incr -> incr
        | MaxIncl -> maxincl
        | MaxExcl -> maxexcl

    let valsToString u vs = 
        vs 
        |> List.map (UN.toBase u)
        |> List.map BigRational.toString 
        |> String.concat ", "
    
    let solve (N.Name n) p vs u = SV.solve id n (p |> propToString) (vs |> valsToString u)
    
 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Unit = 
    
    module UN = Informedica.GenUnits.Lib.CombiUnit

    let freqUnit = "1 X/day" |> UN.fromString
    let timeUnit = "1 hour"  |> UN.fromString
    let qtyUnit s = s |> UN.fromString
    let totalUnit s = s + "/day"  |> UN.fromString
    let rateUnit s  = s + "/hour" |> UN.fromString

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

    let apply f (qty: VariableUnit) = qty |> f

    let get = apply id

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
    module Frequency =

        module N = VAR.Name

        let name = "Freq" |> N.createExc

        type Frequency = Frequency of VariableUnit

        let frequency = 
            let u = Unit.freqUnit |> Some
            let n = name
            create n u u u u |> Frequency


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Time = 

        module N = VAR.Name

        let name = "Time" |> N.createExc

        type Time = Time of VariableUnit

        let time = 
            let u = Unit.timeUnit |> Some
            let n = name
            create n u u u u |> Time


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Quantity =    
    
        module N = VAR.Name

        let name = "Qty" |> N.createExc

        type Quantity = Quantity of VariableUnit

        let quantity u = 
            let u = u |> Unit.qtyUnit |> Some
            let n = name
            create n u u u u |> Quantity


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Total =
    
        module N = VAR.Name

        let name = "Total" |> N.createExc

        type Total = Total of VariableUnit

        let total u = 
            let u = u |> Unit.totalUnit |> Some
            let n = name
            create n u u u u |> Total


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Rate =
    
        module N = VAR.Name

        let name = "Rate" |> N.createExc

        type Rate = Rate of VariableUnit

        let rate u = 
            let u = u |> Unit.rateUnit |> Some
            let n = name
            create n u u u u |> Rate

//
//    module Concentration =
//
//        type T = Concentration of VR.T with
//            interface IQuantity with 
//                member this.GetVariable = let (Concentration v) = this in v
//
//        let create vs u1 u2 =
//            let cr u1 u2 = 
//                vs 
//                |> VR.withUnit u1 
//                |> VR.per 1N u2 
//                |> create Concentration
//        
//            cr 
//            <!> UN.Units.createFromString u1
//            <*> UN.Units.createFromString u2
//            |> Result.mapMessagesR Messages.map
//            >>= Validation.onlyPositiveNum vs
//
//        let conc v = create v
//        let per v u f = f v u
//
//        let empty () = VR.emptyVar() |> Concentration
//
//        let setMin min = min |> setMin |> set<T>
//        let setMax max = max |> setMax |>  set<T>
//        let setIncr incr = incr |> setIncr |> set<T> 
//        let setValue v = v |> setValue |> set<T>
//
//
//    module QuantityAdjust =
//
//        type T = QuantityAdjust of VR.T with
//            interface IQuantity with 
//                member this.GetVariable = let (QuantityAdjust v) = this in v 
//
//        let create vs u v au = 
//            let cr u a = 
//                vs 
//                |> VR.withUnit u
//                |> VR.per v a
//                |> create QuantityAdjust
//
//            cr 
//            <!> UN.Units.fromString u
//            <*> UN.Units.adjustFromString au
//            |> Result.mapMessagesR Messages.map
//            >>= Validation.onlyPositiveNum vs
//            >>= Validation.onlyPositiveNum [v]
//
//        let quantity v = create v
//        let adjustBy v u f = f v u
//
//        let empty () = VR.emptyVar() |> QuantityAdjust
//
//        let setMin min = min |> setMin |> set<T>
//        let setMax max = max |> setMax |>  set<T>
//        let setIncr incr = incr |> setIncr |> set<T> 
//        let setValue v = v |> setValue |> set<T>
//
//
//    module TotalAdjust =
//
//        type T = TotalAdjust of VR.T with
//            interface IQuantity with 
//                member this.GetVariable = let (TotalAdjust v) = this in v
//
//        let create vs u av au tv tu = 
//            let cr u au tu = 
//                vs 
//                |> VR.withUnit u
//                |> VR.per av au
//                |> VR.per tv tu
//                |> create TotalAdjust
//
//            cr 
//            <!> UN.Units.fromString u
//            <*> UN.Units.adjustFromString au  
//            <*> UN.Units.timeFromString tu 
//            |> Result.mapMessagesR Messages.map
//            >>= Validation.onlyPositiveNum vs
//            >>= Validation.onlyPositiveNum [av]
//            >>= Validation.onlyPositiveNum [tv]
//
//        let doseTot v = create v
//
//        let total v = create v
//        let adjustBy v u f = f v u
//        let per v u f = f v u
//
//        let empty () = VR.emptyVar() |> TotalAdjust
//
//        let setMin min = min |> setMin |> set<T>
//        let setMax max = max |> setMax |>  set<T>
//        let setIncr incr = incr |> setIncr |> set<T> 
//        let setValue v = v |> setValue |> set<T>
//
//
//    module Rate =
//
//        type T = Rate of VR.T with
//            interface IQuantity with 
//                member this.GetVariable = let (Rate v) = this in v
//
//        let create vs u v tu = 
//            let cr u tu = 
//                vs 
//                |> VR.withUnit u
//                |> VR.per v tu
//                |> create Rate
//
//            cr 
//            <!> UN.Units.fromString u
//            <*> UN.Units.timeFromString tu
//            |> Result.mapMessagesR Messages.map
//            >>= Validation.onlyPositiveNum vs
//            >>= Validation.onlyPositiveNum [v]
//
//        let rate v = create v
//        let per v u f = f v u
//
//        let empty () = VR.emptyVar() |> Rate
//
//        let setMin min = min |> setMin |> set<T>
//        let setMax max = max |> setMax |>  set<T>
//        let setIncr incr = incr |> setIncr |> set<T> 
//        let setValue v = v |> setValue |> set<T>
//
//
//    module RateAdjust =
//
//        type T = RateAdjust of VR.T with
//            interface IQuantity with 
//                member this.GetVariable = let (RateAdjust v) = this in v
//
//        let create vs u av au tv tu = 
//            let cr u au tu = 
//                vs 
//                |> VR.withUnit u
//                |> VR.per av au
//                |> VR.per tv tu
//                |> create RateAdjust
//
//            cr 
//            <!> UN.Units.fromString u
//            <*> UN.Units.adjustFromString au
//            <*> UN.Units.timeFromString tu
//            |> Result.mapMessagesR Messages.map
//            >>= Validation.onlyPositiveNum vs
//            >>= Validation.onlyPositiveNum [av]
//            >>= Validation.onlyPositiveNum [tv]
//
//
//        let doseRate v = create v
//
//        let doseTotAdj v = create v
//        let adjustBy v u (f: BigRational -> string -> BigRational -> string -> T Option) = f v u
//
//        let per v u (f: BigRational -> string -> T Option) = f v u
//
//        let empty () = VR.emptyVar() |> RateAdjust
//
//        let setMin min = min |> setMin |> set<T>
//        let setMax max = max |> setMax |>  set<T>
//        let setIncr incr = incr |> setIncr |> set<T> 
//        let setValue v = v |> setValue |> set<T>
//
//
//module Product =
//
//    module VR = Variable
//    module QT = Quantities
//    module RW = Railway
//    module WS = WrappedString
//
//    let (>>=) = Result.bindL
//    let (<!>) = Result.liftR
//    let (<*>) = Result.applyR
//
//    let mapToTuple = RW.mapToTuple
//    let mapToTriple = RW.mapToTriple
//
//    let set x = fun _ -> x 
//
//    module Messages = 
//
//        type T = 
//            | CreatedGenericName of string
//            | CannotCreateGenericName of string
//            | SubstanceConcentrationShouldBeGreaterThan0
//            | CannotCreateQuantity of string
//            | CreatedUnit of string
//
//    module Substance = 
//
//        module Dto =
//             
//            type T = Informedica.GenPres.Contracts.Data.ProductSubstance
//
//            let create () = new T()
//
//        module GenericName =
//
//            type T = GenericName of WS.StringMinMax.T
//
//            let create s = 
//
//                let map = function
//                    | WS.Messages.CreatedString _ -> 
//                        Messages.CreatedGenericName s
//                    | WS.Messages.StringIsEmpty -> 
//                        Messages.CannotCreateGenericName "empty string"
//                    | WS.Messages.StringIsNull -> 
//                        Messages.CannotCreateGenericName "null value"
//                    | WS.Messages.StringLargerThan n -> 
//                        Messages.CannotCreateGenericName ("string larger than " + string n)
//                    | WS.Messages.StringSmallerThan n -> 
//                        Messages.CannotCreateGenericName ("string is smaller than " + string n)
//
//                let toLower s = s |> WS.StringMinMax.apply StringBCL.toLower |> Result.succeed
//
//                s 
//                |> WS.StringMinMax.create 2 30
//                >>= toLower
//                |> Result.mapMessagesR map
//                >>= (GenericName >> Result.succeed)
//
//            let apply f (GenericName n) = n |> WS.apply f
//
//            let get = apply id
//            let change f x = x |> apply f |> create
//                
//
//
//        module GenericNameTests =
//
//            open Swensen.Unquote
//            
//            // A generic name cannot be created with an empty string
//            test <@ GenericName.create "" |> Result.isFailure @>
//
//            // A generic name cannot be created with a null string
//            test<@  GenericName.create null |> Result.isFailure @>
//
//            // A generic name cannot be created with a string smaller than 2 characters
//            test<@ GenericName.create "1" |> Result.isFailure @>
//            test<@ GenericName.create "12" |> Result.isSuccess @>
//
//            // A generic name cannot be created with a string larger than 30 characters
//            test<@  [ for _ in [1..31] do yield "s" ] |> List.fold (+) "" |> GenericName.create |> Result.isFailure @>
//            test<@  [ for _ in [1..30] do yield "s" ] |> List.fold (+) "" |> GenericName.create |> Result.isSuccess @>
//
//            // A generic name is all lower case
//            test<@ GenericName.create "LOWERCASE" 
//                   |> Result.either (fun (gn, _) -> gn |> GenericName.get = "lowercase")
//                                    (fun _ -> false)  @>
//
//        type T =
//            { 
//                Name: GenericName.T
//                Quantity: QT.Quantity.T
//                Concentration: QT.Concentration.T 
//            } 
//
//        let apply f (x: T) = x |> f
//
//        let create name qty conc = 
//                { 
//                    Name = name 
//                    Quantity = qty
//                    Concentration = conc
//                } 
//
//        let dtoToSubstance (dto: Dto.T) =
//
//            let map = function 
//            | QT.Messages.CouldNotCreateUnit -> Messages.CannotCreateQuantity "cannot create unit"
//            | QT.Messages.CouldNotFindUnit s -> Messages.CannotCreateQuantity ("cannot find unit: " + s)
//            | QT.Messages.NotATimeUnit s -> Messages.CannotCreateQuantity ("unit " + s + " is not a time unit")
//            | QT.Messages.OnlyPostivesAllowed -> Messages.CannotCreateQuantity "negative or zero numbers"
//            | QT.Messages.CreatedUnit u -> u |> Messages.CreatedUnit
//
//            let conc = dto.Concentration |> Informedica.Utilities.Math.floatToBigN
//
//            let quantityOrFail = 
//                QT.Quantity.create [] dto.Unit
//                |> Result.mapMessagesR map
//
//            let concOrFail =
//                QT.Concentration.create [conc] dto.Unit dto.Adjust
//                |> Result.mapMessagesR map
//
//            create
//            <!> GenericName.create dto.Name
//            <*> quantityOrFail
//            <*> concOrFail
//
//        let getName = apply (fun s -> s.Name)
//        let getQuantity = apply (fun s -> s.Quantity)
//        let getConcentration = apply (fun s -> s.Concentration)
//
//        let applyQuantity k = apply (fun s -> { s with Quantity = s.Quantity |> k })
//        let applyConcentration k = apply (fun s -> { s with Concentration = s.Concentration |> k })
//
//        let setQuantity x = x |> set |> applyQuantity
//        let setConcentration = set >> applyConcentration
//
//        let setQuantityValue x = x |>  QT.Quantity.setValue |> applyQuantity
//        let setConcentrationValue x = x |> QT.Concentration.setValue |> applyConcentration
//
//        let setQuantityIncr x = x |> QT.Quantity.setIncr |> applyQuantity
//        let setConcentrationIncr x = x |> QT.Concentration.setIncr |> applyConcentration
//
//        let setQuantityMin x = x |> QT.Quantity.setMin |> applyQuantity
//        let setConcentrationMin x = x |> QT.Concentration.setMin |> applyConcentration
//
//        let setQuantityMax x = x |> QT.Quantity.setMax |> applyQuantity
//        let setConcentrationMax x = x |> QT.Concentration.setMax |> applyConcentration
//
//
//    type T = 
//        { 
//            Name: string; 
//            Quantity: QT.Quantity.T 
//            Substances: Substance.T * Substance.T list
//        } 
//         
//    let apply f (x: T) = x |> f
//
//    let create n qty subst substs = 
//            { 
//                Name = n 
//                Quantity = qty
//                Substances = subst, substs
//            } 
//
//
//    let getName = apply (fun s -> s.Name)
//    let getQuantity = apply (fun s -> s.Quantity)
//
//    let applyQuantity k = apply (fun s -> { s with Quantity = s.Quantity |> k })
//
//    let setQuantity x = x |> set |> applyQuantity 
//
//    let setQuantityValue x = x |>  QT.Quantity.setValue |> applyQuantity 
//
//    let setQuantityIncr x = x |> QT.setIncr |> applyQuantity
//
//    let setQuantityMin x = x |> QT.setMin |> applyQuantity
//
//    let setQuantityMax x = x |> QT.setMax |> applyQuantity
//
//    let createProduct name vs un sq sun = 
//        let crtSubst = Substance.createSubst name sq sun vs
//        let crt (qty, subst) = 
//            create name qty subst [] |> RW.succeed
//
//        QT.Quantity.create vs un
//        |> mapToTuple crtSubst sun
//        >>= crt
//        
// 
//module Prescription =
//    
//    module WS = WrappedString
//    module VR = Variable
//    module UN = Unit
//    module QT = Quantities
//    module EQ = Equation
//
//
//    let (>>=) m k = Option.bind k m
//    let lift f x = x |> f |> Some
//    let liftp p m = if p then m |> Some else None
//
//    type T =
//        | PRN of Prescription
//        | NonPRN of Prescription 
//    and Prescription = 
//        | Process
//        | Continuous
//        | Discontinuous of QT.Frequency.T
//        | Timed of QT.Frequency.T * QT.Time.T
//
//    let getPrescription = function | PRN p | NonPRN p -> p
//
//    let createProcess = Process |> Some
//
//    let createContinuous () = Continuous
//
//    let createDiscontinous f = f |> Discontinuous
//
//    let createTimed f t =  (f, t) |> Timed
//
//    let toPRN = PRN
//    let toNonPRN = NonPRN
//    
//    let apply f = function | PRN p | NonPRN p -> p |> f
//
//
//module Orderable =
//
//    module UN = Unit
//    module VR = Variable
//    module EQ = Equation
//    module SV = Solver
//    module QT = Quantities
//    module PN = Prescription
//
//    let (>>=) m k = Option.bind k m
//    
//    let set x = fun _ -> x
//
//    type Name = Name of string
//
//    module Dose = 
//
//        module Adjust =
//
//            type T = 
//            | Weight of QT.Quantity.T
//            | Surface of QT.Quantity.T
//            | Length of QT.Quantity.T
//            | NoAdjust
//
//            let createWeightAdjust = Weight
//            let createSurfaceAdjust = Surface
//            let createLengthAdjust = Length
//            let createNoAdjust = NoAdjust
//
//        type T = 
//            {
//                AdminQuantity: QT.Quantity.T 
//                AdminTotal: QT.Total.T 
//                AdminRate: QT.Rate.T 
//                DoseQuantity: QT.QuantityAdjust.T 
//                DoseTotal: QT.TotalAdjust.T 
//                DoseRate: QT.RateAdjust.T                     
//            }
//
//        let create qt tot rate dqt dtot drate =
//            {
//                AdminQuantity = qt 
//                AdminTotal = tot 
//                AdminRate = rate 
//                DoseQuantity = dqt 
//                DoseTotal = dtot 
//                DoseRate = drate                     
//            } 
//
//        let apply f (x: T) = x |> f
//
//        let getAdminQuantity = apply (fun d -> d.AdminQuantity)
//        let getAdminTotal = apply (fun d -> d.AdminTotal)
//        let getAdminRate = apply (fun d -> d.AdminRate)
//        let getDoseQuantity = apply (fun d -> d.DoseQuantity)
//        let getDoseTotal = apply (fun d -> d.DoseTotal)
//        let getDoseRate = apply (fun d -> d.DoseRate)
//
//        let applyAdminQuantity k = apply (fun dose -> { dose with AdminQuantity = dose.AdminQuantity |> k })
//        let applyAdminTotal k = apply (fun dose -> { dose with AdminTotal = dose.AdminTotal |> k })
//        let applyAdminRate k = apply (fun dose -> { dose with AdminRate = dose.AdminRate |> k })
//        let applyDoseQuantity k = apply (fun dose -> { dose with DoseQuantity = dose.DoseQuantity |> k })
//        let applyDoseTotal k = apply (fun dose -> { dose with DoseTotal = dose.DoseTotal |> k })
//        let applyDoseRate k = apply (fun dose -> { dose with DoseRate = dose.DoseRate |> k })
//
//        let setAdminQuantity x = x |> set |> applyAdminQuantity
//        let setAdminTotal x = x |> set |> applyAdminTotal
//        let setAdminRate x = x |> set |> applyAdminRate
//        let setDoseQuantity x = x |> set |> applyDoseQuantity
//        let setDoseTotal x = x |> set |> applyDoseTotal
//        let setDoseRate x = x |> set |> applyDoseRate
//
//        let setAdminQuantityValue x = x |> QT.Quantity.setValue |> applyAdminQuantity
//        let setAdminTotalValue x = x |> QT.Total.setValue |> applyAdminTotal
//        let setAdminRateValue x = x |> QT.Rate.setValue |> applyAdminRate
//        let setDoseQuantityValue x = x |> QT.QuantityAdjust.setValue |> applyDoseQuantity
//        let setDoseTotalValue x = x |> QT.TotalAdjust.setValue |> applyDoseTotal
//        let setDoseRateValue x = x |> QT.RateAdjust.setValue |> applyDoseRate
//
//        let setAdminQuantityIncr x = x |> QT.Quantity.setIncr |> applyAdminQuantity
//        let setAdminTotalIncr x = x |> QT.Total.setIncr |> applyAdminTotal
//        let setAdminRateIncr x = x |> QT.Rate.setIncr |> applyAdminRate
//        let setDoseQuantityIncr x = x |> QT.QuantityAdjust.setIncr |> applyDoseQuantity
//        let setDoseTotalIncr x = x |> QT.TotalAdjust.setIncr |> applyDoseTotal
//        let setDoseRateIncr x = x |> QT.RateAdjust.setIncr |> applyDoseRate
//
//        let setAdminQuantityMin x = x |> QT.Quantity.setMin |> applyAdminQuantity
//        let setAdminTotalMin x = x |> QT.Total.setMin |> applyAdminTotal
//        let setAdminRateMin x = x |> QT.Rate.setMin |> applyAdminRate
//        let setDoseQuantityMin x = x |> QT.QuantityAdjust.setMin |> applyDoseQuantity
//        let setDoseTotalMin x = x |> QT.TotalAdjust.setMin |> applyDoseTotal
//        let setDoseRateMin x = x |> QT.RateAdjust.setMin |> applyDoseRate
//
//        let setAdminQuantityMax x = x |> QT.Quantity.setMax |> applyAdminQuantity
//        let setAdminTotalMax x = x |> QT.Total.setMax |> applyAdminTotal
//        let setAdminRateMax x = x |> QT.Rate.setMax |> applyAdminRate
//        let setDoseQuantityMax x = x |> QT.QuantityAdjust.setMax |> applyDoseQuantity
//        let setDoseTotalMax x = x |> QT.TotalAdjust.setMax |> applyDoseTotal
//        let setDoseRateMax x = x |> QT.RateAdjust.setMax |> applyDoseRate
//    
//    module Drug =
//
//        module PR = Product
//
//        
//
//        module Substance = 
//
//            type T =
//                { 
//                    Substance: PR.Substance.T
//                    Quantity: QT.Quantity.T // Is the substance drug quantity
//                    Concentration: QT.Concentration.T // Is the substance drug concentration
//                    Dose: Dose.T
//                } 
//
//            let apply f (x: T) = x |> f
//
//            let create subst dose qt conc = 
//                    { 
//                        Substance = subst 
//                        Quantity = qt
//                        Concentration = conc
//                        Dose = dose
//                    } 
//
//            let getName = apply (fun s -> s.Substance.Name)
//            let getDrugQuantity = apply (fun s -> s.Quantity)
//            let getComponentQuantity = apply (fun s -> s.Substance.Quantity)
//            let getComponentConcentration = apply (fun s -> s.Substance.Concentration)
//            let getDrugConcentration = apply (fun s -> s.Concentration)
//            let getAdminQuantity = apply (fun s -> s.Dose.AdminQuantity)
//            let getAdminTotal = apply (fun s -> s.Dose.AdminTotal)
//            let getAdminRate = apply (fun s -> s.Dose.AdminRate)
//            let getDoseQuantity = apply (fun s -> s.Dose.DoseQuantity)
//            let getDoseTotal = apply (fun s -> s.Dose.DoseTotal)
//            let getDoseRate = apply (fun s -> s.Dose.DoseRate)
//
//            let applySubstance k = apply (fun s -> { s with Substance = s.Substance |> k })
//            let applyDose k = apply (fun s -> { s with Dose = s.Dose |> k })
//            let applyQuantity k = apply (fun s -> { s with Quantity = s.Quantity |> k })
//            let applyConcentration k = apply (fun s -> { s with Concentration = s.Concentration |> k })
//
//            let setDrugQuantity x = x |> set |> applyQuantity
//            let setDrugConcentration x = x |> set |> applyConcentration
//            let setAdminQuantity x = x |> Dose.setAdminQuantity |> applyDose
//            let setAdminTotal x = x |> Dose.setAdminTotal |> applyDose
//            let setAdminRate x = x |> Dose.setAdminRate |> applyDose
//            let setDoseQuantity x = x |> Dose.setDoseQuantity |> applyDose
//            let setDoseTotal x = x |> Dose.setDoseTotal |> applyDose
//            let setDoseRate x = x |> Dose.setDoseRate |> applyDose
//
//            let setComponentQuantityValue x = x |> PR.Substance.setQuantityValue |> applySubstance
//            let setComponentConcentrationValue x = x |> PR.Substance.setConcentrationValue |> applySubstance
//            let setDrugQuantityValue x = x |> QT.Quantity.setValue |> applyQuantity
//            let setDrugConcentrationValue x = x |> QT.Concentration.setValue |> applyConcentration
//            let setAdminQuantityValue x = x |> Dose.setAdminQuantityValue |> applyDose
//            let setAdminTotalValue x = x |> Dose.setAdminTotalValue |> applyDose
//            let setAdminRateValue x = x |> Dose.setAdminRateValue |> applyDose
//            let setDoseQuantityValue x = x |> Dose.setDoseQuantityValue |> applyDose
//            let setDoseTotalValue x = x |> Dose.setDoseTotalValue |> applyDose
//            let setDoseRateValue x = x |> Dose.setDoseRateValue |> applyDose
//
//            let setComponentQuantityIncr x = x |> PR.Substance.setQuantityIncr |> applySubstance
//            let setComponentConcentrationIncr x = x |> PR.Substance.setConcentrationIncr |> applySubstance
//            let setDrugQuantityIncr x = x |> QT.Quantity.setIncr |> applyQuantity
//            let setDrugConcentrationIncr x = x |> QT.Concentration.setIncr |> applyConcentration
//            let setAdminQuantityIncr x = x |> Dose.setAdminQuantityIncr |> applyDose
//            let setAdminTotalIncr x = x |> Dose.setAdminTotalIncr |> applyDose
//            let setAdminRateIncr x = x |> Dose.setAdminRateIncr |> applyDose
//            let setDoseQuantityIncr x = x |> Dose.setDoseQuantityIncr |> applyDose
//            let setDoseTotalIncr x = x |> Dose.setDoseTotalIncr |> applyDose
//            let setDoseRateIncr x = x |> Dose.setDoseRateIncr |> applyDose
//
//            let setComponentQuantityMin x = x |> PR.Substance.setQuantityMin |> applySubstance
//            let setComponentConcentrationMin x = x |> PR.Substance.setConcentrationMin |> applySubstance
//            let setDrugQuantityMin x = x |> QT.Quantity.setMin |> applyQuantity
//            let setDrugConcentrationMin x = x |> QT.Concentration.setMin |> applyConcentration
//            let setAdminQuantityMin x = x |> Dose.setAdminQuantityMin |> applyDose
//            let setAdminTotalMin x = x |> Dose.setAdminTotalMin |> applyDose
//            let setAdminRateMin x = x |> Dose.setAdminRateMin |> applyDose
//            let setDoseQuantityMin x = x |> Dose.setDoseQuantityMin |> applyDose
//            let setDoseTotalMin x = x |> Dose.setDoseTotalMin |> applyDose
//            let setDoseRateMin x = x |> Dose.setDoseRateMin |> applyDose
//
//            let setComponentQuantityMax x = x |> PR.Substance.setQuantityMax |> applySubstance
//            let setComponentConcentrationMax x = x |> PR.Substance.setConcentrationMax |> applySubstance
//            let setDrugQuantityMax x = x |> QT.Quantity.setMax |> applyQuantity
//            let setDrugConcentrationMax x = x |> QT.Concentration.setMax |> applyConcentration
//            let setAdminQuantityMax x = x |> Dose.setAdminQuantityMax |> applyDose
//            let setAdminTotalMax x = x |> Dose.setAdminTotalMax |> applyDose
//            let setAdminRateMax x = x |> Dose.setAdminRateMax |> applyDose
//            let setDoseQuantityMax x = x |> Dose.setDoseQuantityMax |> applyDose
//            let setDoseTotalMax x = x |> Dose.setDoseTotalMax |> applyDose
//            let setDoseRateMax x = x |> Dose.setDoseRateMax |> applyDose
//
//
//        module Component =
//
//            type T = 
//                { 
//                    Product: PR.T
//                    Quantity: QT.Quantity.T 
//                    Concentration: QT.Concentration.T 
//                    Dose: Dose.T
//                    Substances: Substance.T * Substance.T list
//                } 
//
//            let apply f (x: T) = x |> f
//
//            let create p qt conc d = 
//                // Note: not correct yet!
//                let createS s = Substance.create s d qt conc
//                { 
//                    Product = p
//                    Quantity = qt
//                    Concentration = conc
//                    Dose = d
//                    Substances = 
//                        (p.Substances |> fst |> createS, 
//                            p.Substances |> snd |> List.map createS)
//                } 
//
//
//            let getName = apply (fun c -> c.Product.Name)
//            let getQuantity = apply (fun c -> c.Quantity)
//            let getTotal = apply (fun c -> c.Product.Quantity)
//            let getConcentration = apply (fun c -> c.Concentration)
//            let getAdminQuantity = apply (fun c -> c.Dose.AdminQuantity)
//            let getAdminTotal = apply (fun c -> c.Dose.AdminTotal)
//            let getAdminRate = apply (fun c -> c.Dose.AdminRate)
//            let getDoseQuantity = apply (fun c -> c.Dose.DoseQuantity)
//            let getDoseTotal = apply (fun c -> c.Dose.DoseTotal)
//            let getDoseRate = apply (fun c -> c.Dose.DoseRate)
//
//            let applyProduct k = apply (fun c -> { c with Product = c.Product |> k })
//            let applyDose k = apply (fun c -> { c with Dose = c.Dose |> k })
//            let applyQuantity k = apply (fun c -> { c with Quantity = c.Quantity |> k })
//            let applyConcentration k = apply (fun c -> { c with Concentration = c.Concentration |> k })
//
//            let setQuantity x = x |> set |> applyQuantity
//            let setConcentration x = x |> set|> applyConcentration
//            let setAdminQuantity x = x |> Dose.setAdminQuantity |> applyDose
//            let setAdminTotal x = x |> Dose.setAdminTotal |> applyDose
//            let setAdminRate x = x |> Dose.setAdminRate |> applyDose
//            let setDoseQuantity x = x |> Dose.setDoseQuantity |> applyDose
//            let setDoseTotal x = x |> Dose.setDoseTotal |> applyDose
//            let setDoseRate x = x |> Dose.setDoseRate |> applyDose
//
//            let setQuantityValue x = x |> QT.Quantity.setValue |> applyQuantity
//            let setConcentrationValue x = x |> QT.Concentration.setValue |> applyConcentration
//            let setAdminQuantityValue x = x |> Dose.setAdminQuantityValue |> applyDose
//            let setAdminTotalValue x = x |> Dose.setAdminTotalValue |> applyDose
//            let setAdminRateValue x = x |> Dose.setAdminRateValue |> applyDose
//            let setDoseQuantityValue x = x |> Dose.setDoseQuantityValue |> applyDose
//            let setDoseTotalValue x = x |> Dose.setDoseTotalValue |> applyDose
//            let setDoseRateValue x = x |> Dose.setDoseRateValue |> applyDose
//
//            let setQuantityIncr x = x |> QT.Quantity.setIncr |> applyQuantity
//            let setConcentrationIncr x = x |> QT.Concentration.setIncr |> applyConcentration
//            let setAdminQuantityIncr x = x |> Dose.setAdminQuantityIncr |> applyDose
//            let setAdminTotalIncr x = x |> Dose.setAdminTotalIncr |> applyDose
//            let setAdminRateIncr x = x |> Dose.setAdminRateIncr |> applyDose
//            let setDoseQuantityIncr x = x |> Dose.setDoseQuantityIncr |> applyDose
//            let setDoseTotalIncr x = x |> Dose.setDoseTotalIncr |> applyDose
//            let setDoseRateIncr x = x |> Dose.setDoseRateIncr |> applyDose
//
//            let setQuantityMin x = x |> QT.Quantity.setMin |> applyQuantity
//            let setConcentrationMin x = x |> QT.Concentration.setMin |> applyConcentration
//            let setAdminQuantityMin x = x |> Dose.setAdminQuantityMin |> applyDose
//            let setAdminTotalMin x = x |> Dose.setAdminTotalMin |> applyDose
//            let setAdminRateMin x = x |> Dose.setAdminRateMin |> applyDose
//            let setDoseQuantityMin x = x |> Dose.setDoseQuantityMin |> applyDose
//            let setDoseTotalMin x = x |> Dose.setDoseTotalMin |> applyDose
//            let setDoseRateMin x = x |> Dose.setDoseRateMin |> applyDose
//
//            let setQuantityMax x = x |> QT.Quantity.setMax |> applyQuantity
//            let setConcentrationMax x = x |> QT.Concentration.setMax |> applyConcentration
//            let setAdminQuantityMax x = x |> Dose.setAdminQuantityMax |> applyDose
//            let setAdminTotalMax x = x |> Dose.setAdminTotalMax |> applyDose
//            let setAdminRateMax x = x |> Dose.setAdminRateMax |> applyDose
//            let setDoseQuantityMax x = x |> Dose.setDoseQuantityMax |> applyDose
//            let setDoseTotalMax x = x |> Dose.setDoseTotalMax |> applyDose
//            let setDoseRateMax x = x |> Dose.setDoseRateMax |> applyDose
//
//
//        type T = 
//            { 
//                Name: string; 
//                Total: QT.Quantity.T
//                Dose: Dose.T
//                Components: Component.T * Component.T list
//            } 
//
//        let apply f (x: T) = x |> f
//
//        let getComps (d: T) = (d.Components |> fst)::(d.Components |> snd)
//        let getSubs (c: Component.T) = (c.Substances |> fst)::(c.Substances |> snd)
//
//
//        let getName = apply (fun d -> d.Name)
//        let getTotal = apply (fun d -> d.Total)
//        let getAdminQuantity = apply (fun d -> d.Dose.AdminQuantity)
//        let getAdminTotal = apply (fun d -> d.Dose.AdminTotal)
//        let getAdminRate = apply (fun d -> d.Dose.AdminRate)
//        let getDoseQuantity = apply (fun d -> d.Dose.DoseQuantity)
//        let getDoseTotal = apply (fun d -> d.Dose.DoseTotal)
//        let getDoseRate = apply (fun d -> d.Dose.DoseRate)
//
//        let applyDose k = apply (fun d -> { d with Dose = d.Dose |> k })
//        let applyTotal k = apply (fun d -> { d with Total = d.Total |> k })
//
//        let setTotal x = x |> set |> applyTotal
//        let setAdminQuantity x = x |> Dose.setAdminQuantity |> applyDose
//        let setAdminTotal x = x |> Dose.setAdminTotal |> applyDose
//        let setAdminRate x = x |> Dose.setAdminRate |> applyDose
//        let setDoseQuantity x = x |> Dose.setDoseQuantity |> applyDose
//        let setDoseTotal x = x |> Dose.setDoseTotal |> applyDose
//        let setDoseRate x = x |> Dose.setDoseRate |> applyDose
//
//        let setTotalValue x = x |> QT.Quantity.setValue |> applyTotal
//        let setAdminQuantityValue x = x |> Dose.setAdminQuantityValue |> applyDose
//        let setAdminTotalValue x = x |> Dose.setAdminTotalValue |> applyDose
//        let setAdminRateValue x = x |> Dose.setAdminRateValue |> applyDose
//        let setDoseQuantityValue x = x |> Dose.setDoseQuantityValue |> applyDose
//        let setDoseTotalValue x = x |> Dose.setDoseTotalValue |> applyDose
//        let setDoseRateValue x = x |> Dose.setDoseRateValue |> applyDose
//
//        let setTotalIncr x = x |> QT.Quantity.setIncr |> applyTotal
//        let setAdminQuantityIncr x = x |> Dose.setAdminQuantityIncr |> applyDose
//        let setAdminTotalIncr x = x |> Dose.setAdminTotalIncr |> applyDose
//        let setAdminRateIncr x = x |> Dose.setAdminRateIncr |> applyDose
//        let setDoseQuantityIncr x = x |> Dose.setDoseQuantityIncr |> applyDose
//        let setDoseTotalIncr x = x |> Dose.setDoseTotalIncr |> applyDose
//        let setDoseRateIncr x = x |> Dose.setDoseRateIncr |> applyDose
//
//        let setTotalMin x = x |> QT.Quantity.setMin |> applyTotal
//        let setAdminQuantityMin x = x |> Dose.setAdminQuantityMin |> applyDose
//        let setAdminTotalMin x = x |> Dose.setAdminTotalMin |> applyDose
//        let setAdminRateMin x = x |> Dose.setAdminRateMin |> applyDose
//        let setDoseQuantityMin x = x |> Dose.setDoseQuantityMin |> applyDose
//        let setDoseTotalMin x = x |> Dose.setDoseTotalMin |> applyDose
//        let setDoseRateMin x = x |> Dose.setDoseRateMin |> applyDose
//
//        let setTotalMax x = x |> QT.Quantity.setMax |> applyTotal
//        let setAdminQuantityMax x = x |> Dose.setAdminQuantityMax |> applyDose
//        let setAdminTotalMax x = x |> Dose.setAdminTotalMax |> applyDose
//        let setAdminRateMax x = x |> Dose.setAdminRateMax |> applyDose
//        let setDoseQuantityMax x = x |> Dose.setDoseQuantityMax |> applyDose
//        let setDoseTotalMax x = x |> Dose.setDoseTotalMax |> applyDose
//        let setDoseRateMax x = x |> Dose.setDoseRateMax |> applyDose
//
//
//        let toProdEqs qts = qts |> List.map QT.toProdEq
//        let toSumEqs  qts = qts |> List.map QT.toSumEq
//
//
// //     substAdminRate = adjust x substDoseRate 
// //     substDoseRate  = substDrugConc x drugDoseRate
// //     substDoseRate  = substCompConc x compDoseRate
// //     substAdminRate = substDrugConc x drugAdminRate
// //     substAdminRate = substCompConc x compAdminRate
//        let contSubstEqs adj drug comp eqs subst =
//
//            let drugAdminRate  = drug |> getAdminRate :> QT.IQuantity
//            let drugDoseRate   = drug |> getDoseRate :> QT.IQuantity
//            let compAdminRate  = comp |> Component.getAdminRate :> QT.IQuantity
//            let compDoseRate   = comp |> Component.getDoseRate :> QT.IQuantity
//            let substAdminRate = subst |> Substance.getAdminRate :> QT.IQuantity
//            let substDoseRate  = subst |> Substance.getDoseRate :> QT.IQuantity
//            let substDrugConc  = subst |> Substance.getDrugConcentration :> QT.IQuantity
//            let substCompConc  = subst |> Substance.getComponentConcentration :> QT.IQuantity
//            
//            let adjust adj eqs = 
//                match adj with
//                | Dose.Adjust.Weight a 
//                | Dose.Adjust.Length a
//                | Dose.Adjust.Surface a ->
//                    let adjust = a :> QT.IQuantity
//                    [[substAdminRate; adjust; substDoseRate]] 
//                    |> toProdEqs 
//                    |> List.append eqs
//                | Dose.Adjust.NoAdjust -> eqs
//
//            [[substDoseRate;  substDrugConc; drugDoseRate]
//             [substDoseRate;  substCompConc; compDoseRate]
//             [substAdminRate; substDrugConc; drugAdminRate]
//             [substAdminRate; substCompConc; compAdminRate]] 
//            |> toProdEqs
//            |> List.append eqs
//
////      compAdminRate = adjust x compDoseRate
////      compAdminRate = compConc x drugAdminRate
////      compDoseRate  = compConc x drugDoseRate
//        let contCompEqs adj drug eqs comp = 
//
//            let drugAdminRate = drug |> getAdminRate :> QT.IQuantity
//            let drugDoseRate = drug |> getDoseRate :> QT.IQuantity
//            let compAdminRate = comp |> Component.getAdminRate :> QT.IQuantity
//            let compDoseRate  = comp |> Component.getDoseRate :> QT.IQuantity
//            let compConc      = comp |> Component.getConcentration :> QT.IQuantity
//
//            let adjust adj eqs = 
//                match adj with
//                | Dose.Adjust.Weight a 
//                | Dose.Adjust.Length a
//                | Dose.Adjust.Surface a ->
//                    let adjust = a :> QT.IQuantity
//                    [[compAdminRate; adjust; compDoseRate]] 
//                    |> toProdEqs 
//                    |> List.append eqs
//                | Dose.Adjust.NoAdjust -> eqs
//
//            let eqs =
//                [[compAdminRate; compConc; drugAdminRate]
//                 [compDoseRate; compConc; drugDoseRate]] 
//                |> toProdEqs
//                |> List.append eqs
//                |> adjust adj
//            comp
//            |> getSubs 
//            |> List.fold (contSubstEqs adj drug comp) eqs 
//
////      drugAdminRate = adjust x drugDoseRate 
//        let contDrugEqs adj eqs drug =
//            let drugAdminRate = drug |> getAdminRate :> QT.IQuantity
//            let drugDoseRate  = drug |> getDoseRate :> QT.IQuantity
//
//            let adjust adj eqs = 
//                match adj with
//                | Dose.Adjust.Weight a 
//                | Dose.Adjust.Length a
//                | Dose.Adjust.Surface a ->
//                    let adjust = a :> QT.IQuantity
//                    [[drugAdminRate; adjust; drugDoseRate]] 
//                    |> toProdEqs 
//                    |> List.append eqs
//                | Dose.Adjust.NoAdjust -> eqs
//
//            let eqs = eqs |> adjust adj
//
//            drug 
//            |> getComps
//            |> List.fold (contCompEqs adj drug) eqs
//
////      substDrugQty = substDrugConc x drugTotal   
////      substCompQty = substCompConc x compTotal   
////      substDrugQty = substCompConc x compQty     
////      doseTotal = freq x doseQty     
////      doseQty = substDrugQty               
//        let discSubstEqs adj drug comp freq eqs (s: Substance.T) =
//            let substDrugQty = s.Quantity :> QT.IQuantity
//            let substDrugConc = s.Concentration :> QT.IQuantity
//            let drugTotal = drug |> getTotal :> QT.IQuantity
//            let substCompQty = s.Substance.Quantity :> QT.IQuantity
//            let substCompConc = s.Substance.Concentration :> QT.IQuantity
//            let compTotal = comp |> Component.getTotal :> QT.IQuantity
//            let compQty = comp |> Component.getQuantity :> QT.IQuantity
//            let doseTotal = s.Dose.DoseTotal :> QT.IQuantity
//            let doseQty = s.Dose.DoseQuantity
//
//            [[substDrugQty; substDrugConc; drugTotal]
//             [substCompQty; substCompConc; compTotal]
//             [substDrugQty; substCompConc; compQty]
//             [doseTotal; freq; doseQty]]
//            |> toProdEqs
//            |> List.append eqs
//
////      compQty = compConc x drugTotal   
////      compDose = compConc x prescrQty   
////      compDoseTotal = compConc x prescrTotal 
////      compDoseRate = compConc x prescrRate  
////      compDose = compDoseRate x time        
////      compDoseTotal = compDose x freq        
//        let discCompEqs adj drug freq eqs (comp: Component.T) =
//            let qty = drug |> getAdminQuantity :> QT.IQuantity
//            let freq = freq :> QT.IQuantity
//            let tot = drug |> getAdminTotal :> QT.IQuantity
//
//            comp
//            |> getSubs 
//            |> List.fold (discSubstEqs adj drug comp freq) eqs 
//
//
//        let discDrugEqs adj freq eqs drug =
//            drug
//            |> getComps
//            |> List.fold (discCompEqs adj drug freq) eqs
//
//            
////      substDrugQty = substDrugConc x drugTotal  
////      substDrugQty = substCompConc x compQty    
////      substCompQty = substCompConc x compTotal  
////      doseTotal = freq x doseQty    
////      doseQty = time x doseRate   
////      doseRate = substDrugConc x prescrRate 
////      doseQty = substDrugConc x prescrQty  
////      doseTotal = substDrugConc x prescrTotal
//        let timedSubstEqs adj drug comp freq time eqs (subst: Substance.T) =
//            let substDrugQty = subst |> Substance.getDrugQuantity :> QT.IQuantity
//            let substDrugConc = subst |> Substance.getDrugConcentration :> QT.IQuantity
//            let drugTotal = drug |> getTotal :> QT.IQuantity
//            let susbstCompConc = subst |> Substance.getComponentConcentration :> QT.IQuantity
//            let compQty = comp |> Component.getQuantity :> QT.IQuantity
//            let substCompQty = subst |> Substance.getComponentQuantity :> QT.IQuantity
//            let compTotal = comp |> Component.getTotal :> QT.IQuantity  
//            let doseTotal = subst |> Substance.getDoseTotal :> QT.IQuantity
//            let doseQty = subst |> Substance.getDoseQuantity :> QT.IQuantity
//            let doseRate = subst |> Substance.getDoseRate :> QT.IQuantity
//            let adminRate = drug |> getAdminRate :> QT.IQuantity
//            let adminQty = drug |> getAdminQuantity :> QT.IQuantity
//            let adminTotal = drug |> getAdminTotal :> QT.IQuantity
//
//            [[substDrugQty; substDrugConc; drugTotal]]
//            |> toProdEqs
//            |> List.append eqs
//
////      compQty = compConc x drugTotal  
////      compDose = compConc x prescrQty  
////      compDoseTotal = compConc x prescrTotal
////      compDoseRate = compConc x prescrRate 
////      compDose = compDoseRate x time       
////      compDoseTotal = compDose x freq       
//        let timedCompEqs adj drug freq time eqs (comp: Component.T) =
//            let compQty = comp |> Component.getQuantity :> QT.IQuantity
//            let compConc = comp |> Component.getConcentration :> QT.IQuantity
//            let drugTotal = drug |> getTotal :> QT.IQuantity
//            let compDose = comp |> Component.getAdminQuantity :> QT.IQuantity
//            let compConc = comp |> Component.getConcentration :> QT.IQuantity
//            let adminQty = drug |> getAdminQuantity :> QT.IQuantity
//            let compDoseTotal = comp |> Component.getDoseTotal :> QT.IQuantity
//            let adminTotal = drug |> getAdminTotal :> QT.IQuantity
//            let compDoseRate = comp |> Component.getDoseRate :> QT.IQuantity
//
//            let eqs =
//                [[compQty; compConc; drugTotal]] 
//                |> toProdEqs
//                |> List.append eqs
//            comp
//            |> getSubs 
//            |> List.fold (timedSubstEqs adj drug comp freq time) eqs 
//
//        let timedDrugEqs adj freq time eqs drug =
//            drug
//            |> getComps
//            |> List.fold (timedCompEqs adj drug freq time) eqs
//            
//        let getSumEq (d: T) =
//            d
//            |> getComps
//            |> List.map(fun c -> c.Quantity :> QT.IQuantity)
//            |> QT.toSumEq
//
//
//    module Lab =
//        type T = { Name: string; Quantity: int }
//        type Create = string -> int -> T option
//
//        let create: Create = 
//            fun n q -> { Name = n; Quantity = q } |> Some
//
//    type T = 
//        | DrugOrderable of Drug.T
//        | LabOrderable of Lab.T
//        | OrderableComposition of T * T list
//
//    let create c =
//        function
//        | [] -> None
//        | x::[] -> x |> c |> Some
//        | x::xs -> OrderableComposition (x |> c, xs |> List.map c) |> Some
//
//    let toEquations prescr ord adj =
//        let eqs = 
//            match ord with 
//            | DrugOrderable d -> [(Drug.getSumEq d)]
//            | LabOrderable _ -> []
//
//        match prescr |> PN.getPrescription with
//        | PN.Process -> []
//        | PN.Continuous -> 
//            match ord with
//            | DrugOrderable d -> d |> Drug.contDrugEqs adj eqs
//            | LabOrderable _ -> eqs
//        | PN.Discontinuous freq ->
//            match ord with
//            | DrugOrderable d -> d |> Drug.discDrugEqs adj freq eqs
//            | LabOrderable _ -> eqs
//        | PN.Timed (freq, time) ->
//            match ord with
//            | DrugOrderable d -> d |> Drug.timedDrugEqs adj freq time eqs
//            | LabOrderable _ -> eqs
//
//
//module BirthDate =
//    open System
//
//    module WV = WrappedValue
//    module WDT = WrappedDateTime
//
//    [<Literal>]
//    let MaxAge = 130
//        
//    type T =
//        | BirthDate of WrappedDateTime.T
//        | NoBirthDate
//
//    let isValid dt = 
//        let minDt = DateTime.Now.AddYears(-MaxAge)
//        let maxDt = DateTime.Now
//        dt > minDt && dt <= maxDt
//
//    let create dt =
//        match dt |> WrappedDateTime.create id isValid with
//        | Some dt' -> dt' |> BirthDate
//        | None -> NoBirthDate
//
//    let value =
//        function
//        | BirthDate dt -> dt |> WV.value
//        | NoBirthDate -> DateTime.MaxValue 
//
//    let stringValue format dt = 
//        match dt |> value with
//        | dt' when dt' = DateTime.MaxValue -> String.Empty
//        | dt' -> dt'.ToString()
//        
//    type T with
//        static member Create = create 
//
//
//module Person =
//    open System
//
//    open Informedica.Utilities
//
//    module WV = WrappedValue
//    module WS = WrappedString
//    module WD = WrappedDateTime
//    module BD = BirthDate
//
//    let (<!>) = Result.liftR
//    let (<*>) = Result.applyR
//
//    module Messages = 
//        type T =
//            | CreatedId of string
//            | CreatedFirstName of string
//            | CouldNotCreateId of string
//            | CouldNotCreateFirstName of string
//
//
//    module PersonDto =
//
//        type T () =
//            member val Id: string = null with get, set
//            member val FirstName : string = null with get, set
////            member val Initials : string list = [] with get, set
////            member val LastName : string = null with get, set
////            member val BirthDate : Nullable<DateTime> = Nullable() with get, set
//
//
//    module Id =
//
//        [<CustomEquality; NoComparison>]
//        type T = 
//            | Id of WS.String10.T
//            | NoId
//
//            override this.GetHashCode() = hash this
//
//            override this.Equals(other) =
//                match other with
//                | :? T as id -> 
//                    match this, id with
//                    | Id ws1, Id ws2 -> ws1 |> WrappedValue.equals ws2
//                    | _ -> false
//                | _ -> false
//
//        let create s = 
//            let map = function
//                | WS.Messages.StringIsNull -> Messages.CouldNotCreateId "because string was null"
//                | WS.Messages.CreatedString _ -> Messages.CreatedId s
//                | WS.Messages.StringLargerThan n -> Messages.CouldNotCreateId (s + " was larger than " + string n)
//
//            match s |> WS.String10.create with
//            | Result.Failure errs -> (NoId, errs) |> Result.Success
//            | Result.Success (s10, msgs) -> (s10 |> Id, msgs) |> Result.Success
//            |> Result.mapMessagesR map
//
//        let apply f = function
//            | Id s10 -> s10 |> WS.String10.apply f
//            | NoId -> "" |> f
//
//        let get = apply id
//        let change f x = x |> apply f |> create
//
//
//    module FirstName =
//
//        type T = FirstName of  WS.CapitalizedName.T
//
//        let create s =
//            let map = function 
//                | WS.Messages.StringIsNull -> Messages.CouldNotCreateFirstName "because string was null"
//                | WS.Messages.CreatedString s -> Messages.CreatedFirstName s
//                | WS.Messages.StringLargerThan _ -> failwith "unexpected message"
//
//            match s |> WS.CapitalizedName.create with
//            | Result.Failure errs -> errs |> Result.Failure
//            | Result.Success (n, msgs) -> (n |> FirstName, msgs) |> Result.Success
//            |> Result.mapMessagesR map
//
//        let apply f (FirstName n) = n |> WS.CapitalizedName.apply f
//
//        let get = apply id
//        let change f x = x |> apply f |> create
//
//
////    type SingleCapital = WS.SingleCapital
////    type FirstName = WS.CapitalizedName
////    type LastName = WS.CapitalizedName
////    type BirthDate = BirthDate.T
//
//    [<StructuralEquality; NoComparison>]
//    type PersonName =
//        { 
//            FirstName: FirstName.T
////            Initials: SingleCapital list
////            LastName: LastName
//        }
//
//    
//    [<CustomEquality; NoComparison>]
//    type T = 
//        { 
//            Id: Id.T
//            Name: PersonName
////            BirthDate: BirthDate
//        }
//
//        override this.GetHashCode() = hash this.Id
//
//        override this.Equals(other) =
//            match other with
//            | :? T as p -> p.Id = this.Id
//            | _ -> false
//
//
//    let create (dto: PersonDto.T) =
//
//        let createPerson id name = { Id = id; Name = name } 
//        let createPersonName s =
//            match s |> FirstName.create with
//            | Result.Success (n, msgs) -> ({ FirstName = n }, msgs) |> Result.Success
//            | Result.Failure errs -> Result.Failure errs
//
//        createPerson
//        <!> Id.create dto.Id
//        <*> createPersonName dto.FirstName
//
//
//module Patient =
//
//    open Informedica.Utilities
//
//    module WV = WrappedValue
//    module WS = WrappedString
//    module MP = Multipliers
//    module UN = Unit
//    module VR = Variable
//
//    [<NoComparison>]
//    type HospitalId = 
//        | HospitalId of WS.String10
//        | NoHospitalId
//
//    type Person = 
//        | Person of Person.T
//        | Anonymous
//
//    type Weight = Weight of VR.T
//    type Length = Length of VR.T
//    type BSA = BSA of VR.T
//
//    [<CustomEquality; NoComparison>]
//    type T = 
//        {
//            HospitalId: HospitalId
//            Person: Person
//            Weight: Weight option
//            Length: Length option
//            BodySurfaceArea: BSA option
//        } with
//
//        override this.GetHashCode() = this.HospitalId |> hash
//
//        override this.Equals(other) = 
//            match other with
//            | :? T as pat -> pat.HospitalId = this.HospitalId
//            | _ -> false
//
//    let (>>=) o f = Option.bind f o
//
//    let createHospitalId s = 
//        match s|> WS.createString10 with
//        | Some n -> n |> HospitalId |> Some 
//        | None -> None
//
//    let createWeight q u =
//        let weight = [] |> VR.withUnit UN.gram |> VR.create
//        weight |> VR.setMin 200N
//        weight |> VR.setMax (700N * MP.kilo)
//
//        match u |> UN.weightFromString with
//        | Some wu -> weight |> VR.setValue [q]
//        | None -> ()
//        
//        weight |> Weight
//
//    let getWeight (Weight w) = 
//        match w |> VR.getUnitValues with
//        | v::[] -> v | _ -> 0N
//
//    let createLength q u =
//        let length = [] |> VR.withUnit UN.centimeter |> VR.create
//        length |> VR.setMin 30N
//        length |> VR.setMax 300N
//
//        match u |> UN.distanceFromString with
//        | Some du -> length |> VR.setValue [q]
//        | None -> ()
//        
//        length |> Length
//
//    let getLength (Length l) =
//        match l |> VR.getUnitValues with
//        | v::[] -> v | _ -> 0N
//
//    let newPatient =
//        {
//            HospitalId = NoHospitalId
//            Person = Anonymous
//            Weight = None
//            Length = None
//            BodySurfaceArea = None
//        } |> Some
//
//    let setHospitalId =
//        fun id p ->
//            match id |> createHospitalId with
//            | Some id' -> { p with HospitalId = id' } |> Some
//            | None -> None
//
//    let setPerson =
//        fun id name p ->
//            match Person.create_opt id name with
//            | Some person -> { p with Person =  person |> Person } |> Some
//            | None -> None
//
//    let setBirthDate =
//        fun bd p ->
//            let set bd =
//                match p.Person with
//                | Person person -> person.SetBirthDate bd |> Some
//                | Anonymous -> None
//
//            match bd |> set with
//            | Some person -> { p with Person = person |> Person } |> Some
//            | None -> None
//
//    let setWeight =
//        fun qt u p -> { p with Weight = u |> createWeight qt |> Some } |> Some
//
//    let setLength =
//        fun v u p -> { p with Length = u |> createLength v |> Some } |> Some
//
//    let create id n bd =
//        newPatient 
//        >>= (id |> setHospitalId)
//        >>= (setPerson id n)
//        >>= (bd |> setBirthDate)
//
//
//module Order =
//
//    exception InvalidOrderException
//
//    module OR = Orderable
//    module PR = Prescription
//
//    type T = 
//        {
//            Id: string // OrderId.T
//            Patient: Patient.T
//            Indications: string * string list //Indication.T * Indication.T list
//            Orderable: Orderable.T
//            Prescription: Prescription.T
//            Route: string // Route.T
//            StartDate: System.DateTime // StartDate.T
//            StopDate: System.DateTime option // StopDate.T option
//        }
//
//    let applyToT f (x: T) = x |> f
//
//    let getPatient = applyToT (fun x -> x.Patient)
//    let getOrderable = applyToT (fun x -> x.Orderable)
//    let getPrescription = applyToT (fun x -> x.Prescription)
//    
//    let orderToEquations o = 
//        let pat, ord, pr = o |> getPatient, o |> getOrderable, o |> getPrescription 
//
//        []
//
//
//module EQ = Equation
//module VR = Variable
//module VC = ValuesUnitCombi
//module UN = Unit
//module SV = Solver
//module QT = Quantities
//module PR = Prescription
//module OR = Orderable
//
//let tot = [12N] |> VR.withUnit UN.Units.milliGram |> VR.per 1N UN.Units.day |> VR.create
//let freq = VR.emptyVar()
//let qty = [1N;2N;3N;4N;6N;12N] |> VR.withUnit UN.Units.milliGram |> VR.create
//let rate = [] |> VR.withUnit UN.Units.milliLiter |> VR.per 1N UN.Units.hour |> VR.create
//let time = [] |> VR.withUnit UN.Units.hour |> VR.create
//freq |> VR.setIncr 1N
//rate |> VR.setIncr (1N/10N)
//rate |> VR.setMax 20N
//
//let e1 = [tot;freq;qty] |> EQ.createProductEq
//let e2 = [qty;rate;time] |> EQ.createProductEq
//
//
//let (>>=) m k = Railway.bind k m
//let lift2 k m1 m2 = 
//    match m1 with 
//    | Railway.Success m1' -> k m1' m2 |> Railway.succeed  
//    | Railway.Failure f -> 
//        f @ [""] |> ignore; // Necessary to avoid value restriction exception
//        f |> Railway.fail
//
//let setSubstDrugQty = lift2 Product.Substance.setQuantity
//let setSubstDoseTot = lift2 OR.Drug.Substance.setDoseTotal
//let setSubstDoseTotIncr = lift2 OR.Drug.Substance.setDoseTotalIncr
//
//
//let pcmS = Product.Substance.create "paracetamol"
//
//pcmS |> Railway.succeed
//>>= setSubstDrugQty (QT.Quantity.create [240N] "milligram")
//
//let createSubst n qty qun cqty cun = 
//    let setQty = lift2 Product.Substance.setQuantity
//    let setConc = lift2 Product.Substance.setConcentration
//
//    Product.Substance.create n 
//    |> Railway.succeed
//    >>= setQty (QT.Quantity.create [qty] qun)
//    >>= setConc (QT.Concentration.create [cqty] qun cun)
//
//createSubst "dopamine" 200N "milligram" 5N "milliliter"
//
//
//let freq2 = QT.Frequency.freq [] |> QT.Frequency.per 1N "day" |> Railway.getSuccess
//freq2 :> QT.IQuantity |> QT.setIncr 2N :?> QT.Frequency.T
//
//freq2 |> QT.Frequency.setIncr 3N
//
//
//let doseTot = QT.Total.create [] "milligram" 1N "day" |> Railway.getSuccess
//doseTot |> QT.setIncr 1N
//doseTot |> QT.setMax 10N
//
//let dt = QT.Total.total [] "milligram" |> QT.Total.per 1N "day"
//let dt2 = QT.TotalAdjust.total [] "milligram" |> QT.TotalAdjust.adjustBy 1N "kilogram" |> QT.TotalAdjust.per 1N "day"
//
//failwithf "test"
//
//type DomainMessages = 
//    | ``Name cannot be null``
//    | ``Value must be positive``
//
//let msg = ``Name cannot be null``
//
//let map = function
//    | ``Name cannot be null`` -> printfn "Name cannot be positibe"
//    | ``Value must be positive`` -> ()
//
//
//
//
