(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../src/Informedica.GenOrder.Lib/Scripts"
#load "load-project-release.fsx"
#load "load-project-release.fsx"

#time

open Informedica.GenUtils.Lib.BCL
open Informedica.GenOrder.Lib

module VU = VariableUnit
module NM = VU.Name
module FR = VU.Frequency
module OR = Orderable
module IT = OR.Item
module CM = OR.Component
module PR = Prescription
module OD = Order
module MP = OD.Mapping
module SV = Solver
module UN = Unit
module UG = Informedica.GenUnits.Lib.UnitGroup

let print ord =
    for s in ord |> OD.toString do
        printfn "// [fsi: %s]" s
    ord

(**
GenOrder
======================

Documentation

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The GenOrder library can be <a href="https://nuget.org/packages/Informedica.GenOrder.Lib">installed from NuGet</a>:
      <pre>PM> Install-Package Informedica.GenOrder.Lib -Pre</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

This example demonstrates using a function defined in this sample library.

*)

// Create an orderable with two components
// dopamine and sodium/chloride
// dopamine component contains item dopamine
// sodium/chloride component (is saline) contains sodium and chloride
// dopamine is measured in mass units, sodium and chloride in molar units
// the components are measured in volume units
// Dose is adjusteded in weight units        
for o in OR.createNew 
    [
        ["dopamine", "mg[Mass]"]
        ["sodium", "mmol[Molar]";"chloride", "mmol[Molar]"]
    ] 
    "ml[Volume]" "kg[Weight]" |> OR.toString do
    printfn "%s" o

// Create a paracetamol order with an paracetamol orderable
// and how to prescribe this
let pcm = 
    OR.createNew 
        [
            ["paracetamol", "mg[Mass]"]
        ] 
        "tabl[Shape]" 
        "kg[Weight]" 

let prs = PR.discontinuous
let ord = OD.createNew "kg[Weight]" pcm prs "oral"

// Print out the order
ord |> print |> ignore

// Prints:

// [fsi: Route]
// [fsi: oral]
// [fsi: Prescription]
// [fsi: Freq<..> Count/Time]
// [fsi: Orderable]
// [fsi: paracetamol.Item.Component.Qty<..> Mass]
// [fsi: paracetamol.Item.Orderable.Qty<..> Mass]
// [fsi: paracetamol.Item.Component.Conc<..> Mass/Tabl]
// [fsi: paracetamol.Item.Orderable.Conc<..> Mass/Tabl]
// [fsi: paracetamol.Item.Dose.Qty<..> Mass]
// [fsi: paracetamol.Item.Dose.Total<..> Mass/Time]
// [fsi: paracetamol.Item.Dose.Rate<..> Mass/Time]
// [fsi: paracetamol.Item.DoseAdjust.QtyAdjust<..> Mass/Weight]
// [fsi: paracetamol.Item.DoseAdjust.TotalAdjust<..> Mass/Weight/Time]
// [fsi: paracetamol.Item.DoseAdjust.RateAdjust<..> Mass/Weight/Time]
// [fsi: paracetamol.Component.Component.Qty<..> Tabl]
// [fsi: paracetamol.Component.Orderable.Qty<..> Tabl]
// [fsi: paracetamol.Component.Orderable.Conc<..> Tabl/Tabl]
// [fsi: paracetamol.Component.Dose.Qty<..> Tabl]
// [fsi: paracetamol.Component.Dose.Total<..> Tabl/Time]
// [fsi: paracetamol.Component.Dose.Rate<..> Tabl/Time]
// [fsi: paracetamol.Component.DoseAdjust.QtyAdjust<..> Tabl/Weight]
// [fsi: paracetamol.Component.DoseAdjust.TotalAdjust<..> Tabl/Weight/Time]
// [fsi: paracetamol.Component.DoseAdjust.RateAdjust<..> Tabl/Weight/Time]
// [fsi: paracetamol.Orderable.Orderable.Qty<..> Tabl]
// [fsi: paracetamol.Orderable.Order.Qty<..> Tabl]
// [fsi: paracetamol.Orderable.Dose.Qty<..> Tabl]
// [fsi: paracetamol.Orderable.Dose.Total<..> Tabl/Time]
// [fsi: paracetamol.Orderable.Dose.Rate<..> Tabl/Time]
// [fsi: paracetamol.Orderable.DoseAdjust.QtyAdjust<..> Tabl/Weight]
// [fsi: paracetamol.Orderable.DoseAdjust.TotalAdjust<..> Tabl/Weight/Time]
// [fsi: paracetamol.Orderable.DoseAdjust.RateAdjust<..> Tabl/Weight/Time]
// [fsi: Adjust]
// [fsi: Adjust.Qty<..> Weight]

// Start entering values for the order while solving
// the calculation model the order at each step
let solve = OD.solve "paracetamol"
ord
|> solve MP.ItemComponentQty SV.Vals [240N; 300N; 500N] "mg"
|> solve MP.Freq SV.Vals [2N;3N;4N;5N;6N] "X/day"
|> solve MP.OrderableOrderableQty SV.Vals [1N] "tabl"
|> solve MP.OrderableDoseQty SV.Vals [1N] "tabl"
|> solve MP.ItemDoseTotal SV.MaxIncl [4N] "gram/day"
|> solve MP.ItemDoseAdjustTotalAdjust SV.MaxIncl [90N] "mg/kg/day"
|> solve MP.AdjustQty SV.Vals [10N] "kg"
|> print
|> ignore

// Prints:

// [fsi: Route]
// [fsi: oral]
// [fsi: Prescription]
// [fsi: Freq[1/43200, 1/28800] Count/Time]
// [fsi: Orderable]
// [fsi: paracetamol.Item.Component.Qty[6/25, 3/10] Mass]
// [fsi: paracetamol.Item.Orderable.Qty[6/25, 3/10] Mass]
// [fsi: paracetamol.Item.Component.Conc[6/25, 3/10] Mass/Tabl]
// [fsi: paracetamol.Item.Orderable.Conc[6/25, 3/10] Mass/Tabl]
// [fsi: paracetamol.Item.Dose.Qty[6/25, 3/10] Mass]
// [fsi: paracetamol.Item.Dose.Total[1/180000, 1/144000, 1/120000, 1/96000] Mass/Time]
// [fsi: paracetamol.Item.Dose.Rate<0..> Mass/Time]
// [fsi: paracetamol.Item.DoseAdjust.QtyAdjust[3/125000, 3/100000] Mass/Weight]
// [fsi: paracetamol.Item.DoseAdjust.TotalAdjust[1/1800000000, 1/1440000000, 1/1200000000, 1/960000000] Mass/Weight/Time]
// [fsi: paracetamol.Item.DoseAdjust.RateAdjust<0..> Mass/Weight/Time]
// [fsi: paracetamol.Component.Component.Qty[1] Tabl]
// [fsi: paracetamol.Component.Orderable.Qty[1] Tabl]
// [fsi: paracetamol.Component.Orderable.Conc[1] Tabl/Tabl]
// [fsi: paracetamol.Component.Dose.Qty[1] Tabl]
// [fsi: paracetamol.Component.Dose.Total[1/43200, 1/28800] Tabl/Time]
// [fsi: paracetamol.Component.Dose.Rate<0..> Tabl/Time]
// [fsi: paracetamol.Component.DoseAdjust.QtyAdjust[1/10000] Tabl/Weight]
// [fsi: paracetamol.Component.DoseAdjust.TotalAdjust[1/432000000, 1/288000000] Tabl/Weight/Time]
// [fsi: paracetamol.Component.DoseAdjust.RateAdjust<0..> Tabl/Weight/Time]
// [fsi: paracetamol.Orderable.Orderable.Qty[1] Tabl]
// [fsi: paracetamol.Orderable.Order.Qty[1/43200, 1/28800] Tabl]
// [fsi: paracetamol.Orderable.Dose.Qty[1] Tabl]
// [fsi: paracetamol.Orderable.Dose.Total[1/43200, 1/28800] Tabl/Time]
// [fsi: paracetamol.Orderable.Dose.Rate<0..> Tabl/Time]
// [fsi: paracetamol.Orderable.DoseAdjust.QtyAdjust<0..> Tabl/Weight]
// [fsi: paracetamol.Orderable.DoseAdjust.TotalAdjust<0..> Tabl/Weight/Time]
// [fsi: paracetamol.Orderable.DoseAdjust.RateAdjust<0..> Tabl/Weight/Time]
// [fsi: Adjust]
// [fsi: Adjust.Qty[10000] Weight]

(**
Some more info

Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/halcwb/GenOrder/tree/master/docs/content
  [gh]: https://github.com/halcwb/GenOrder
  [issues]: https://github.com/halcwb/GenOrder/issues
  [readme]: https://github.com/halcwb/GenOrder/blob/master/README.md
  [license]: https://github.com/halcwb/GenOrder/blob/master/LICENSE.txt
*)