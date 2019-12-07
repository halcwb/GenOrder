namespace Informedica.GenOrder.Lib

/// Helper functions to 
/// facilitate the use of the
/// `Informedica.GenSolver.Lib`
module ValueUnit =

    open Informedica.GenUtils.Lib
    open Informedica.GenUnits.Lib

    let unitToString = ValueUnit.Units.toString ValueUnit.Units.Dutch ValueUnit.Units.Short                     