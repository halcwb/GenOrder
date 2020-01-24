namespace Informedica.GenOrder.Lib


module Logging =

    open Informedica.GenSolver.Lib.Types.Logging


    module SolverLogging = Informedica.GenSolver.Lib.Logging
    module LoggingType = Informedica.GenSolver.Lib.Types.Logging
    

    let logInfo logger msg    = SolverLogging.logMessage Informative logger msg


    let logWarning logger msg = SolverLogging.logMessage Warning logger msg


    let logError logger msg   = SolverLogging.logMessage Error logger msg

            