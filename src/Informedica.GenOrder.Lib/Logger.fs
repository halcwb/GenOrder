namespace Informedica.GenOrder.Lib

module Logger =

    open Informedica.GenSolver.Lib.Logger

    type Message =
        | SolverReplacingUnits of obj
        | OrderSolved of obj
        | OrderWithConstraintsSolved of obj
        | Scenario of obj
        | ScenerioValue of obj
        interface IMessage


    let createMessage o (m : obj -> Message) : IMessage = 
        o  |> box |> m :> IMessage

    let logMessage level (logger : Logger) msg = 
        logger.LogMessage level msg

    let logInfo logger msg = logMessage Informative logger msg

    let logWarning logger msg = logMessage Warning logger msg

    let logError logger msg = logMessage Error logger msg