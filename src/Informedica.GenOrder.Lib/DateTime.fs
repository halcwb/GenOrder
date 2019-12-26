namespace Informedica.GenUtils.Lib.BCL


module DateTime =

    open System

    let formattedString (s: String) (dt : DateTime) =
        dt.ToString(s)
        

