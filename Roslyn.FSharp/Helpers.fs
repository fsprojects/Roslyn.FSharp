namespace Roslyn.FSharp
open System
[<AutoOpen>]
module helpers = //TODO: I suck at naming
    let notImplemented() = raise (new NotImplementedException())