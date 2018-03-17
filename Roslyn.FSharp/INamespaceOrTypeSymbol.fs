namespace Roslyn.FSharp

open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices

type FSharpNamespaceOrTypeSymbol (symbolUse:FSharpSymbol) =
    inherit FSharpISymbol(symbolUse, false, [])
    interface INamespaceOrTypeSymbol with
        member x.IsNamespace = notImplemented()

        member x.IsType = notImplemented()

        member x.GetMembers () = notImplemented()

        member x.GetMembers (name) = notImplemented()

        member x.GetTypeMembers () = notImplemented()

        member x.GetTypeMembers (name) = notImplemented()

        member x.GetTypeMembers (name, arity) = notImplemented()
