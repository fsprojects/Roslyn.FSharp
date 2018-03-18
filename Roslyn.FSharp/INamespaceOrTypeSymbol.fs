namespace Roslyn.FSharp

open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices

type FSharpNamespaceOrTypeSymbol (symbolUse:FSharpEntity) =
    inherit FSharpISymbol(symbolUse, true, symbolUse.DeclarationLocation)
    interface INamespaceOrTypeSymbol with
        member x.IsNamespace = symbolUse.IsNamespace

        member x.IsType = not symbolUse.IsNamespace && not symbolUse.IsArrayType
            // && not TypeParameter - how?
        member x.GetMembers () = notImplemented()

        member x.GetMembers (name) = notImplemented()

        member x.GetTypeMembers () = notImplemented()

        member x.GetTypeMembers (name) = notImplemented()

        member x.GetTypeMembers (name, arity) = notImplemented()
