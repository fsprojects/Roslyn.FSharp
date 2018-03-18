namespace Roslyn.FSharp

open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices

type FSharpNamespaceOrTypeSymbol (entity:FSharpEntity) =
    inherit FSharpISymbol(entity, true, entity.DeclarationLocation)
    interface INamespaceOrTypeSymbol with
        member x.IsNamespace = entity.IsNamespace

        member x.IsType = not entity.IsNamespace && not entity.IsArrayType
            //TODO: && not TypeParameter - how?
        member x.GetMembers () = notImplemented()

        member x.GetMembers (name) = notImplemented()

        member x.GetTypeMembers () = notImplemented()

        member x.GetTypeMembers (name) = notImplemented()

        member x.GetTypeMembers (name, arity) = notImplemented()
