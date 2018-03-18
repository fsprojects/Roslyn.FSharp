namespace Roslyn.FSharp

open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices

type FSharpNamespaceOrTypeSymbol (entity:FSharpEntity) =
    inherit FSharpISymbol(entity, true, entity.DeclarationLocation)

    let memberToISymbol (m: FSharpMemberOrFunctionOrValue) : ISymbol =
        match m with
        | _ when m.IsProperty -> FSharpPropertySymbol(m) :> _
        | _ -> FSharpISymbol(m, true, m.DeclarationLocation) :> _

    let getMembers() =
        entity.TryGetMembersFunctionsAndValues
        |> Seq.map memberToISymbol

    interface INamespaceOrTypeSymbol with
        member x.IsNamespace = entity.IsNamespace

        member x.IsType = not entity.IsNamespace && not entity.IsArrayType
            //TODO: && not TypeParameter - how?
        member x.GetMembers () = getMembers() |> Seq.toImmutableArray

        member x.GetMembers (name) =
            getMembers()
            |> Seq.filter(fun m -> m.Name = name)
            |> Seq.toImmutableArray

        member x.GetTypeMembers () = notImplemented()

        member x.GetTypeMembers (name) = notImplemented()

        member x.GetTypeMembers (name, arity) = notImplemented()
