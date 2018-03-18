namespace Roslyn.FSharp
open System
open System.Collections.Immutable
open Microsoft.FSharp.Compiler.SourceCodeServices
[<AutoOpen>]
module helpers = //TODO: I suck at naming
    let notImplemented() = raise (new NotImplementedException())

    type FSharpType with
        member this.TypeDefinitionSafe() =
            match this.HasTypeDefinition with
            | true ->   Some this.TypeDefinition
            | false -> None

    let typeDefinitionSafe (typ: FSharpType) = typ.TypeDefinitionSafe()

module Seq =
    let toImmutableArray (sequence: 'a seq) =
        sequence.ToImmutableArray()

