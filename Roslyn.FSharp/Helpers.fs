namespace Roslyn.FSharp
open System
open System.Collections.Generic
open System.Collections.Immutable
open Microsoft.FSharp.Compiler.SourceCodeServices

[<AutoOpen>]
module helpers = //TODO: I suck at naming
    let notImplemented() = raise (new NotImplementedException())

    type FSharpType with
        member this.TypeDefinitionSafe =
            match this.HasTypeDefinition with
            | true -> Some this.TypeDefinition
            | false -> None

    let typeDefinitionSafe (typ: FSharpType) = typ.TypeDefinitionSafe

    type FSharpEntity with
        member this.AbbreviatedTypeSafe =
            match this.IsFSharpAbbreviation with
            | true -> Some this.AbbreviatedType
            | false -> None

    let pathFromFullyQualifiedMetadataName (fullyQualifiedMetadataName: string) =
        fullyQualifiedMetadataName.Split '.'
        |> Array.collect(fun s -> s.Split '+')
        |> List.ofArray

module Seq =
    let inline toImmutableArray (sequence: 'a seq) =
        sequence.ToImmutableArray()

    let toCollection (sequence: 'a seq) =
        match sequence with
        | :? IList<'a> as list -> list :> ICollection<'a>
        | _ -> ResizeArray(sequence) :> ICollection<'a>

