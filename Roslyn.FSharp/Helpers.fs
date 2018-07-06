namespace Roslyn.FSharp
open System
open System.Collections.Generic
open System.Collections.Immutable
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library

[<AutoOpen>]
module helpers = //TODO: I suck at naming
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

    let rangeToLocation (range:Range.range) =
        match range.FileName with
        | "unknown" | "startup" ->
            Location.None
        | f when Shim.FileSystem.SafeExists f ->
            use stream = Shim.FileSystem.FileStreamReadShim(range.FileName)
            let text = SourceText.From(stream)
            let startPosition = new LinePosition(range.StartLine, range.StartColumn)
            let endPosition = new LinePosition(range.EndLine, range.EndColumn)
            let linePositionSpan = new LinePositionSpan(startPosition, endPosition)
            let tree = FSharpSyntaxTree(text, range.FileName)
            Location.Create(tree, text.Lines.GetTextSpan(linePositionSpan))
        | _ ->
            Location.None

module Seq =
    let inline toImmutableArray (sequence: 'a seq) =
        sequence.ToImmutableArray()

    let toCollection (sequence: 'a seq) =
        match sequence with
        | :? IList<'a> as list -> list :> ICollection<'a>
        | _ -> ResizeArray(sequence) :> ICollection<'a>

