namespace Roslyn.FSharp
open Microsoft.CodeAnalysis

type ICompilation =
    abstract member GetTypeByMetadataName : fullyQualifiedMetadataName:string -> INamedTypeSymbol

/// Microsoft.CodeAnalysis.Compilation constructor is internal,
/// so we can't inherit from it. 
/// Instead, we can create a wrapper that implements our interface
/// that mirrors Compilation methods that we have working for F#
type CompilationWrapper(compilation: Compilation) =
    interface ICompilation with
        member x.GetTypeByMetadataName(fullyQualifiedMetadataName:string) =
            compilation.GetTypeByMetadataName(fullyQualifiedMetadataName)

open Microsoft.FSharp.Compiler.SourceCodeServices

type FSharpCompilation (checkProjectResults: FSharpCheckProjectResults) =
    let assemblySignature = checkProjectResults.AssemblySignature

    interface ICompilation with
        member x.GetTypeByMetadataName(fullyQualifiedMetadataName:string) =
            assemblySignature.Entities
            |> Seq.tryFind(fun e -> e.FullName = fullyQualifiedMetadataName)
            |> Option.map(fun e -> FSharpNamedTypeSymbol(e) :> INamedTypeSymbol)
            |> Option.toObj
