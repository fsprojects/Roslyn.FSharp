namespace Roslyn.FSharp
open Microsoft.CodeAnalysis

type ICompilation =
    abstract member GetTypeByMetadataName : fullyQualifiedMetadataName:string -> INamedTypeSymbol
    abstract member References : MetadataReference seq
    abstract member GetAssemblyOrModuleSymbol : MetadataReference -> ISymbol
    abstract member Assembly : IAssemblySymbol

/// Microsoft.CodeAnalysis.Compilation constructor is internal,
/// so we can't inherit from it. 
/// Instead, we can create a wrapper that implements our interface
/// that mirrors Compilation methods that we have working for F#
type CompilationWrapper(compilation: Compilation) =
    interface ICompilation with
        member x.GetTypeByMetadataName(fullyQualifiedMetadataName:string) =
            compilation.GetTypeByMetadataName(fullyQualifiedMetadataName)
        member x.References = compilation.References
        member x.GetAssemblyOrModuleSymbol(reference) = compilation.GetAssemblyOrModuleSymbol(reference)
        member x.Assembly = compilation.Assembly

open Microsoft.FSharp.Compiler.SourceCodeServices

type FSharpCompilation (checkProjectResults: FSharpCheckProjectResults) =
    let assemblySignature = checkProjectResults.AssemblySignature

    interface ICompilation with
        member x.GetTypeByMetadataName(fullyQualifiedMetadataName:string) =
            let path =
                fullyQualifiedMetadataName.Split '.'
                |> List.ofArray

            assemblySignature.FindEntityByPath path 
            |> Option.map(fun e -> FSharpNamedTypeSymbol(e) :> INamedTypeSymbol)
            |> Option.toObj

        member x.References =
            checkProjectResults.ProjectContext.GetReferencedAssemblies()
            |> Seq.choose (fun asm -> asm.FileName)
            |> Seq.map(fun fileName -> MetadataReference.CreateFromFile (fileName) :> MetadataReference)

        member x.GetAssemblyOrModuleSymbol(reference) =
            let fsharpAssembly =
                checkProjectResults.ProjectContext.GetReferencedAssemblies()
                |> List.find(fun a ->
                    a.FileName
                    |> Option.exists(fun f -> f = reference.Display))
            //TODO: handle ModuleSymbol
            FSharpAssemblySymbol(fsharpAssembly) :> ISymbol

        member x.Assembly =
            //TODO: Is there a better way to get the current FSharpAssembly?
            assemblySignature.Entities
            |> Seq.tryHead
            |> function
               | Some e -> FSharpAssemblySymbol(e.Assembly) :> _
               | None -> failwith "Couldn't find any entities"

