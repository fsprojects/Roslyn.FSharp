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

type FSharpCompilation (checkProjectResults: FSharpCheckProjectResults, outputFile) =
    let assemblySignature = checkProjectResults.AssemblySignature

    interface ICompilation with
        member x.GetTypeByMetadataName(fullyQualifiedMetadataName:string) =
            let path =
                fullyQualifiedMetadataName.Split '.'
                |> Array.collect(fun s -> s.Split '+')
                |> List.ofArray

            let selfAndReferences =
                seq {
                    yield assemblySignature
                    yield! checkProjectResults.ProjectContext.GetReferencedAssemblies()
                           |> List.map(fun a -> a.Contents)
                }

            selfAndReferences
            |> Seq.tryPick(fun a -> a.FindEntityByPath path)
            |> Option.map(fun e -> FSharpNamedTypeSymbol(e) :> INamedTypeSymbol)
            |> Option.toObj

        member x.References =
            checkProjectResults.ProjectContext.GetReferencedAssemblies()
            //TODO: System.IO.FileNotFoundException: Could not load file or assembly 'FSharp.Core' or one of its dependencies
            |> Seq.filter (fun a -> a.SimpleName <> "FSharp.Core") 
            //TODO:System.IO.FileNotFoundException: Cannot resolve dependency to assembly because it has not been preloaded. When using the ReflectionOnly APIs, dependent assemblies must be pre-loaded or loaded on demand through the ReflectionOnlyAssemblyResolve event.
            // File name: 'netstandard, Version=2.0.0.0, Culture=neutral, PublicKeyToken=cc7b13ffcd2ddd51'
            |> Seq.filter (fun a -> a.SimpleName <> "netstandard") 
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
            FSharpAssemblySymbol(assemblySignature, outputFile) :> _

