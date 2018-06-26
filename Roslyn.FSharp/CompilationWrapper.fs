namespace Roslyn.FSharp

open Microsoft.CodeAnalysis

/// Microsoft.CodeAnalysis.Compilation constructor is internal,
/// so we can't inherit from it. 
/// Instead, we can create a wrapper that implements our interface
/// that mirrors Compilation methods that we have working for F#
type CompilationWrapper(compilation: Compilation) =
    interface IRoslynCompilation with
        member x.GetTypeByMetadataName(fullyQualifiedMetadataName:string) =
            compilation.GetTypeByMetadataName(fullyQualifiedMetadataName)
        member x.References = compilation.References
        member x.GetAssemblyOrModuleSymbol(reference) = compilation.GetAssemblyOrModuleSymbol(reference)
        member x.Assembly = compilation.Assembly
