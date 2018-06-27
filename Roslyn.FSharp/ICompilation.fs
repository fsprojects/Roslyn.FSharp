namespace Roslyn.FSharp

open Microsoft.CodeAnalysis

type IRoslynCompilation =
    abstract member GetTypeByMetadataName : fullyQualifiedMetadataName:string -> INamedTypeSymbol
    abstract member References : MetadataReference seq
    abstract member GetAssemblyOrModuleSymbol : MetadataReference -> ISymbol
    abstract member Assembly : IAssemblySymbol
    abstract member GlobalNamespace : INamespaceSymbol

