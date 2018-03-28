namespace Roslyn.FSharp.Tests

open System.IO
open Microsoft.CodeAnalysis
open Roslyn.FSharp

[<AutoOpen>]
module TestHelpers =
    let getCompilation input =
        let filename = "test.fsx"
        File.WriteAllText(filename, input)
        CompilationLoader.Load("test.fsproj", [filename], [])
        |> Async.AwaitTask
        |> Async.RunSynchronously

[<AutoOpen>]
module extensions =
    type INamedTypeSymbol with
        // mimic the C# extension method that I see used everywhere
        member this.GetBaseTypesAndThis() =
            let rec getBaseTypesAndThis(current:INamedTypeSymbol) =
                [ yield current
                  if not (isNull current.BaseType) then
                      yield! getBaseTypesAndThis current.BaseType ]
            getBaseTypesAndThis this

        /// Gets the full MetadataName(ReflectionName in NR5).
        /// Example: Namespace1.Namespace2.Classs1+NestedClassWithTwoGenericTypes`2+NestedClassWithoutGenerics
        member this.GetFullMetadataName() =
            let rec prependParentTypes (parentType:INamedTypeSymbol) res =
                match parentType with
                | null -> res
                | parent ->
                    printfn "containing type %A" parentType.ContainingType
                    prependParentTypes parentType.ContainingType
                        (sprintf "%s+%s" parentType.MetadataName res)

            let rec prependNamespaces (ns:INamespaceSymbol) res =
                match ns with
                | null -> res
                | _ when ns.IsGlobalNamespace -> res
                | _ ->
                    printfn "metadata name - %s" ns.MetadataName
                    prependNamespaces ns.ContainingNamespace
                        (sprintf "%s.%s" ns.MetadataName res)

            printfn "metadata name %s" this.MetadataName
            prependParentTypes this.ContainingType this.MetadataName
            |> prependNamespaces this.ContainingNamespace
// namespace2.namespace1+namespace2+MyGenericClass
    type INamespaceSymbol with
        member x.GetFullName() =
            x.ToDisplayString (SymbolDisplayFormat.CSharpErrorMessageFormat)
