namespace Roslyn.FSharp.Tests

open System.Linq
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open NUnit.Framework

/// These aren't really tests. They're just experiments for seeing how Roslyn handles C# so that I can compare with F#
module ``C# playground`` =
    let getCompilation(source:string) =
        let tree = CSharpSyntaxTree.ParseText source
        let compilation = 
            CSharpCompilation.Create("HelloWorld")
                .AddReferences(
                     MetadataReference.CreateFromFile(typeof<obj>.Assembly.Location))
                .AddSyntaxTrees(tree);
        compilation

    [<Test>]
    let ``property get methods``() =
        let compilation = 
            """
            public class MyClass
            {
                public int MyProperty { get; set; }
            }
            """
            |> getCompilation

        let namedType = compilation.GetTypeByMetadataName("MyClass")
        let methodNames =
            namedType.GetMembers().OfType<IMethodSymbol>().Select(fun m -> m.Name)
            |> List.ofSeq

        let metadataNames =
            namedType.GetMembers().OfType<IMethodSymbol>().Select(fun m -> m.MetadataName)
            |> List.ofSeq

        CollectionAssert.AreEqual(["get_MyProperty"; "set_MyProperty"; ".ctor"], methodNames)
        CollectionAssert.AreEqual(["get_MyProperty"; "set_MyProperty"; ".ctor"], metadataNames)

    [<Test>]
    let ``Assembly symbols``() =
        let compilation = getCompilation ""
        let reference = compilation.References.First()

        let asm = compilation.GetAssemblyOrModuleSymbol(reference) :?> IAssemblySymbol
        let z = asm.GlobalNamespace
        Assert.AreEqual("mscorlib", asm.Name)
