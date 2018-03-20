namespace Roslyn.FSharp.Tests

open Microsoft.CodeAnalysis
open Roslyn.FSharp
open NUnit.Framework

module ``Compilation tests`` =
    [<Test>]
    let ``can get type by metadata name``() =
        let compilation = 
            """
            namespace MyNamespace
            type MyType() = class end
            """
            |> getCompilation

        let namedType = compilation.GetTypeByMetadataName("MyNamespace.MyType")
        Assert.AreEqual("MyType", namedType.Name)
        Assert.AreEqual("MyType", namedType.MetadataName)


    [<Test>]
    let ``can get compilation name``() =
        let compilation = getCompilation ""
        let mscorlib =
            compilation.References |> Seq.find(fun r -> r.Display.EndsWith "mscorlib.dll")
        let name = compilation.GetAssemblyOrModuleSymbol(mscorlib).Name
        Assert.AreEqual("mscorlib", name)

    [<Test>]
    let ``global namespace``() =
        let compilation =
            """
            type GlobalType() =
                member x.X = 1
            """
            |> getCompilation

        let mscorlib =
            compilation.References |> Seq.find(fun r -> r.Display.EndsWith "mscorlib.dll")
        let assembly = compilation.GetAssemblyOrModuleSymbol(mscorlib) :?> IAssemblySymbol
        Assert.AreEqual(assembly.GlobalNamespace.IsGlobalNamespace, true)



