namespace Roslyn.FSharp.Tests

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



