namespace Roslyn.FSharp.Tests

open Roslyn.FSharp
open NUnit.Framework

module ``Type symbol tests`` =
    [<Test>]
    let ``can get base type``() =
        let compilation = 
            """
            namespace MyNamespace
            type MyBaseType() = class end
            type MyType() =
                inherit MyBaseType()
            """
            |> getCompilation

        let namedType = compilation.GetTypeByMetadataName("MyNamespace.MyType")
        Assert.AreEqual("MyBaseType", namedType.BaseType.Name)

    [<Test>]
    let ``can get all base types``() =
        let compilation = 
            """
            namespace MyNamespace
            type MyBaseType1() = class end
            type MyBaseType2() =
                inherit MyBaseType1()
            type MyType() =
                inherit MyBaseType2()
            """
            |> getCompilation

        let namedType = compilation.GetTypeByMetadataName("MyNamespace.MyType")
        Assert.AreEqual("MyBaseType", namedType.BaseType.Name)