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