﻿namespace Roslyn.FSharp.Tests

open Roslyn.FSharp
open Microsoft.CodeAnalysis
open NUnit.Framework

[<AutoOpen>]
module extenstions =
    type INamedTypeSymbol with
    // mimic the C# extension method that I see used everywhere
    member this.GetBaseTypesAndThis() =
        let rec getBaseTypesAndThis(current:INamedTypeSymbol) =
            [ yield current
              if not (isNull current.BaseType) then
                  yield! getBaseTypesAndThis current.BaseType ]
        getBaseTypesAndThis this

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

        let baseTypes =
            compilation.GetTypeByMetadataName("MyNamespace.MyType").GetBaseTypesAndThis()
            |> List.map(fun t -> t.Name)

        Assert.AreEqual(["MyType"; "MyBaseType2"; "MyBaseType1"; "obj"], baseTypes)
