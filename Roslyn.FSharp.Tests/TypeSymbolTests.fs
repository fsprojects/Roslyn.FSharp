namespace Roslyn.FSharp.Tests

open System.Linq
open Roslyn.FSharp
open Microsoft.CodeAnalysis
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

        let baseTypes =
            compilation.GetTypeByMetadataName("MyNamespace.MyType").GetBaseTypesAndThis()
            |> List.map(fun t -> t.Name)

        CollectionAssert.AreEqual(["MyType"; "MyBaseType2"; "MyBaseType1"; "obj"], baseTypes)

    [<Test>]
    let ``can get properties``() =
        let compilation = 
            """
            namespace MyNamespace
            type MyType() =
                member x.A = 1
                member x.B = 2
                member x.C = 3
                member x.Method() = 4
            """
            |> getCompilation

        let properties = 
            compilation.GetTypeByMetadataName("MyNamespace.MyType").GetMembers().OfType<IPropertySymbol>()
            |> Seq.map(fun t -> t.Name)
            |> Seq.sort
            |> List.ofSeq

        CollectionAssert.AreEqual(["A"; "B"; "C"], properties)

    [<Test>]
    let ``can get methods``() =
        let compilation =
            """
            namespace MyNamespace
            type MyType() =
                member x.A = 1
                member x.B = 2
                member x.C = 3
                member x.Method() = 4
            """
            |> getCompilation

        let methods =
            compilation.GetTypeByMetadataName("MyNamespace.MyType").GetMembers().OfType<IMethodSymbol>()
            |> Seq.map(fun t -> t.Name)
            |> Seq.sort
            |> List.ofSeq

        CollectionAssert.AreEqual([".ctor"; "Method"; "get_A"; "get_B"; "get_C"], methods)

    [<Test>]
    let ``can get constructor``() =
        let compilation =
            """
            namespace MyNamespace
            type MyType() =
                member x.A = 1
                member x.B = 2
                member x.C = 3
                member x.Method() = 4
            """
            |> getCompilation

        let constructors =
            compilation.GetTypeByMetadataName("MyNamespace.MyType").Constructors

        Assert.AreEqual(1, constructors.Length)

    [<Test>]
    let ``can get property type``() =
        let compilation =
            """
            namespace MyNamespace
            type MyType() =
                member x.A = 1
                member x.B = 2
                member x.C = 3
                member x.Method() = 4
            """
            |> getCompilation

        let property =
            compilation.GetTypeByMetadataName("MyNamespace.MyType").GetMembers("A").OfType<IPropertySymbol>()
            |> Seq.head

        Assert.AreEqual("int", property.Type.Name)

    [<Test>]
    let ``can get nested type``() =
        let compilation =
            """
            namespace MyNamespace
            module myModule =
                type MyType() =
                    member x.A = 1
            """
            |> getCompilation

        let nestedType =
            compilation.GetTypeByMetadataName("MyNamespace.myModule").GetTypeMembers("MyType")
            |> Seq.head

        Assert.AreEqual("MyType", nestedType.Name)

    [<Test>]
    let ``IDictionary metadata name is IDictionary`2``() =
        let compilation =
            """
            namespace MyNamespace
            type MyType() =
                member x.Dict = System.Collections.Generic.Dictionary<string, string>()
            """
            |> getCompilation

        let dictionaryInterfaces =
            compilation.GetTypeByMetadataName("MyNamespace.MyType").GetMembers("Dict").OfType<IPropertySymbol>().First().Type.AllInterfaces

        let metadataNames = dictionaryInterfaces |> Seq.map(fun i -> i.MetadataName)
        let interfaceNamespaces = dictionaryInterfaces |> Seq.map(fun i -> i.ContainingNamespace.GetFullName())

        CollectionAssert.Contains(metadataNames, "IDictionary`2")
        CollectionAssert.Contains(interfaceNamespaces, "System.Collections.Generic")

