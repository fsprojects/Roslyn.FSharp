namespace Roslyn.FSharp.Tests

open System.Linq
open Microsoft.CodeAnalysis
open Roslyn.FSharp
open NUnit.Framework

open Microsoft.FSharp.Compiler.SourceCodeServices
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
    let ``GetTypeByMetadataName returns types from references``() =
        let compilation = getCompilation ""
        let dictionary = compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")
        Assert.AreEqual("List", dictionary.Name)

    [<Test>]
    let ``Named types are equal``() =
        let compilation = getCompilation ""
        let list1 = compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")
        let list2 = compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")
        Assert.AreEqual (list1, list2)

    [<Test>]
    let ``NamespaceTypes are equal``() =
        let compilation = getCompilation ""
        let list1 = compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")
        let list2 = compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")
        Assert.AreEqual (list1.ContainingNamespace, list2.ContainingNamespace)

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

    [<Test>]
    let ``Global namespace GetNamespaceMembers``() =
        let compilation = getCompilation ""
        let mscorlib =
            compilation.References |> Seq.find(fun r -> r.Display.EndsWith "mscorlib.dll")

        let asm = compilation.GetAssemblyOrModuleSymbol(mscorlib) :?> IAssemblySymbol
        let namespaces =
            asm.GlobalNamespace.GetNamespaceMembers()

        let namespaces =
            namespaces
            |> Seq.map(fun n -> n.Name)
            |> List.ofSeq

        printfn "%A" namespaces
        CollectionAssert.AreEqual(["Internal"; "Microsoft"; "Mono"; "System"; "XamMac"], namespaces)

    [<Test>]
    let ``Nested namespace GetNamespaceMembers``() =
        let compilation = getCompilation ""
        let mscorlib =
            compilation.References |> Seq.find(fun r -> r.Display.EndsWith "mscorlib.dll")

        let asm = compilation.GetAssemblyOrModuleSymbol(mscorlib) :?> IAssemblySymbol
        let systemNamespace =
            asm.GlobalNamespace.GetNamespaceMembers()
            |> Seq.find(fun n -> n.Name = "System")

        let namespaces =
            systemNamespace.GetNamespaceMembers()
            |> Seq.map(fun n -> n.Name)
            |> List.ofSeq

        printfn "%A" namespaces 
        let expected =
            ["Buffers"; "Collections"; "Configuration"; "Deployment"; "Diagnostics";
             "Globalization"; "IO"; "Numerics"; "Reflection"; "Resources"; "Runtime";
             "Security"; "Text"; "Threading"]
        CollectionAssert.AreEqual(expected, namespaces)

    [<Test>]
    let ``Deeply nested namespace GetNamespaceMembers``() =
        let compilation = getCompilation ""
        let mscorlib =
            compilation.References |> Seq.find(fun r -> r.Display.EndsWith "mscorlib.dll")

        let asm = compilation.GetAssemblyOrModuleSymbol(mscorlib) :?> IAssemblySymbol
        let systemNamespace =
            asm.GlobalNamespace.GetNamespaceMembers()
            |> Seq.find(fun n -> n.Name = "System")

        let collectionsNamespace =
            systemNamespace.GetNamespaceMembers()
            |> Seq.find(fun n -> n.Name = "Collections")

        let namespaces =
            collectionsNamespace.GetNamespaceMembers()
            |> Seq.map(fun n -> n.Name)
            |> List.ofSeq

        printfn "%A" namespaces 
        CollectionAssert.AreEqual(["Concurrent"; "Generic"; "ObjectModel"], namespaces)

    [<Test>]
    let ``nested namespace types``() =
        let getNamespaceMembers (ns: INamespaceSymbol) =
            ns.GetNamespaceMembers()

        let compilation = getCompilation ""
        let mscorlib =
            compilation.References |> Seq.find(fun r -> r.Display.EndsWith "mscorlib.dll")

        let asm = compilation.GetAssemblyOrModuleSymbol(mscorlib) :?> IAssemblySymbol
        let genericCollectionsNamespace =
            asm.GlobalNamespace
            |> getNamespaceMembers
            |> Seq.find(fun n -> n.Name = "System")
            |> getNamespaceMembers
            |> Seq.find(fun n -> n.Name = "Collections")
            |> getNamespaceMembers
            |> Seq.find(fun n -> n.Name = "Generic")

        let types =
            genericCollectionsNamespace.GetMembers()

        let typeNames =
            types
            |> Seq.map(fun n -> n.MetadataName)
            |> List.ofSeq

        printfn "%A" types
        CollectionAssert.IsSubsetOf(["Dictionary`2"; "IDictionary`2"; "List`1"], typeNames)
        CollectionAssert.AllItemsAreInstancesOfType(types, typeof<INamedTypeSymbol>)

    [<Test>]
    let ``Assembly NamespaceNames``() =
        let compilation = getCompilation ""
        let mscorlib = compilation.References.First()

        let asm = compilation.GetAssemblyOrModuleSymbol(mscorlib) :?> IAssemblySymbol
        let namespaces =
            asm.NamespaceNames
            |> List.ofSeq
            |> List.sort

        let expected =
            ["AccessControl"; "Activation"; "Assemblies"; "Augments"; "Authenticode";
             "Binary"; "Buffers"; "Channels"; "Claims"; "CodeAnalysis"; "Collections";
             "ComTypes"; "CompilerServices"; "Concurrent"; "Configuration";
             "ConstrainedExecution"; "Contexts"; "Contracts"; "CoreFoundation";
             "Cryptography"; "Deployment"; "Diagnostics"; "Emit"; "ExceptionServices";
             "Expando"; "Extensions"; "Formatters"; "Generator"; "Generic"; "Globalization";
             "Hashing"; "Hosting"; "IO"; "Internal"; "Interop"; "InteropServices";
             "IsolatedStorage"; "Lifetime"; "Math"; "Messaging"; "Metadata"; "Microsoft";
             "Mono"; "Numerics"; "ObjectModel"; "Permissions"; "Policy"; "Prime";
             "Principal"; "Private"; "Proxies"; "Reflection"; "Remoting"; "Resources";
             "Runtime"; "SafeHandles"; "Security"; "Serialization"; "Services";
             "SymbolStore"; "System"; "Tasks"; "Text"; "Threading"; "Tracing"; "Unicode";
             "Util"; "Versioning"; "W3cXsd2001"; "Win32"; "WindowsRuntime"; "X509";
             "X509Certificates"; "XamMac"; "Xml"]

        CollectionAssert.AreEqual(expected, namespaces)

    [<Test>]
    let ``Assembly TypeNames``() =
        let compilation = getCompilation ""
        let mscorlib = compilation.References.First()

        let asm = compilation.GetAssemblyOrModuleSymbol(mscorlib) :?> IAssemblySymbol
        let typeNames =
            asm.TypeNames
            |> List.ofSeq
            |> List.sort

        CollectionAssert.IsSubsetOf(["ASCIIEncoding"; "Action"; "File"; "Directory"], typeNames)

    [<Test>]
    let ``Assembly identity``() =
        let compilation = getCompilation ""
        let mscorlib = compilation.References.First()

        let asm = compilation.GetAssemblyOrModuleSymbol(mscorlib) :?> IAssemblySymbol
        Assert.AreEqual("mscorlib", asm.Identity.Name)

    [<Test>]
    let ``Assembly attributes``() =
        let compilation = getCompilation ""
        let mscorlib = compilation.References.First()

        let asm = compilation.GetAssemblyOrModuleSymbol(mscorlib) :?> IAssemblySymbol
        let attrs =
            asm.GetAttributes()
            |> Seq.map(fun a -> a.AttributeClass.Name)
            |> Seq.sort
            |> List.ofSeq

        printfn "%A" attrs
        let expected =
            ["AllowPartiallyTrustedCallersAttribute"; "AssemblyCompanyAttribute";
             "AssemblyCopyrightAttribute"; "AssemblyDefaultAliasAttribute";
             "AssemblyDelaySignAttribute"; "AssemblyDescriptionAttribute";
             "AssemblyFileVersionAttribute"; "AssemblyInformationalVersionAttribute";
             "AssemblyKeyFileAttribute"; "AssemblyProductAttribute";
             "AssemblyTitleAttribute"; "CLSCompliantAttribute";
             "ComCompatibleVersionAttribute"; "ComVisibleAttribute";
             "CompilationRelaxationsAttribute"; "DebuggableAttribute";
             "DefaultDependencyAttribute"; "GuidAttribute"; "InternalsVisibleToAttribute";
             "InternalsVisibleToAttribute"; "InternalsVisibleToAttribute";
             "InternalsVisibleToAttribute"; "InternalsVisibleToAttribute";
             "NeutralResourcesLanguageAttribute"; "RuntimeCompatibilityAttribute";
             "SatelliteContractVersionAttribute"; "StringFreezingAttribute"]

        CollectionAssert.IsSubsetOf(expected, attrs)

    [<Test>]
    let ``Attribute constructor arguments``() =
        let compilation =
            """
            namespace MyNamespace
            open System
            type XmlnsDefinitionAttribute(xmlNamespace, clrNamespace) =
                inherit Attribute()

            [<XmlnsDefinition("xmlns", "MyNamespace")>]
            type MyDefinition() = class end
            """
            |> getCompilation

        let t = compilation.GetTypeByMetadataName("MyNamespace.MyDefinition") 
        let attrs = t.GetAttributes()

        let attr = attrs.First();
        let args = compilation.GetAttributeConstructorArguments(attr)
        Assert.AreEqual("xmlns", args.[0].Value)
        Assert.AreEqual("MyNamespace", args.[1].Value)

    [<Test>]
    let ``Attribute constructor``() =
        let compilation =
            """
            namespace MyNamespace
            open System
            type XmlnsDefinitionAttribute(xmlNamespace, clrNamespace) =
                inherit Attribute()

                new() = XmlnsDefinitionAttribute("default", "default")

            [<XmlnsDefinition("xmlns", "MyNamespace")>]
            type MyDefinition() = class end
            """
            |> getCompilation

        let t = compilation.GetTypeByMetadataName("MyNamespace.MyDefinition") 
        let attrs = t.GetAttributes()

        let attr = attrs.First();
        Assert.AreEqual(2, attr.AttributeConstructor.Parameters.Length)

    [<Test>]
    let ``Attribute named arguments``() =
        let compilation =
            """
            namespace MyNamespace
            open System

            type XmlnsDefinitionAttribute() =
                inherit Attribute()

                member val XmlNamespace = "" with get, set
                member val ClrNamespace = "" with get, set

            [<XmlnsDefinition(XmlNamespace="xmlns", ClrNamespace="MyNamespace")>]
            type MyDefinition() = class end
            """
            |> getCompilation

        let t = compilation.GetTypeByMetadataName("MyNamespace.MyDefinition") 
        let attrs = t.GetAttributes()

        let attr = attrs.First();
        let namedArguments = compilation.GetAttributeNamedArguments attr
        let expectedKeys = ["XmlNamespace"; "ClrNamespace"]
        let expectedValues = ["xmlns"; "MyNamespace"]

        CollectionAssert.AreEqual(expectedKeys, namedArguments.Select(fun kv -> kv.Key))
        CollectionAssert.AreEqual(expectedValues, namedArguments.Select(fun kv -> kv.Value.Value))