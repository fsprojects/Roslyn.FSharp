namespace Roslyn.FSharp.Tests
open System
open System.Linq
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open NUnit.Framework
open Roslyn.FSharp

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

    let getICompilation(source:string) =
        let compilation = getCompilation source
        CompilationWrapper(compilation) :> ICompilation

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
        Assert.AreEqual("mscorlib", asm.Name)

    [<Test>]
    let ``Global namespace GetNamespaceMembers``() =
        let compilation = getCompilation ""
        let reference = compilation.References.First()

        let asm = compilation.GetAssemblyOrModuleSymbol(reference) :?> IAssemblySymbol
        let namespaces =
            asm.GlobalNamespace.GetNamespaceMembers()

        let namespaces =
            namespaces
            |> Seq.map(fun n -> n.Name)
            |> List.ofSeq

        CollectionAssert.AreEqual(["Internal"; "Microsoft"; "Mono"; "System"; "XamMac"], namespaces)

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
        CollectionAssert.AreEqual(expected, attrs)

    [<Test>]
    let ``Attribute constructor arguments``() =
        let compilation =
            """
            namespace MyNamespace;
            using System;
            public class XmlnsDefinitionAttribute : Attribute
            {
                public XmlnsDefinitionAttribute(
                    string xmlNamespace,
                    string clrNamespace
                )
            }

            [XmlnsDefinition("xmlns", "MyNamespace")]
            public class MyDefinition
            {
            }
            """
            |> getICompilation

        let t = compilation.GetTypeByMetadataName("MyNamespace.MyDefinition") 
        let attrs = t.GetAttributes()

        let attr = attrs.First();
        let args = compilation.GetAttributeConstructorArguments attr
        Assert.AreEqual("xmlns", args.[0].Value)
        Assert.AreEqual("MyNamespace", args.[1].Value)

    [<Test>]
    let ``Attribute named arguments``() =
        let compilation =
            """
            namespace MyNamespace;
            using System;

            public class XmlnsDefinitionAttribute : Attribute
            {
                public string XmlNamespace { get; set; }
                public string ClrNamespace { get; set; }
            }

            [XmlnsDefinition(XmlNamespace="xmlns", ClrNamespace="MyNamespace")]
            public class MyDefinition
            {
            }
            """
            |> getICompilation

        let t = compilation.GetTypeByMetadataName("MyNamespace.MyDefinition") 
        let attrs = t.GetAttributes()

        let attr = attrs.First();
        let namedArguments = compilation.GetAttributeNamedArguments attr
        let expectedKeys = ["XmlNamespace"; "ClrNamespace"]
        let expectedValues = ["xmlns"; "MyNamespace"]

        CollectionAssert.AreEqual(expectedKeys, namedArguments.Select(fun kv -> kv.Key))
        CollectionAssert.AreEqual(expectedValues, namedArguments.Select(fun kv -> kv.Value.Value))