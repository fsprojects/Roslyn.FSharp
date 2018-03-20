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