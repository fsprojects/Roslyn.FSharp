namespace Roslyn.FSharp
open Microsoft.CodeAnalysis

type ICompilation =
    abstract member GetTypeByMetadataName : fullyQualifiedMetadataName:string -> INamedTypeSymbol

type CompilationWrapper(compilation: Compilation) =
    interface ICompilation with
        member x.GetTypeByMetadataName(fullyQualifiedMetadataName:string) =
            compilation.GetTypeByMetadataName(fullyQualifiedMetadataName)

open Microsoft.FSharp.Compiler.SourceCodeServices
module scratch =

    let checker = FSharpChecker.Create()

    let parseAndCheckScript (file, input) = 
        let projOptions, errorList = 
            checker.GetProjectOptionsFromScript(file, input)
            |> Async.RunSynchronously

        checker.ParseAndCheckProject(projOptions) |> Async.RunSynchronously

type FSharpCompilation (checkProjectResults: FSharpCheckProjectResults) =
    let assemblySignature = checkProjectResults.AssemblySignature

    let allSymbols =
        checkProjectResults.GetAllUsesOfAllSymbols()
        |> Async.RunSynchronously

    interface ICompilation with
        member x.GetTypeByMetadataName(fullyQualifiedMetadataName:string) =
            assemblySignature.Entities
            |> Seq.tryFind(fun e -> e.FullName = fullyQualifiedMetadataName)
            |> function
               | Some e -> FSharpNamedTypeSymbol(e) :> INamedTypeSymbol
               | None -> null

