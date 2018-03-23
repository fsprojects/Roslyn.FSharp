namespace Roslyn.FSharp.Tests

open System.IO
open Microsoft.CodeAnalysis
open Roslyn.FSharp

[<AutoOpen>]
module TestHelpers =
    let getCompilation input =
        let filename = "test.fsx"
        File.WriteAllText(filename, input)
        CompilationLoader.Load("test.fsproj", [filename], [])
        |> Async.AwaitTask
        |> Async.RunSynchronously

[<AutoOpen>]
module extensions =
    type INamedTypeSymbol with
        // mimic the C# extension method that I see used everywhere
        member this.GetBaseTypesAndThis() =
            let rec getBaseTypesAndThis(current:INamedTypeSymbol) =
                [ yield current
                  if not (isNull current.BaseType) then
                      yield! getBaseTypesAndThis current.BaseType ]
            getBaseTypesAndThis this

    type INamespaceSymbol with
        member x.GetFullName() =
            x.ToDisplayString (SymbolDisplayFormat.CSharpErrorMessageFormat)
