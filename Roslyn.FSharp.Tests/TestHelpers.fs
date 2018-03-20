namespace Roslyn.FSharp.Tests

open System.IO
open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices
open Roslyn.FSharp

[<AutoOpen>]
module TestHelpers =
    let getCompilation input =
        let checker = FSharpChecker.Create() 
        let filename = "test.fsx"
        File.WriteAllText(filename, input)
        let projOptions, _errors = 
            checker.GetProjectOptionsFromScript(filename, input)
            |> Async.RunSynchronously

        let checkResults = checker.ParseAndCheckProject(projOptions) |> Async.RunSynchronously

        FSharpCompilation(checkResults) :> ICompilation

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
