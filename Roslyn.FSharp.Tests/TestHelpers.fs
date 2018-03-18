namespace Roslyn.FSharp.Tests

open System.IO
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

