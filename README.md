
### Roslyn.FSharp

Implements a Roslyn read-only API to work with F# code (via bridge to FSharp.Compiler.Service).

### Usage

See sample:

```
open System.IO
open Microsoft.CodeAnalysis
open Roslyn.FSharp

let filename = "test.fsx"
File.WriteAllText(filename, "let x = 1")
let compilation = 
    CompilationLoader.Load("test.fsproj", [filename], [])
    |> Async.AwaitTask
    |> Async.RunSynchronously
```

### Notes

* Could be called `Microsoft.CodeAnalysis.FSharp`.

* Some operations report `NotImplementedException`, search for `notImplemented()` in this repo.

* Implementing Roslyn's `ISymbol` types may require an exact match in Roslyn binaries.  As a result, this component is
  not published as a nuget pacakge with static binary dependencies but rather you must use direct source inclusion, e.g. via a
  paket github reference or a git submodule.
