namespace Roslyn.FSharp

module CompilationLoader =
    open System
    open System.IO
    open Microsoft.FSharp.Compiler.SourceCodeServices

    let private checker = FSharpChecker.Create()

    /// Super simple projectOptions builder for now
    let Load(projectFileName: string, sourceFiles: string seq, references: string seq) =
        async {
            let outputFile = Path.ChangeExtension(projectFileName, ".dll")

            let otherOptions =
                [| yield "--simpleresolution"
                   yield "--noframework"
                   yield "--debug:full"
                   yield "--define:DEBUG"
                   yield "--optimize-"
                   yield "--out:" + outputFile
                   yield "--warn:3"
                   yield "--fullpaths"
                   yield "--flaterrors"
                   yield "--target:library"
                   for r in references do
                       yield "-r:" + r |]

            let projectOptions =
                { ProjectFileName = projectFileName
                  SourceFiles = sourceFiles |> Array.ofSeq
                  OtherOptions = otherOptions
                  ReferencedProjects = [||]
                  IsIncompleteTypeCheckEnvironment = false
                  UseScriptResolutionRules = false
                  LoadTime = DateTime.Now
                  UnresolvedReferences = None
                  OriginalLoadReferences = []
                  ExtraProjectInfo = None
                  Stamp = None }

            let! checkResults = checker.ParseAndCheckProject(projectOptions)
            return FSharpCompilation(checkResults, outputFile) :> ICompilation
        } |> Async.StartAsTask
