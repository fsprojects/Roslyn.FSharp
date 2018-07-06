namespace Roslyn.FSharp

open System
open System.Threading
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.FSharp.Compiler.SourceCodeServices

[<AutoOpen>]
module exceptions = //TODO: I suck at naming
    let notImplemented() = raise (new NotImplementedException())

type FSharpSyntaxTree(sourceText: SourceText, filePath) = //TODO: make sourceText lazy
    inherit SyntaxTree()
    override x.FilePath = filePath
    override x.HasCompilationUnitRoot = notImplemented()
    override x.OptionsCore = notImplemented()
    override x.Length = sourceText.Length
    override x.TryGetText(text: byref<SourceText>) =
        text <- sourceText
        true
    override x.GetText(cancellationToken: CancellationToken) =
        sourceText
    override x.Encoding = sourceText.Encoding
    override x.TryGetRootCore(root: byref<SyntaxNode>) = notImplemented()
    override x.GetRootCore(cancellationToken: CancellationToken) = notImplemented()
    override x.GetRootAsyncCore(cancellationToken: CancellationToken) = notImplemented()
    override x.WithChangedText(newText: SourceText) =
        FSharpSyntaxTree(newText, filePath) :> _
    override x.GetDiagnostics(cancellationToken: CancellationToken) : Diagnostic seq = notImplemented()
    override x.GetDiagnostics(node: SyntaxNode) : Diagnostic seq = notImplemented()
    override x.GetDiagnostics(token: SyntaxToken) : Diagnostic seq = notImplemented()
    override x.GetDiagnostics(trivia: SyntaxTrivia) : Diagnostic seq = notImplemented()
    override x.GetDiagnostics(nodeOrToken: SyntaxNodeOrToken) : Diagnostic seq = notImplemented()
    override x.GetLineSpan(span: TextSpan,cancellationToken: CancellationToken) = notImplemented()
    override x.GetMappedLineSpan(span: TextSpan,cancellationToken: CancellationToken) = notImplemented()
    override x.HasHiddenRegions() = notImplemented()
    override x.GetChangedSpans(syntaxTree) = notImplemented()
    override x.GetLocation(span: TextSpan) = Location.Create(x, span)
    override x.IsEquivalentTo(tree, topLevel) = notImplemented()
    override x.GetReference(node: SyntaxNode) = notImplemented()
    override x.GetChanges(oldTree) = notImplemented()
    override x.WithRootAndOptions(root: SyntaxNode, options: ParseOptions) = notImplemented()
    override x.WithFilePath(path: string) =
        FSharpSyntaxTree(sourceText, path) :> _

type FSharpSyntaxReference(entity:FSharpEntity) =
    inherit SyntaxReference()

    let sourceText = SourceText.From "" //TODO:
    override x.SyntaxTree =
        let filePath =
            match entity.ImplementationLocation with
            | Some range -> range.FileName
            /// The FilePath can be empty but never null
            /// https://github.com/dotnet/roslyn/blob/2868849409d44fcd5ecb84bef45575a98018a8c6/src/Compilers/Core/Portable/Syntax/SyntaxTree.cs#L28-L29
            | None -> ""

        FSharpSyntaxTree(sourceText, filePath) :> SyntaxTree

    override x.Span = notImplemented()
    override x.GetSyntax(cancellationToken) = notImplemented()