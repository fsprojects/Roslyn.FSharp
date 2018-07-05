namespace Roslyn.FSharp

open System
open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices

type FSharpSyntaxTree(entity: FSharpEntity) =
    inherit SyntaxTree()
    override x.FilePath =
        match entity.ImplementationLocation with
        | Some range -> range.FileName
        /// The FilePath can be empty but never null
        /// https://github.com/dotnet/roslyn/blob/2868849409d44fcd5ecb84bef45575a98018a8c6/src/Compilers/Core/Portable/Syntax/SyntaxTree.cs#L28-L29
        | None -> ""
    override x.HasCompilationUnitRoot = notImplemented()
    override x.OptionsCore = notImplemented()
    override x.Length = notImplemented()
    override x.TryGetText(text: byref<Text.SourceText>) = notImplemented()
    override x.GetText(cancellationToken: Threading.CancellationToken) = notImplemented()
    override x.Encoding = notImplemented()
    override x.TryGetRootCore(root: byref<SyntaxNode>) = notImplemented()
    override x.GetRootCore(cancellationToken: Threading.CancellationToken) = notImplemented()
    override x.GetRootAsyncCore(cancellationToken: Threading.CancellationToken) = notImplemented()
    override x.WithChangedText(newText: Text.SourceText) = notImplemented()
    override x.GetDiagnostics(cancellationToken: Threading.CancellationToken) : Diagnostic seq = notImplemented()
    override x.GetDiagnostics(node: SyntaxNode) : Diagnostic seq = notImplemented()
    override x.GetDiagnostics(token: SyntaxToken) : Diagnostic seq = notImplemented()
    override x.GetDiagnostics(trivia: SyntaxTrivia) : Diagnostic seq = notImplemented()
    override x.GetDiagnostics(nodeOrToken: SyntaxNodeOrToken) : Diagnostic seq = notImplemented()
    override x.GetLineSpan(span: Text.TextSpan,cancellationToken: Threading.CancellationToken) = notImplemented()
    override x.GetMappedLineSpan(span: Text.TextSpan,cancellationToken: Threading.CancellationToken) = notImplemented()
    override x.HasHiddenRegions() = notImplemented()
    override x.GetChangedSpans(syntaxTree) = notImplemented()
    override x.GetLocation(span: Text.TextSpan) = notImplemented()
    override x.IsEquivalentTo(tree, topLevel) = notImplemented()
    override x.GetReference(node: SyntaxNode) = notImplemented()
    override x.GetChanges(oldTree) = notImplemented()
    override x.WithRootAndOptions(root: SyntaxNode, options: ParseOptions) = notImplemented()
    override x.WithFilePath(path: string) = notImplemented()

type FSharpSyntaxReference(entity) =
    inherit SyntaxReference()

    override x.SyntaxTree = FSharpSyntaxTree(entity) :> SyntaxTree
    override x.Span = notImplemented()
    override x.GetSyntax(cancellationToken) = notImplemented()