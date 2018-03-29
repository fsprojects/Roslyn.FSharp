namespace Roslyn.FSharp

open System
open System.Collections.Immutable
open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices

type FSharpSymbolBase () as this =
    abstract member MetadataName : string
    /// This should be overriden to provide LogicalName to include Generic information
    default this.MetadataName = notImplemented()

    abstract member ContainingNamespace : INamespaceSymbol
    default this.ContainingNamespace = notImplemented()

    /// Override with CompiledName so that we get ".ctor" and "get_PropertyGetter" etc
    abstract member Name : string
    default this.Name = notImplemented()

    abstract member DeclaredAccessibility : Accessibility
    default this.DeclaredAccessibility = notImplemented()

    abstract member GetDocumentationCommentId : unit -> string
    default this.GetDocumentationCommentId() = notImplemented()

    abstract member GetDocumentationCommentXml :  Globalization.CultureInfo * bool * Threading.CancellationToken -> string
    default this.GetDocumentationCommentXml(_culture, _expand, _token) = notImplemented()

    abstract member ToMinimalDisplayString : SemanticModel * int * SymbolDisplayFormat -> string
    default this.ToMinimalDisplayString(_semanticModel, _position, _format) = this.Name

    abstract member GetAttributes : unit -> ImmutableArray<AttributeData>
    default this.GetAttributes() = ImmutableArray.Empty

    abstract member Kind : SymbolKind
    default this.Kind = SymbolKind.ErrorType

    override x.ToString() = this.Name

    /// We can't implement IEquatable<Symbol> on the various symbol types due to:
    /// ```
    /// Error FS0443: This type implements the same interface at different generic instantiations 'IEquatable<ISymbol>' and 'IEquatable<FSharpTypeSymbol>'.
    /// This is not permitted in this version of F#.
    /// ```
    /// Overriding this member seems to be the best that we can do.
    abstract member CommonEquals : ISymbol -> bool
    default this.CommonEquals(symbol:ISymbol) = false

    abstract member ToDisplayString : SymbolDisplayFormat -> string
    default this.ToDisplayString(_format) = this.Name
    abstract member ContainingType : INamedTypeSymbol
    default this.ContainingType = notImplemented()

    interface ISymbol with
        member x.Kind = this.Kind
        member x.Language = "F#"
        member x.Name = this.Name
        member x.MetadataName = this.MetadataName
        member x.ContainingSymbol = null //TODO
        member x.ContainingAssembly = null //TODO
        member x.ContainingModule = null //TODO
        member x.ContainingType = this.ContainingType
        member x.ContainingNamespace = this.ContainingNamespace
        member x.IsDefinition = false
        member x.IsStatic = false //TODO
        member x.IsVirtual = false //TODO
        member x.IsOverride = false //TODO
        member x.IsAbstract = false //TODO
        member x.IsSealed = false //TODO
        member x.IsExtern = false //TODO
        member x.IsImplicitlyDeclared = false //TODO
        member x.CanBeReferencedByName = true //TODO
        member x.Locations = ImmutableArray.Empty //TODO
        member x.DeclaringSyntaxReferences = ImmutableArray.Empty //TODO
        member x.GetAttributes () = this.GetAttributes()
        member x.DeclaredAccessibility = this.DeclaredAccessibility
        member x.OriginalDefinition = x :> ISymbol
        member x.Accept (_visitor:SymbolVisitor) = () //TODO
        member x.Accept<'a> (_visitor: SymbolVisitor<'a>) = Unchecked.defaultof<'a>
        member x.GetDocumentationCommentId () = this.GetDocumentationCommentId()
        member x.GetDocumentationCommentXml (_culture, _expand, _token) =
            this.GetDocumentationCommentXml(_culture, _expand, _token)
        member x.ToDisplayString format = this.ToDisplayString(format)
        member x.ToDisplayParts _format = ImmutableArray.Empty //TODO
        member x.ToMinimalDisplayString (_semanticModel, _position, _format) =
            this.ToMinimalDisplayString(_semanticModel, _position, _format)
        member x.ToMinimalDisplayParts (_semanticModel, _position, _format) = ImmutableArray.Empty //TODO
        member x.HasUnsupportedMetadata = false //TODO
        member x.Equals (other:ISymbol) = this.CommonEquals other

type FSharpISymbol (symbol:FSharpSymbol) =
    inherit FSharpSymbolBase()

    override this.Name = symbol.DisplayName

    override this.DeclaredAccessibility =
        let accessibility =
            match symbol with
            | :? FSharpEntity as e -> e.Accessibility
            | :? FSharpMemberOrFunctionOrValue as m -> m.Accessibility
            | _ -> invalidArg "symbol"  "Symbol was of a type not containing accessibility information"

        if accessibility.IsPublic then Accessibility.Public
        elif accessibility.IsInternal then Accessibility.Internal
        else Accessibility.Private
