namespace Roslyn.FSharp

open System
open System.Collections.Immutable
open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices

type FSharpISymbol (symbol:FSharpSymbol, isFromDefinition, location) as this =
    abstract member MetadataName : string
    /// This should be overriden to provide LogicalName to include Generic information
    default this.MetadataName = notImplemented()

    abstract member ContainingNamespace : INamespaceSymbol
    default this.ContainingNamespace = notImplemented()

    /// Override with CompiledName so that we get ".ctor" and "get_PropertyGetter" etc
    abstract member Name : string
    default this.Name = symbol.DisplayName

    interface ISymbol with
        member x.Kind = SymbolKind.Local
        member x.Language = "F#"
        member x.Name = this.Name
        member x.MetadataName = this.MetadataName
        member x.ContainingSymbol = null //TODO
        member x.ContainingAssembly = null //TODO
        member x.ContainingModule = null //TODO
        member x.ContainingType = null ////TODO for entities or functions this will be available
        member x.ContainingNamespace = this.ContainingNamespace
        member x.IsDefinition = isFromDefinition
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
        member x.GetAttributes () = ImmutableArray.Empty //TODO
        member x.DeclaredAccessibility =
            let accessibility =
                match symbol with
                | :? FSharpEntity as e -> e.Accessibility
                | :? FSharpMemberOrFunctionOrValue as m -> m.Accessibility
                | _ -> invalidArg "symbol"  "Symbol was of a type not containing accessibility information"

            if accessibility.IsPublic then Accessibility.Public
            elif accessibility.IsInternal then Accessibility.Internal
            else Accessibility.Private

        member x.OriginalDefinition = x :> ISymbol
        member x.Accept (_visitor:SymbolVisitor) = () //TODO
        member x.Accept<'a> (_visitor: SymbolVisitor<'a>) = Unchecked.defaultof<'a>
        member x.GetDocumentationCommentId () =
            match symbol with
            | :? FSharpEntity as e -> e.XmlDocSig
            | :? FSharpMemberOrFunctionOrValue as m -> m.XmlDocSig
            | _ -> invalidArg "symbol"  "Symbol was of a type not containing a documentation comment"
        member x.GetDocumentationCommentXml (_culture, _expand, _token) =
            let xmlDoc =
                match symbol with
                | :? FSharpEntity as e -> e.XmlDoc
                | :? FSharpMemberOrFunctionOrValue as m -> m.XmlDoc
                | _ -> invalidArg "symbol"  "Symbol was of a type not containing a documentation comment"
            String.concat "\n" xmlDoc

        member x.ToDisplayString _format = symbol.DisplayName //TODO format?
        member x.ToDisplayParts _format = ImmutableArray.Empty //TODO
        member x.ToMinimalDisplayString (_semanticModel, _position, _format) = symbol.DisplayName //TODO format?
        member x.ToMinimalDisplayParts (_semanticModel, _position, _format) = ImmutableArray.Empty //TODO
        member x.HasUnsupportedMetadata = false //TODO
        member x.Equals (other:ISymbol) = x.Equals(other)