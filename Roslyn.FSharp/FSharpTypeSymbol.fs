namespace Roslyn.FSharp

open System.Collections.Immutable

open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices


type FSharpNamespaceSymbol (entity:FSharpEntity) =
    inherit FSharpNamespaceOrTypeSymbol(entity)

    let getTypeMembers() =
        entity.NestedEntities
        |> Seq.map (fun e -> FSharpNamespaceOrTypeSymbol(e) :> INamespaceOrTypeSymbol)


    interface INamespaceSymbol with
        member x.ConstituentNamespaces = notImplemented()

        member x.ContainingCompilation = notImplemented()

        member x.IsGlobalNamespace = entity.FullName = "global"

        member x.NamespaceKind = notImplemented()

        member x.GetMembers () =
            getTypeMembers()

        member x.GetMembers (name) =
            getTypeMembers()
            |> Seq.filter(fun m -> m.Name = name)

        /// Get all the members of this symbol that are namespaces 
        member x.GetNamespaceMembers () =
            entity.NestedEntities
            |> Seq.filter(fun m -> m.IsNamespace)
            |> Seq.map(fun n -> FSharpNamespaceSymbol(n) :> INamespaceSymbol)

and FSharpTypeSymbol (entity:FSharpEntity) =
    inherit FSharpNamespaceOrTypeSymbol(entity)

    let namedTypeFromEntity (entity:FSharpEntity) =
        FSharpNamedTypeSymbol(entity) :> INamedTypeSymbol

    interface ITypeSymbol with
        member x.AllInterfaces =
            entity.AllInterfaces
            |> Seq.choose typeDefinitionSafe
            |> Seq.map namedTypeFromEntity
            |> Seq.toImmutableArray

        member x.BaseType =
            match entity.BaseType with
            | Some baseType ->
                typeDefinitionSafe baseType
                |> Option.map namedTypeFromEntity
                |> Option.toObj
            | None -> null

        member x.Interfaces =
            entity.DeclaredInterfaces
            |> Seq.choose typeDefinitionSafe
            |> Seq.map namedTypeFromEntity
            |> Seq.toImmutableArray

        member x.IsAnonymousType = false

        member x.IsReferenceType = not entity.IsValueType && not entity.IsFSharpModule

        member x.IsTupleType = notImplemented()

        member x.IsValueType = entity.IsValueType

        /// Currently we only care about definitions for entities, not uses
        member x.OriginalDefinition = x :> ITypeSymbol

        member x.SpecialType = notImplemented() // int, string, void, enum, nullable etc

        member x.TypeKind =
            match entity with
            | _ when entity.IsArrayType -> TypeKind.Array
            | _ when entity.IsClass -> TypeKind.Class
            | _ when entity.IsDelegate -> TypeKind.Delegate
            | _ when entity.IsEnum -> TypeKind.Enum
            | _ when entity.IsInterface -> TypeKind.Interface
            | _ when entity.IsFSharpModule -> TypeKind.Module // TODO: Is this the same meaning?
            | _ when entity.IsValueType && not entity.IsEnum -> TypeKind.Struct
            | _ -> TypeKind.Unknown

        member x.FindImplementationForInterfaceMember(interfaceMember) =
            notImplemented()

/// Represents a type other than an array, a pointer, a type parameter, and dynamic.
and FSharpNamedTypeSymbol (entity: FSharpEntity) as this =
    inherit FSharpTypeSymbol(entity)

    let constructors() =
        (this :> INamespaceOrTypeSymbol).GetMembers().OfType<IMethodSymbol>()
        |> Seq.filter(fun m -> m.Name = ".ctor")

    override this.MetadataName = entity.LogicalName
    override this.ContainingNamespace =
        // Ideally we would want to be able to fetch the FSharpEntity representing the namespace here
        entity.Namespace
        |> Option.map (fun n -> FSharpLimitedNamespaceSymbol(n) :> INamespaceSymbol)
        |> Option.toObj

    interface INamedTypeSymbol with
        member x.Arity = entity.GenericParameters.Count //TODO: check - is this what Arity means here? Constructors are IMethodSymbols

        member x.AssociatedSymbol = notImplemented()

        member x.ConstructedFrom = notImplemented()

        member x.Constructors = constructors() |> Seq.toImmutableArray

        member x.DelegateInvokeMethod = notImplemented()

        member x.EnumUnderlyingType = notImplemented()

        member x.InstanceConstructors =
            constructors()
            |> Seq.filter(fun m -> not m.IsStatic)
            |> Seq.toImmutableArray

        member x.IsComImport = notImplemented()

        member x.IsGenericType = entity.GenericParameters.Count > 0

        member x.IsImplicitClass = notImplemented()

        member x.IsScriptClass = entity.DeclarationLocation.FileName.EndsWith(".fsx")

        member x.IsUnboundGenericType  = notImplemented()

        member x.MemberNames =
            (x :> INamespaceOrTypeSymbol).GetMembers()
            |> Seq.map(fun m -> m.Name)

        member x.MightContainExtensionMethods = notImplemented()

        member x.OriginalDefinition = x :> INamedTypeSymbol

        member x.StaticConstructors =
            constructors()
            |> Seq.filter(fun m -> m.IsStatic)
            |> Seq.toImmutableArray

        member x.TupleElements = notImplemented()

        member x.TupleUnderlyingType = notImplemented()

        member x.TypeArguments = notImplemented()

        member x.TypeParameters = notImplemented()

        member x.Construct (typeArguments) = notImplemented()

        member x.ConstructUnboundGenericType () = notImplemented()

        member x.GetTypeArgumentCustomModifiers (ordinal) = notImplemented()

and FSharpMethodSymbol (method:FSharpMemberOrFunctionOrValue) =
    inherit FSharpISymbol(method, true, method.DeclarationLocation)

    override x.Name = method.CompiledName

    interface IMethodSymbol with
        member x.Arity = notImplemented()

        member x.AssociatedAnonymousDelegate = notImplemented()

        member x.AssociatedSymbol = notImplemented()

        member x.ConstructedFrom = notImplemented()

        member x.ExplicitInterfaceImplementations = notImplemented()

        member x.HidesBaseMethodsByName = notImplemented()

        member x.IsAsync = notImplemented()

        member x.IsCheckedBuiltin = notImplemented()

        member x.IsExtensionMethod = method.IsExtensionMember

        member x.IsGenericMethod = method.GenericParameters.Count > 0

        member x.IsVararg = notImplemented()

        member x.MethodKind = notImplemented()

        member x.OriginalDefinition = x :> IMethodSymbol

        member x.OverriddenMethod = notImplemented()

        member x.Parameters = notImplemented()

        member x.PartialDefinitionPart = notImplemented()

        member x.PartialImplementationPart = notImplemented()

        member x.ReceiverType = notImplemented()

        member x.ReducedFrom = notImplemented()

        member x.RefCustomModifiers = notImplemented()

        member x.RefKind = notImplemented()

        member x.ReturnsByRef = notImplemented()

        member x.ReturnsByRefReadonly = notImplemented()

        member x.ReturnsVoid = notImplemented()

        member x.ReturnType =
            method.ReturnParameter.Type
            |> typeDefinitionSafe
            |> Option.map(fun e -> FSharpTypeSymbol(e) :> ITypeSymbol)
            |> Option.toObj

        member x.ReturnTypeCustomModifiers = notImplemented()

        member x.TypeArguments = notImplemented()

        member x.TypeParameters = notImplemented()

        member x.Construct (typeArguments)= notImplemented()

        member x.GetDllImportData () = notImplemented()

        member x.GetReturnTypeAttributes () = notImplemented()

        member x.GetTypeInferredDuringReduction (reducedFromTypeParameter) = notImplemented()

        member x.ReduceExtensionMethod (receiverType) = notImplemented()

and FSharpNamespaceOrTypeSymbol (entity:FSharpEntity) =
    inherit FSharpISymbol(entity, true, entity.DeclarationLocation)

    let memberToISymbol (m: FSharpMemberOrFunctionOrValue) : ISymbol =
        match m with
        | _ when m.IsProperty -> FSharpPropertySymbol(m) :> _
        | _ when m.IsMember -> FSharpMethodSymbol(m) :> _
        | _ -> FSharpISymbol(m, true, m.DeclarationLocation) :> _

    let getMembers() =
        entity.TryGetMembersFunctionsAndValues
        |> Seq.map memberToISymbol

    let getTypeMembers() =
        entity.NestedEntities
        |> Seq.map (fun e -> FSharpNamedTypeSymbol(e) :> INamedTypeSymbol)


    interface INamespaceOrTypeSymbol with
        member x.IsNamespace = entity.IsNamespace

        member x.IsType = not entity.IsNamespace && not entity.IsArrayType
            //TODO: && not TypeParameter - how?
        member x.GetMembers () =
            getMembers()
            |> Seq.toImmutableArray

        member x.GetMembers (name) =
            getMembers()
            |> Seq.filter(fun m -> m.Name = name)
            |> Seq.toImmutableArray

        /// Get all the members of this symbol that are types
        member x.GetTypeMembers () =
            getTypeMembers()
            |> Seq.toImmutableArray

        /// Get all the members of this symbol that are types with the given name
        member x.GetTypeMembers (name) =
            getTypeMembers()
            |> Seq.filter(fun m -> m.Name = name)
            |> Seq.toImmutableArray

        /// Get all the members of this symbol that are types with the given name and arity
        member x.GetTypeMembers (name, arity) =
            getTypeMembers()
            |> Seq.filter(fun m -> m.Name = name)
            |> Seq.filter(fun m -> m.Arity = arity)
            |> Seq.toImmutableArray

and FSharpPropertySymbol (property:FSharpMemberOrFunctionOrValue) =
    inherit FSharpISymbol(property, true, property.DeclarationLocation)

    interface IPropertySymbol with
        member x.ExplicitInterfaceImplementations = notImplemented()

        member x.GetMethod =
            if property.HasGetterMethod then
                FSharpMethodSymbol(property.GetterMethod) :> _
            else
                null

        member x.IsIndexer = notImplemented()

        member x.IsReadOnly = not property.HasSetterMethod

        member x.IsWithEvents = notImplemented()

        member x.IsWriteOnly = not property.HasGetterMethod

        member x.OriginalDefinition = x :> IPropertySymbol

        member x.OverriddenProperty = notImplemented()

        member x.Parameters = notImplemented()

        member x.RefCustomModifiers = notImplemented()

        member x.RefKind = notImplemented()

        member x.ReturnsByRef = notImplemented()

        member x.ReturnsByRefReadonly = notImplemented()

        member x.SetMethod =
            if property.HasSetterMethod then
                FSharpMethodSymbol(property.SetterMethod) :> _
            else
                null

        member x.Type =
            property.ReturnParameter.Type
            |> typeDefinitionSafe
            |> Option.map(fun e -> FSharpTypeSymbol(e) :> ITypeSymbol)
            |> Option.toObj

        member x.TypeCustomModifiers = notImplemented()

/// Limited namespace symbol - only useful for fetching the name
/// If we could go from FSharpEntity (type) -> FSharpEntity (containing namespace) we wouldn't need this
and FSharpLimitedNamespaceSymbol (namespaceName: string) =
    interface INamespaceSymbol with
        member x.ConstituentNamespaces = notImplemented()
        member x.ContainingCompilation = notImplemented()
        member x.IsGlobalNamespace = namespaceName = "global"
        member x.NamespaceKind = notImplemented()
        member x.GetMembers () : INamespaceOrTypeSymbol seq = notImplemented()
        member x.GetMembers () : ImmutableArray<ISymbol> = notImplemented()
        member x.GetMembers (name:string) : INamespaceOrTypeSymbol seq = notImplemented()
        member x.GetMembers (name:string) : ImmutableArray<ISymbol> = notImplemented()
        member x.GetTypeMembers () = notImplemented()
        member x.GetTypeMembers (name:string) = notImplemented()
        member x.GetTypeMembers (name:string, arity:int) = notImplemented()
        member x.GetNamespaceMembers () = notImplemented()
        member x.Kind = SymbolKind.Local
        member x.Language = "F#"
        member x.Name = namespaceName
        member x.MetadataName = namespaceName
        member x.ContainingSymbol = null
        member x.ContainingAssembly = null
        member x.ContainingModule = null
        member x.ContainingType = null
        member x.ContainingNamespace = null
        member x.IsDefinition = true
        member x.IsStatic = false
        member x.IsVirtual = false
        member x.IsOverride = false
        member x.IsAbstract = false
        member x.IsSealed = false
        member x.IsExtern = false
        member x.IsImplicitlyDeclared = false
        member x.CanBeReferencedByName = true
        member x.Locations = ImmutableArray.Empty
        member x.DeclaringSyntaxReferences = ImmutableArray.Empty //TODO
        member x.GetAttributes () = ImmutableArray.Empty //TODO
        member x.DeclaredAccessibility = notImplemented()
        member x.OriginalDefinition = notImplemented()
        member x.Accept (_visitor:SymbolVisitor) = () //TODO
        member x.Accept<'a> (_visitor: SymbolVisitor<'a>) = Unchecked.defaultof<'a>
        member x.GetDocumentationCommentId () = notImplemented()
        member x.GetDocumentationCommentXml (_culture, _expand, _token) = notImplemented()
        member x.ToDisplayString _format = namespaceName
        member x.ToDisplayParts _format = ImmutableArray.Empty //TODO
        member x.ToMinimalDisplayString (_semanticModel, _position, _format) = namespaceName
        member x.ToMinimalDisplayParts (_semanticModel, _position, _format) = ImmutableArray.Empty //TODO
        member x.HasUnsupportedMetadata = false //TODO
        member x.Equals (other:ISymbol) = x.Equals(other)
        member x.IsNamespace = true
        member x.IsType = false