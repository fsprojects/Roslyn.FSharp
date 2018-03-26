namespace rec Roslyn.FSharp

open System.Collections.Immutable
open System.Collections.Generic

open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices

[<AutoOpen>]
module TypeHelpers =
    let namedTypeFromEntity (entity:FSharpEntity) =
        FSharpNamedTypeSymbol(entity) :> INamedTypeSymbol

type FSharpTypeSymbol (entity:FSharpEntity) =
    inherit FSharpNamespaceOrTypeSymbol(entity)

    override this.GetAttributes () =
        entity.Attributes
        |> Seq.map(fun a -> FSharpAttributeData(a) :> AttributeData)
        |> Seq.toImmutableArray

    interface ITypeSymbol with
        member x.AllInterfaces =
            entity.AllInterfaces
            |> Seq.choose typeDefinitionSafe
            |> Seq.map namedTypeFromEntity
            |> Seq.toImmutableArray

        member x.BaseType =
            entity.BaseType
            |> Option.bind typeDefinitionSafe
            |> Option.map namedTypeFromEntity
            |> Option.toObj

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
        |> Option.map (fun n -> FSharpNamespaceSymbol(n, Seq.empty, 0) :> INamespaceSymbol)
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

        member x.IsSerializable = false

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
        |> Seq.map namedTypeFromEntity


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

and FSharpNamespaceSymbol (namespaceName: string, entities: FSharpEntity seq, namespaceLevel: int) =
    inherit FSharpSymbolBase()
    let getNamedTypes() =
        entities
        |> Seq.map(fun e -> FSharpNamedTypeSymbol(e) :> INamedTypeSymbol)

    override x.Name = namespaceName
    override x.DeclaredAccessibility = Accessibility.Public

    interface INamespaceSymbol with
        member x.ConstituentNamespaces = notImplemented()
        member x.ContainingCompilation = notImplemented()
        member x.IsGlobalNamespace = namespaceName = "global"
        member x.NamespaceKind = notImplemented()
        member x.GetMembers () : ImmutableArray<ISymbol> =
            getNamedTypes()
            |> Seq.cast<ISymbol>
            |> Seq.toImmutableArray

        member x.GetMembers () : INamespaceOrTypeSymbol seq =
            getNamedTypes()
            |> Seq.cast<INamespaceOrTypeSymbol>

        member x.GetMembers (name:string) : INamespaceOrTypeSymbol seq =
            getNamedTypes()
            |> Seq.cast<INamespaceOrTypeSymbol>
            |> Seq.filter(fun t -> t.Name = name)

        member x.GetMembers (name:string) : ImmutableArray<ISymbol> =
            getNamedTypes()
            |> Seq.cast<ISymbol>
            |> Seq.filter(fun t -> t.Name = name)
            |> Seq.toImmutableArray

        member x.GetTypeMembers () =
            getNamedTypes()
            |> Seq.toImmutableArray

        member x.GetTypeMembers (name:string) =
            getNamedTypes()
            |> Seq.filter(fun t -> t.Name = name)
            |> Seq.toImmutableArray

        member x.GetTypeMembers (name:string, arity:int) =
            getNamedTypes()
            |> Seq.filter(fun t -> t.Name = name)
            |> Seq.filter(fun t -> t.Arity = arity)
            |> Seq.toImmutableArray

        member x.GetNamespaceMembers () =
            entities
            |> Seq.groupBy(fun entity ->
                match entity.Namespace with
                | Some ns -> 
                    let namespaceParts = ns.Split('.')
                    if namespaceParts.Length > namespaceLevel then
                        namespaceParts.[namespaceLevel] |> Some
                    else
                        None
                | None -> None)

            |> Seq.filter(fun (ns, entities) -> ns.IsSome)
            |> Seq.sortBy(fun (ns, entities) -> ns) // Roslyn sorts these
            |> Seq.map (fun (ns, entities) -> FSharpNamespaceSymbol(ns.Value, entities, namespaceLevel+1) :> INamespaceSymbol)
        member x.IsNamespace = true
        member x.IsType = false

and FSharpAssemblySymbol (assembly: FSharpAssembly) =
    inherit FSharpSymbolBase()
    override x.Name = assembly.SimpleName 

    override this.GetAttributes () =
        assembly.Contents.Attributes
        |> Seq.map(fun attr -> FSharpAttributeData(attr) :> AttributeData)
        |> Seq.toImmutableArray

    interface IAssemblySymbol with
        member x.GlobalNamespace = FSharpNamespaceSymbol("global", assembly.Contents.Entities, 0) :> INamespaceSymbol
        member x.Identity =
            //match assembly.FileName with
            //| Some filename ->
            //    let asm = System.Reflection.Assembly.ReflectionOnlyLoadFrom(filename)
            //    AssemblyIdentity.FromAssemblyDefinition asm
            ////TODO: Probably better to instantiate this directly,
            //// but I don't know where to get the information needed to construct
            ////AssemblyIdentity(name,version,cultureName,publicKey,hasPublicKey, isRetargetable,contentType)
            //| None ->
            AssemblyIdentity(assembly.SimpleName)
        member x.IsInteractive = notImplemented()
        member x.MightContainExtensionMethods = true //TODO: no idea
        member x.Modules = notImplemented()
        member x.NamespaceNames =
            assembly.Contents.Entities
            |> Seq.choose(fun entity -> entity.Namespace)
            |> Seq.distinct
            |> Seq.collect(fun ns -> ns.Split('.'))
            |> Seq.distinct
            |> Seq.sort // Roslyn sorts these
            |> Seq.toCollection

        member x.TypeNames =
            assembly.Contents.Entities
            |> Seq.map(fun entity -> entity.CompiledName)
            |> Seq.toCollection

        member x.GetMetadata ()= notImplemented()
        member x.GetTypeByMetadataName (fullyQualifiedMetadataName) =
            let path =
                fullyQualifiedMetadataName.Split '.'
                |> List.ofArray

            assembly.Contents.FindEntityByPath path
            |> Option.map(fun e -> FSharpNamedTypeSymbol(e) :> INamedTypeSymbol)
            |> Option.toObj

        member x.GivesAccessTo (toAssembly)= notImplemented()
        member x.ResolveForwardedType (fullyQualifiedMetadataName)= notImplemented()
        member x.Kind = SymbolKind.Assembly

/// Hopefully temporary type because Microsoft.CodeAnalysis.TypedConstant
/// has internal constructors - see https://github.com/dotnet/roslyn/issues/25669
and TypedConstant(entity: ITypeSymbol, kind:TypedConstantKind, value:obj) =
    member x.Type = entity
    member x.Kind = kind
    member x.Value = value
    member x.Values = notImplemented()
        //if x.Kind = TypedConstantKind.Array then
        //    let sequence = x.Value :?> seq<TypedConstant>
        //    sequence |> Seq.toImmutableArray
        //else
            //ImmutableArray.Empty


and FSharpAttributeData(attribute: FSharpAttribute) =
    inherit AttributeData()

    let getTypeKind(entity:FSharpEntity) =
        let fullName =
            entity.AbbreviatedTypeSafe
            |> Option.bind (fun t -> t.TypeDefinitionSafe)
            |> Option.map (fun typeDefinition -> typeDefinition.FullName)

        match fullName with
        | Some "System.Boolean"
        | Some "System.SByte"
        | Some "System.Int16"
        | Some "System.Int32"
        | Some "System.Int64"
        | Some "System.Byte"
        | Some "System.UInt16"
        | Some "System.UInt32"
        | Some "System.UInt64"
        | Some "System.Single"
        | Some "System.Double"
        | Some "System.Char"
        | Some "System.String"
        | Some "System.Object" ->
            TypedConstantKind.Primitive
        | _ ->
            match entity with
            | _ when entity.IsArrayType -> TypedConstantKind.Array
            | _ when entity.IsEnum -> TypedConstantKind.Enum
            | _ when entity.IsFSharpModule || entity.IsClass -> TypedConstantKind.Type //TODO: no idea
            | _ -> TypedConstantKind.Error

    override x.CommonAttributeClass =
        FSharpNamedTypeSymbol(attribute.AttributeType) :> INamedTypeSymbol
    override x.CommonConstructorArguments = notImplemented()
    override x.CommonAttributeConstructor = notImplemented()
        //attribute.
    override x.CommonApplicationSyntaxReference = notImplemented()
    override x.CommonNamedArguments =
        notImplemented()

    /// substitute method for CommonConstructorArguments that uses our TypedConstant type
    member x.ConstructorArguments =
        attribute.ConstructorArguments
        |> Seq.choose (fun (ty, obj) ->
            ty.TypeDefinitionSafe
            |> Option.map(fun entity ->
                let typeSymbol = FSharpNamedTypeSymbol(entity) :> ITypeSymbol
                let typeKind = getTypeKind entity
                TypedConstant(typeSymbol, typeKind, obj)))
        |> Seq.toImmutableArray

    /// substitute method for CommonNamedArguments that uses our TypedConstant type
    member x.NamedArguments =
        attribute.NamedArguments
        |> Seq.choose (fun (ty, nm, isField, obj) ->
            ty.TypeDefinitionSafe
            |> Option.map(fun entity ->
                let typeSymbol = FSharpNamedTypeSymbol(entity) :> ITypeSymbol
                let typeKind = getTypeKind entity
                let constant = TypedConstant(typeSymbol, typeKind, obj)
                KeyValuePair(nm, constant)))
        |> Seq.toImmutableArray
