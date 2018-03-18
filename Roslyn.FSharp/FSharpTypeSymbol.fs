namespace Roslyn.FSharp

open System.Collections.Immutable

open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices

type FSharpTypeSymbol (entity:FSharpEntity) =
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
        |> Seq.filter(fun m -> m.Name = "( .ctor )")

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

        member x.MightContainExtensionMethods  = notImplemented()

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
        | _ when m.IsMember && not m.IsPropertyGetterMethod && not m.IsPropertySetterMethod -> FSharpMethodSymbol(m) :> _
        | _ -> FSharpISymbol(m, true, m.DeclarationLocation) :> _

    let getMembers() =
        entity.TryGetMembersFunctionsAndValues
        |> Seq.map memberToISymbol

    interface INamespaceOrTypeSymbol with
        member x.IsNamespace = entity.IsNamespace

        member x.IsType = not entity.IsNamespace && not entity.IsArrayType
            //TODO: && not TypeParameter - how?
        member x.GetMembers () = getMembers() |> Seq.toImmutableArray

        member x.GetMembers (name) =
            getMembers()
            |> Seq.filter(fun m -> m.Name = name)
            |> Seq.toImmutableArray

        member x.GetTypeMembers () = notImplemented()

        member x.GetTypeMembers (name) = notImplemented()

        member x.GetTypeMembers (name, arity) = notImplemented()