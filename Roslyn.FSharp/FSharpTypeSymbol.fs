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
                match baseType.TypeDefinitionSafe() with
                | Some typeDefinition -> namedTypeFromEntity typeDefinition
                | None -> null
            | None -> null

        member x.Interfaces =
            entity.DeclaredInterfaces
            |> Seq.choose typeDefinitionSafe
            |> Seq.map namedTypeFromEntity
            |> Seq.toImmutableArray

        member x.IsAnonymousType = false

        member x.IsReferenceType = not entity.IsValueType

        member x.IsTupleType = notImplemented()

        member x.IsValueType = entity.IsValueType

        /// Currently we only care about definitions for entities, not uses
        member x.OriginalDefinition = x :> ITypeSymbol

        member x.SpecialType = notImplemented()

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
and FSharpNamedTypeSymbol (symbolUse:FSharpEntity) =
    inherit FSharpTypeSymbol(symbolUse)

    interface INamedTypeSymbol with
        member x.Arity  = notImplemented()

        member x.AssociatedSymbol  = notImplemented()

        member x.ConstructedFrom  = notImplemented()

        member x.Constructors  = [].ToImmutableArray()

        member x.DelegateInvokeMethod  = notImplemented()

        member x.EnumUnderlyingType  = notImplemented()

        member x.InstanceConstructors  = notImplemented()

        member x.IsComImport  = notImplemented()

        member x.IsGenericType  = notImplemented()

        member x.IsImplicitClass  = notImplemented()

        member x.IsScriptClass  = notImplemented()

        member x.IsUnboundGenericType  = notImplemented()

        member x.MemberNames  = notImplemented()

        member x.MightContainExtensionMethods  = notImplemented()

        member x.OriginalDefinition : INamedTypeSymbol = notImplemented()

        member x.StaticConstructors  = notImplemented()

        member x.TupleElements  = notImplemented()

        member x.TupleUnderlyingType  = notImplemented()

        member x.TypeArguments  = notImplemented()

        member x.TypeParameters  = notImplemented()

        member x.Construct (typeArguments) = notImplemented()

        member x.ConstructUnboundGenericType () = notImplemented()

        member x.GetTypeArgumentCustomModifiers (ordinal) = notImplemented()