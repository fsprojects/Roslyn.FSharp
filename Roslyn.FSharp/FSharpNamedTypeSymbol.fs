namespace Roslyn.FSharp


open System
open System.Collections.Generic
open System.Collections.Immutable
open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices

/// Represents a type other than an array, a pointer, a type parameter, and dynamic.
type FSharpNamedTypeSymbol (symbolUse:FSharpEntity) =
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
