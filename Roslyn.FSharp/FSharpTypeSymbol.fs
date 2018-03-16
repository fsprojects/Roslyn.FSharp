namespace Roslyn.FSharp

open System
open System.Collections.Immutable

open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices

type FSharpTypeSymbol (symbolUse:FSharpSymbolUse) =
    inherit FSharpNamespaceOrTypeSymbol(symbolUse)
    interface ITypeSymbol with
        //
        // Properties
        //
        member x.AllInterfaces = notImplemented()

        member x.BaseType =notImplemented()

        member x.Interfaces =notImplemented()

        member x.IsAnonymousType =notImplemented()

        member x.IsReferenceType =notImplemented()

        member x.IsTupleType =notImplemented()

        member x.IsValueType =notImplemented()

        member x.OriginalDefinition : ITypeSymbol = notImplemented()

        member x.SpecialType =notImplemented()

        member x.TypeKind =notImplemented()

        //
        // Methods
        //
        member x.FindImplementationForInterfaceMember(interfaceMember) =
            notImplemented()