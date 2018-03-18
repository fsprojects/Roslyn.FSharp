namespace Roslyn.FSharp

open Microsoft.CodeAnalysis
open Microsoft.FSharp.Compiler.SourceCodeServices

type FSharpPropertySymbol (property:FSharpMemberOrFunctionOrValue) =
    inherit FSharpISymbol(property, true, property.DeclarationLocation)

    interface IPropertySymbol with
        member x.ExplicitInterfaceImplementations = notImplemented()

        member x.GetMethod = notImplemented()

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

        member x.SetMethod = notImplemented()

        member x.Type = notImplemented()

        member x.TypeCustomModifiers = notImplemented()
