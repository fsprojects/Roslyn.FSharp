namespace Roslyn.FSharp

open System.IO
open ExtCore.Control

type FSharpXmlDocumentationProvider(xmlPath) =
    inherit Microsoft.CodeAnalysis.XmlDocumentationProvider()
    member x.XmlPath = xmlPath
    member x.GetDocumentation documentationCommentId culture cancellationToken =
        base.GetDocumentationForSymbol(documentationCommentId, culture, cancellationToken)

    override x.GetSourceStream(_cancellationToken) =
        new FileStream(xmlPath, FileMode.Open, FileAccess.Read) :> Stream

    override x.Equals(other) =
        match other with
        | :? FSharpXmlDocumentationProvider as other' -> other'.XmlPath = xmlPath
        | _ -> false

    override x.GetHashCode() = xmlPath.GetHashCode()

module XmlDocumentation =
    open System.IO
    /// LRU based memoize
    let private memoize f n =
        let lru = ref (ExtCore.Caching.LruCache.create n)
        fun x -> match (!lru).TryFind x with
                 | Some entry, cache ->
                     lru := cache
                     entry
                 | None, cache ->
                     let res = f x
                     lru := cache.Add (x, res)
                     res

    let private xmlDocProvider =
        memoize (fun filename ->
            try Some (FSharpXmlDocumentationProvider(filename))
            with exn -> None) 20u

    let private tryExt file ext =
        Path.ChangeExtension(file,ext)

    let private tryFindDocFileFromAssemblyFileName file  =
        [ tryExt file "xml" ; tryExt file "XML" ]
        |> List.tryFind File.Exists 

    let getXmlDocFromAssembly assemblyFileName key culture token =
        let doc =
            maybe { let! assembly = assemblyFileName
                    let! xmlFile = tryFindDocFileFromAssemblyFileName assembly
                    let! docReader = xmlFile  |> xmlDocProvider
                    return docReader.GetDocumentation key culture token }
        doc |> Option.toObj