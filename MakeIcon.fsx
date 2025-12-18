#r "nuget:MetadataExtractor"
open System
open System.IO
open System.Runtime.InteropServices

[<Literal>]
let RES_ICON = 1us

[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type NEWHEADER = {
    Reserved: uint16
    ResType: uint16
    ResCount: uint16
}

[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type ICONDIRENTRY= {
    Width: uint8
    Height: uint8
    ColorCount: uint8
    Reserved: uint8
    Planes: uint16
    BitCount: uint16
    BytesInRes: uint32
    ImageOffset: uint32
}

type ImageSource = {
    Width: int
    Height: int
    Data: Memory<byte>
}

let makeICOFromPNGs outputFile inputFiles =
    let readPNG source =
        use strm = System.IO.File.OpenRead source
        let metadata = MetadataExtractor.ImageMetadataReader.ReadMetadata(strm)
        let pngInfo =
            metadata
            |> Seq.tryPick (fun dir ->
                match dir with
                | :? MetadataExtractor.Formats.Png.PngDirectory as pngDir ->
                    let width = pngDir.GetObject(MetadataExtractor.Formats.Png.PngDirectory.TagImageWidth) :?> int
                    let height = pngDir.GetObject(MetadataExtractor.Formats.Png.PngDirectory.TagImageHeight) :?> int
                    let bitDepth = pngDir.GetObject(MetadataExtractor.Formats.Png.PngDirectory.TagBitsPerSample) :?> byte
                    let colorType = pngDir.GetObject(MetadataExtractor.Formats.Png.PngDirectory.TagColorType) :?> int |> MetadataExtractor.Formats.Png.PngColorType.FromNumericValue
                    if colorType <> MetadataExtractor.Formats.Png.PngColorType.TrueColorWithAlpha then
                        failwithf "[%s] ColorType must be TrueColorWithAlpha, but got %s" source colorType.Description
                    if bitDepth <> 8uy then
                        failwithf "[%s] BitsPerSample must be 8, but got %d" source bitDepth
                    strm.Position <- 0
                    let buf = Memory<byte>(Array.zeroCreate (int strm.Length))
                    strm.ReadExactly buf.Span
                    Some { Width = width; Height = height; Data = buf }
                | _ ->
                    None
            )
        match pngInfo with
        | Some img ->
            img
        | None ->
            failwithf "[%s] No PNG info found" source

    let writeEntry (output: Stream) offset image =
        let entry = {
            Width = if image.Width >= 256 then 0uy else uint8 image.Width
            Height = if image.Height >= 256 then 0uy else uint8 image.Height
            ColorCount = 0uy
            Reserved = 0uy
            Planes = 1us
            BitCount = 32us
            BytesInRes = image.Data.Length |> uint32
            ImageOffset = offset
        }
        let bin = MemoryMarshal.AsBytes (Span(ref entry))
        output.Write bin
        offset + entry.BytesInRes

    let images =
        inputFiles
        |> List.map readPNG

    use f = File.OpenWrite outputFile

    let header = { Reserved=0us; ResType=RES_ICON; ResCount=List.length images |> uint16 }
    let headerBin = (MemoryMarshal.AsBytes (Span<NEWHEADER>(ref header)))
    f.Write headerBin

    let offset =
        Marshal.SizeOf<NEWHEADER>() + Marshal.SizeOf<ICONDIRENTRY>() * List.length images
        |> uint32
    List.fold (writeEntry f) offset images |> ignore

    images
    |> List.iter (fun image -> f.Write image.Data.Span)

let inputFiles = [
    @"AssetSources/ZenkeiViewerIcon_16.png"
    @"AssetSources/ZenkeiViewerIcon_32.png"
    @"AssetSources/ZenkeiViewerIcon_48.png"
    @"AssetSources/ZenkeiViewerIcon_64.png"
    @"AssetSources/ZenkeiViewerIcon_128.png"
    @"AssetSources/ZenkeiViewerIcon_256.png"
    @"AssetSources/ZenkeiViewerIcon_512.png"
]
let outputFile = "Assets/ZenkeiViewer.ico";
makeICOFromPNGs outputFile inputFiles

