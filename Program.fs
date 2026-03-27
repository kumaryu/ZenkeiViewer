module ZenkeiViewer

open System.Numerics
open FSharp.Control
open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Input
open Avalonia.Platform.Storage
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open System
open Avalonia.Threading
open LiteDB
open SkiaSharp
open Common
open ImageViewControl
open DragMoveGestureRecognizer
open ExclusivePinchGestureRecognizer
open Avalonia.FuncUI.DSL
open Avalonia.Layout

[<Literal>]
let settingEntriesLimit = 10000

type Image = {
    bitmap: SKBitmap
    file: IStorageFile
    folder: IStorageFolder option
    equirectangular: bool
}

type PerImageState = {
    fov: float<deg>
    distance: float
    yaw: float<deg>
    pitch: float<deg>
    roll: float<deg>
    pan: Vector
    useEquirectangular: bool
    source: Image option
}

module PerImageState =
    let defaultState = {
        fov = 60.0<deg>
        distance = 0.5
        yaw = 0.0<deg>
        pitch = 0.0<deg>
        roll = 0.0<deg>
        pan = Vector.Zero
        useEquirectangular = true
        source = None
    }

type PinchState = {
    originalScale: float option
    originalAngle: float<deg> option
}

module PinchState =
    let none = { originalScale = None; originalAngle = None }
    let scale originalScale = { none with originalScale = Some originalScale }
    let rotate originalAngle = { none with originalAngle = Some originalAngle }

type State = {
    db: LiteDatabase
    fullScreen: bool
    image: PerImageState
    pinchState: PinchState
}

module PerImageSettings =
    let pathToId (path: string) =
        System.IO.Hashing.XxHash128.Hash(System.Text.Encoding.UTF8.GetBytes(path))

type PerImageSettings () =
    let mutable sourcePath = ""
    [<BsonId>]
    member val Id: byte array = [||] with get, set
    member val Updated : System.DateTime = System.DateTime.Now with get, set
    member val Fov: float<deg> = 60.0<deg> with get, set
    member val Distance: float = 0.5 with get, set
    member val Yaw: float<deg> = 0.0<deg> with get, set
    member val Pitch: float<deg> = 0.0<deg> with get, set
    member val Roll: float<deg> = 0.0<deg> with get, set
    member val PanX: float = 0.0 with get, set
    member val PanY: float = 0.0 with get, set
    member val UseEquirectangular: bool = true with get, set
    member this.SourcePath
        with get () = sourcePath
        and set value =
            sourcePath <- value
            this.Id <- PerImageSettings.pathToId value

    static member serialize (settings: PerImageSettings) : BsonValue =
        let doc = BsonDocument()
        doc["_id"] <- settings.Id
        doc["Updated"] <- settings.Updated
        doc["Fov"] <- float settings.Fov
        doc["Distance"] <- settings.Distance
        doc["Yaw"] <- float settings.Yaw
        doc["Pitch"] <- float settings.Pitch
        doc["Roll"] <- float settings.Roll
        doc["PanX"] <- settings.PanX
        doc["PanY"] <- settings.PanY
        doc["UseEquirectangular"] <- settings.UseEquirectangular
        doc["SourcePath"] <- settings.SourcePath
        doc

    static member deserialize (doc: BsonValue) =
        let settings = PerImageSettings()
        settings.Id <- doc["_id"].AsBinary
        settings.Updated <- doc["Updated"].AsDateTime
        settings.Fov <- doc["Fov"].AsDouble * 1.0<deg>
        settings.Distance <- doc["Distance"].AsDouble
        settings.Yaw <- doc["Yaw"].AsDouble * 1.0<deg>
        settings.Pitch <- doc["Pitch"].AsDouble * 1.0<deg>
        settings.Roll <- doc["Roll"].AsDouble * 1.0<deg>
        settings.PanX <- doc["PanX"].AsDouble
        settings.PanY <- doc["PanY"].AsDouble
        settings.UseEquirectangular <- doc["UseEquirectangular"].AsBoolean
        settings.SourcePath <- doc["SourcePath"].AsString
        settings

let liteDBMapper = BsonMapper()
liteDBMapper.RegisterType<PerImageSettings>(
    (fun t -> PerImageSettings.serialize t),
    (fun d -> PerImageSettings.deserialize d)
)

type Msg =
| NextImage
| PreviousImage
| SelectImage
| OpenImage of PerImageState option
| OpenFile of IStorageFile
| OpenFolder of IStorageFolder
| ResetView
| Zoom of float
| Pinch of scale: float * angle: float<deg>
| PinchEnd
| ZoomFov of float
| Move of delta: Vector * size: Vector
| Roll of delta: Vector * size: Vector
| RollAngle of delta: float<deg>
| SetViewEquirectangular of bool
| ToggleFullScreen
| ExitFullScreen
| Exiting
| Exit

let openImageFileWithFolderAsync (file: IStorageFile) (folder: IStorageFolder option) =
    task {
        use! strm = file.OpenReadAsync()
        let metadata = MetadataExtractor.ImageMetadataReader.ReadMetadata(strm)
        let useEquirectangular =
            metadata
            |> Seq.tryPick (fun dir ->
                match dir with
                | :? MetadataExtractor.Formats.Xmp.XmpDirectory as xmpDir ->
                    let props = xmpDir.GetXmpProperties()
                    match (props.TryGetValue "GPano:ProjectionType", props.TryGetValue "GPano:UsePanoramaViewer") with
                    | ((true, v1), (true, v2)) ->
                        (String.Equals(v1, "equirectangular", StringComparison.InvariantCultureIgnoreCase) && XmpCore.XmpUtils.ConvertToBoolean(v2)) |> Some
                    | ((true, v1), (false, _)) ->
                        String.Equals(v1, "equirectangular", StringComparison.InvariantCultureIgnoreCase) |> Some
                    | ((false, _), _) ->
                        Some false
                | _ ->
                    None
            )
            |> Option.defaultValue false
        strm.Position <- 0
        let bitmap = SKBitmap.Decode(strm)
        let imageSource = { bitmap=bitmap; file=file; folder=folder; equirectangular=useEquirectangular } |> Some
        return { PerImageState.defaultState with useEquirectangular=useEquirectangular; source=imageSource } |> Some
    }

let openImageFileAsync (file: IStorageFile) =
    task {
        let! folder = file.GetParentAsync()
        let folder = Option.ofObj folder
        return! openImageFileWithFolderAsync file folder
    }

let isImageFile (item: IStorageItem) =
    match item with
    | :? IStorageFile as f ->
        let ext =
            System.IO.Path.GetExtension(f.Name)
            |> Option.ofObj
            |> Option.map _.ToLowerInvariant()
            |> Option.defaultValue ""
        match ext with
        | ".jpg" | ".jpeg" | ".jpe" | ".jif" | ".jfif"
        | ".png"
        | ".webp"
        | ".bmp"
        | ".avif" ->
            Some f
        | _ ->
            None
    | _ -> None

let getImagesFromFolderAsync (folder: IStorageFolder) =
    folder.GetItemsAsync()
    |> TaskSeq.choose isImageFile

let openImageFileFromFolderAsync (folder: IStorageFolder) =
    task {
        match! getImagesFromFolderAsync folder |> TaskSeq.tryHead with
        | Some file ->
            return! openImageFileWithFolderAsync file (Some folder)
        | None ->
            return None
    }

let selectImageAsync (host: HostWindow) =
    task {
        let filters = [
            Platform.Storage.FilePickerFileType("Image Files",
                Patterns=[
                    "*.jpg"; "*.JPG"; "*.jpe"; "*.JPE"; "*.jpeg"; "*.JPEG"; "*.jif"; "*.JIF"; "*.jfif"; "*.JFIF"
                    "*.png"; "*.PNG"
                    "*.webp"; "*.WEBP"
                    "*.bmp"; "*.BMP"
                    "*.avif"; "*.AVIF"
                ],
                MimeTypes=[
                    "image/jpeg"
                    "image/png"
                    "image/webp"
                    "image/bmp"
                    "image/avif"
                ]
            )
        ]
        let! files =
            Platform.Storage.FilePickerOpenOptions(AllowMultiple=false, FileTypeFilter=filters)
            |> host.StorageProvider.OpenFilePickerAsync
        match Seq.tryHead files |> Option.bind isImageFile with
        | Some file ->
            return! openImageFileAsync file
        | None ->
            return None
    }

let openImageByPath (host: HostWindow) (path: string) =
    task {
        let! item = host.StorageProvider.TryGetFileFromPathAsync path
        match item |> Option.ofObj |> Option.bind isImageFile with
        | None ->
            return None
        | Some file ->
            return! openImageFileAsync file
    }

let init (host: HostWindow) args =
    let settingsDir = System.IO.Path.Join(
        Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData, Environment.SpecialFolderOption.Create), 
        "ZenkeiViewer")
    System.IO.Directory.CreateDirectory(settingsDir) |> ignore

    let db = new LiteDatabase(ConnectionString(Filename=System.IO.Path.Join(settingsDir, "ZenkeiViewerSettings.db"), Upgrade=true, AutoRebuild=true, Connection=ConnectionType.Shared), liteDBMapper)
    let collection = db.GetCollection<PerImageSettings>()
    collection.EnsureIndex("Updated") |> ignore

    match Array.tryHead args with
    | Some path ->
        let cmd = Cmd.OfTask.perform (openImageByPath host) path OpenImage
        { db=db; fullScreen=false; image=PerImageState.defaultState; pinchState=PinchState.none }, cmd
    | None ->
        { db=db; fullScreen=false; image=PerImageState.defaultState; pinchState=PinchState.none }, Cmd.none

let update (host: HostWindow) (msg: Msg) (state: State) =
    match msg with
    | NextImage ->
        match state.image.source with
        | Some { file=file; folder=Some folder } ->
            let getNextImage () =
                task {
                    let! files = getImagesFromFolderAsync folder |> TaskSeq.toArrayAsync
                    let next =
                        files
                        |> Array.tryFindIndex (fun item -> item.Path = file.Path)
                        |> Option.map (fun idx -> files[(idx + 1) % Array.length files])
                    return!
                        next
                        |> Option.defaultValue file
                        |> openImageFileAsync
                }
            let cmd = Cmd.OfTask.perform getNextImage () OpenImage
            state, cmd
        | _ ->
            state, Cmd.none
    | PreviousImage ->
        match state.image.source with
        | Some { file=file; folder=Some folder } ->
            let getPreviousImage () =
                task {
                    let! files = getImagesFromFolderAsync folder |> TaskSeq.toArrayAsync
                    let prev =
                        files
                        |> Array.tryFindIndex (fun item -> item.Path = file.Path)
                        |> Option.map (fun idx -> files[(idx + Array.length files - 1) % Array.length files])
                    return!
                        prev
                        |> Option.defaultValue file
                        |> openImageFileAsync
                }
            let cmd = Cmd.OfTask.perform getPreviousImage () OpenImage
            state, cmd
        | _ ->
            state, Cmd.none
    | SelectImage ->
        let cmd = Cmd.OfTask.perform selectImageAsync host OpenImage
        state, cmd
    | OpenImage value ->
        match value with
        | None -> state, Cmd.none
        | Some image ->
            let collection = state.db.GetCollection<PerImageSettings>()

            state.image.source
            |> Option.iter (fun imageSource ->
                imageSource.bitmap.Dispose()
                let serializable = PerImageSettings(
                    SourcePath = imageSource.file.Path.ToString(),
                    Fov = state.image.fov,
                    Distance = state.image.distance,
                    Yaw = state.image.yaw,
                    Pitch = state.image.pitch,
                    Roll = state.image.roll,
                    PanX = state.image.pan.X,
                    PanY = state.image.pan.Y,
                    UseEquirectangular = state.image.useEquirectangular
                )
                collection.Upsert(serializable) |> ignore
            )

            let entry =
                try
                    image.source
                    |> Option.bind (fun source ->
                        source.file.Path.ToString()
                        |> PerImageSettings.pathToId
                        |> collection.FindById
                        |> Option.ofObj
                    )
                with
                | _ ->
                    None
            match entry with
            | Some e ->
                let newImageState = {
                    fov = e.Fov
                    distance = e.Distance
                    yaw = e.Yaw
                    pitch = e.Pitch
                    roll = e.Roll
                    pan = Vector(e.PanX, e.PanY)
                    useEquirectangular = e.UseEquirectangular
                    source = image.source
                }
                { state with image=newImageState; pinchState=PinchState.none }, Cmd.none
            | None ->
                { state with image=image; pinchState=PinchState.none }, Cmd.none
    | OpenFile file ->
        let cmd = Cmd.OfTask.perform openImageFileAsync file OpenImage
        state, cmd
    | OpenFolder folder ->
        let cmd = Cmd.OfTask.perform openImageFileFromFolderAsync folder OpenImage
        state, cmd
    | ResetView ->
        let newImageState =
            match state.image.source with
            | Some img ->
                { PerImageState.defaultState with useEquirectangular=img.equirectangular; source=Some img }
            | None ->
                PerImageState.defaultState
        { state with image=newImageState; pinchState=PinchState.none }, Cmd.none
    | Pinch (scale, angle) ->
        match state.pinchState with
        | { originalScale=None; originalAngle=None } ->
            if abs (scale - 1.0) > 0.1 then
                let originalScale = Option.defaultValue state.image.distance state.pinchState.originalScale
                let distanceScale = 1.0 / tan(state.image.fov / 2.0 |> toRad |> float)
                let newDistance = originalScale + (1.0 - scale) * 0.5 * distanceScale |> max -0.9 |> min (1.5 * distanceScale)
                { state with image={ state.image with distance=newDistance }; pinchState=PinchState.scale originalScale }, Cmd.none
            elif abs angle > 5.0<deg> then
                let originalAngle = Option.defaultValue state.image.roll state.pinchState.originalAngle
                let newRoll = angle + originalAngle |> clamp -90.0<deg> 90.0<deg>
                { state with image={ state.image with roll=newRoll }; pinchState=PinchState.rotate originalAngle }, Cmd.none
            else
                state, Cmd.none
        | { originalScale=Some originalScale; originalAngle=_ } ->
            let distanceScale = 1.0 / tan(state.image.fov / 2.0 |> toRad |> float)
            let newDistance = originalScale + (1.0 - scale) * 0.5 * distanceScale |> max -0.9 |> min (1.5 * distanceScale)
            { state with image={ state.image with distance=newDistance }; pinchState=PinchState.scale originalScale }, Cmd.none
        | { originalScale=_; originalAngle=Some originalAngle } ->
            let newRoll = angle + originalAngle |> clamp -90.0<deg> 90.0<deg>
            { state with image={ state.image with roll=newRoll }; pinchState=PinchState.rotate originalAngle }, Cmd.none
    | PinchEnd ->
        { state with pinchState=PinchState.none }, Cmd.none
    | RollAngle angleDelta ->
        let roll = 
            angleDelta + state.image.roll
            |> clamp -90.0<deg> 90.0<deg>
        { state with image={ state.image with roll=roll } }, Cmd.none
    | Zoom delta ->
        let distanceScale = 1.0 / tan(state.image.fov / 2.0 |> toRad |> float)
        let newDistance = state.image.distance + delta * 0.05 * distanceScale |> max -0.9 |> min (1.5 * distanceScale)
        { state with image={ state.image with distance=newDistance } }, Cmd.none
    | ZoomFov delta ->
        let newFov = state.image.fov + delta * 2.5<deg> |> max 5.0<deg> |> min 90.0<deg>
        let nearPlaneHeight = 2.0 * (state.image.distance + 1.0) * Math.Tan(state.image.fov / 2.0 |> toRad |> float)
        let newDistance = (nearPlaneHeight / 2.0) / Math.Tan(newFov / 2.0 |> toRad |> float) - 1.0
        { state with image={ state.image with distance=newDistance; fov=newFov } }, Cmd.none
    | Roll (screenDelta , screenSize) ->
        if state.image.useEquirectangular then
            let fov = state.image.fov |> toRad |> single
            let aspect = screenSize.X / screenSize.Y |> single
            let distance = state.image.distance |> single

            let forward = Vector3.UnitY
            let upward = Vector3.UnitZ
            let cameraPos = forward * -distance
            let worldViewMatrix =
                Matrix4x4.CreateLookTo(cameraPos, forward, upward)
            let viewProjectionMatrix =
                Matrix4x4.CreatePerspectiveFieldOfView(fov, aspect, 1.0f+distance, 10.0f+distance)
            let projectionViewportMatrix =
                Matrix4x4.CreateViewport(0.0f, 0.0f, screenSize.X |> single, screenSize.Y |> single, 0.0f, 1.0f)
            let viewportWorldMatrix = worldViewMatrix * viewProjectionMatrix * projectionViewportMatrix |> Matrix4x4.invert
            let fromDir = forward - cameraPos
            let toDir = Vector3.Transform(
                Vector3((screenDelta.X + (screenSize.X / 2.0)) |> single, (screenDelta.Y + (screenSize.Y / 2.0)) |> single, 0.0f), 
                viewportWorldMatrix)
            let toDir2 = Vector3(0.0f, toDir.Y, toDir.Z) |> Vector3.Normalize
            let roll = 
                (angleFromTo fromDir toDir2 Vector3.UnitX |> toDeg) + state.image.roll
                |> clamp -90.0<deg> 90.0<deg>
            { state with image={ state.image with roll=roll } }, Cmd.none
        else
            match state.image.source with
            | None ->
                state, Cmd.none
            | Some { bitmap=bmp } ->
                let forward = -Vector3.UnitZ
                let upward = Vector3.UnitY
                let aspect = screenSize.X / screenSize.Y
                let imageAspect = (bmp.Info.Width |> float) / (bmp.Info.Height |> float)
                let cameraPos = forward * single -state.image.distance
                let scale = 1.0 / (1.0 + state.image.distance)
                let scaleToFit =
                    if imageAspect > aspect then
                        Matrix4x4.CreateScale(1.0f, aspect / imageAspect |> single, 1.0f)
                    else
                        Matrix4x4.CreateScale(imageAspect / aspect |> single, 1.0f, 1.0f)
                let worldViewMatrix =
                    Matrix4x4.CreateScale(scale |> single, scale |> single, 1.0f) *
                    scaleToFit *
                    Matrix4x4.CreateLookTo(cameraPos, forward, upward)
                let viewProjectionMatrix = Matrix4x4.CreateOrthographicOffCenter(-1.0f, 1.0f, -1.0f, 1.0f, 1.0f, 10.0f)
                let projectionWorldMatrix = worldViewMatrix * viewProjectionMatrix |> Matrix4x4.invert
                let delta = Vector3.Transform(Vector3(single (screenDelta.X * 2.0 / screenSize.X), single (screenDelta.Y * 2.0 / screenSize.Y), 0.0f), projectionWorldMatrix)
                let pan = Vector(
                    state.image.pan.X + float delta.X |> clamp -1.0 1.0,
                    state.image.pan.Y - float delta.Y |> clamp -1.0 1.0
                )
                { state with image={ state.image with pan=pan } }, Cmd.none
    | Move (screenDelta , screenSize) ->
        if state.image.useEquirectangular then
            let fov = state.image.fov |> toRad |> single
            let aspect = screenSize.X / screenSize.Y |> single
            let distance = state.image.distance |> single

            let forward = Vector3.UnitY
            let upward = Vector3.UnitZ
            let cameraPos = forward * -distance
            let worldViewMatrix =
                Matrix4x4.CreateLookTo(cameraPos, forward, upward)
            let viewProjectionMatrix =
                Matrix4x4.CreatePerspectiveFieldOfView(fov, aspect, 1.0f+distance, 10.0f+distance)
            let projectionViewportMatrix =
                Matrix4x4.CreateViewport(0.0f, 0.0f, screenSize.X |> single, screenSize.Y |> single, 0.0f, 1.0f)
            let viewportWorldMatrix = worldViewMatrix * viewProjectionMatrix * projectionViewportMatrix |> Matrix4x4.invert
            let fromDir = forward - cameraPos
            let toDir = Vector3.Transform(
                Vector3((screenDelta.X + (screenSize.X / 2.0)) |> single, (screenDelta.Y + (screenSize.Y / 2.0)) |> single, 0.0f), 
                viewportWorldMatrix)
            let toDir1 = Vector3(toDir.X, toDir.Y, 0.0f) |> Vector3.Normalize
            let toDir2 = Vector3(0.0f, toDir.Y, toDir.Z) |> Vector3.Normalize
            let yawDelta = angleFromTo fromDir toDir1 Vector3.UnitZ |> toDeg
            let pitchDelta = angleFromTo fromDir toDir2 Vector3.UnitX |> toDeg
            let roll =
                signf state.image.roll *
                (abs state.image.roll - 0.7 * (abs pitchDelta + abs yawDelta) |> max 0.0<deg>)
            let yaw = state.image.yaw + yawDelta
            let yaw =
                if yaw > 180.0<deg> then
                    yaw - 360.0<deg>
                else if yaw < -180.0<deg> then
                    yaw + 360.0<deg>
                else
                    yaw
            let pitch = clamp -90.0<deg> 90.0<deg> (state.image.pitch + pitchDelta)
            let roll = clamp -90.0<deg> 90.0<deg> roll
            { state with image={ state.image with yaw=yaw; pitch=pitch; roll=roll } }, Cmd.none
        else
            match state.image.source with
            | None ->
                state, Cmd.none
            | Some { bitmap=bmp } ->
                let forward = -Vector3.UnitZ
                let upward = Vector3.UnitY
                let aspect = screenSize.X / screenSize.Y
                let imageAspect = (bmp.Info.Width |> float) / (bmp.Info.Height |> float)
                let cameraPos = forward * single -state.image.distance
                let scale = 1.0 / (1.0 + state.image.distance)
                let scaleToFit =
                    if imageAspect > aspect then
                        Matrix4x4.CreateScale(1.0f, aspect / imageAspect |> single, 1.0f)
                    else
                        Matrix4x4.CreateScale(imageAspect / aspect |> single, 1.0f, 1.0f)
                let worldViewMatrix =
                    Matrix4x4.CreateScale(scale |> single, scale |> single, 1.0f) *
                    scaleToFit *
                    Matrix4x4.CreateLookTo(cameraPos, forward, upward)
                let viewProjectionMatrix = Matrix4x4.CreateOrthographicOffCenter(-1.0f, 1.0f, -1.0f, 1.0f, 1.0f, 10.0f)
                let projectionWorldMatrix = worldViewMatrix * viewProjectionMatrix |> Matrix4x4.invert
                let delta = Vector3.Transform(Vector3(single (screenDelta.X * 2.0 / screenSize.X), single (screenDelta.Y * 2.0 / screenSize.Y), 0.0f), projectionWorldMatrix)
                let pan = Vector(
                    state.image.pan.X + float delta.X |> clamp -1.0 1.0,
                    state.image.pan.Y - float delta.Y |> clamp -1.0 1.0
                )
                { state with image={ state.image with pan=pan } }, Cmd.none
    | SetViewEquirectangular value ->
        { state with image={ state.image with useEquirectangular=value } }, Cmd.none
    | ToggleFullScreen ->
        if state.fullScreen then
            host.WindowState <- WindowState.Normal
        else
            host.WindowState <- WindowState.FullScreen
        { state with fullScreen = not state.fullScreen }, Cmd.none
    | ExitFullScreen ->
        if state.fullScreen then
            host.WindowState <- WindowState.Normal
        { state with fullScreen = false }, Cmd.none
    | Exiting ->
        let collection = state.db.GetCollection<PerImageSettings>()

        state.db.BeginTrans() |> ignore
        state.image.source
        |> Option.iter (fun imageSource ->
            imageSource.bitmap.Dispose()
            let serializable = PerImageSettings(
                SourcePath = imageSource.file.Path.ToString(),
                Fov = state.image.fov,
                Distance = state.image.distance,
                Yaw = state.image.yaw,
                Pitch = state.image.pitch,
                Roll = state.image.roll,
                PanX = state.image.pan.X,
                PanY = state.image.pan.Y,
                UseEquirectangular = state.image.useEquirectangular
            )
            collection.Upsert(serializable) |> ignore
        )

        collection.Find(LiteDB.Query.All("Updated", LiteDB.Query.Descending), settingEntriesLimit, 1)
        |> Seq.tryHead
        |> Option.iter (fun oldest -> collection.DeleteMany(Query.LTE("Updated", oldest.Updated)) |> ignore)
        state.db.Commit() |> ignore

        state.db.Dispose()

        state, Cmd.none

    | Exit ->
        host.Close()
        state, Cmd.none

let view (host: HostWindow) (state: State) (dispatch) =
    Grid.create [
        Grid.classes ["root"]
        Grid.allowDrop true
        Grid.onDrop (fun args ->
            match args.DataTransfer.TryGetFile() with
            | :? IStorageFile as file ->
                OpenFile file |> dispatch
            | :? IStorageFolder as folder ->
                OpenFolder folder |> dispatch
            | _ -> ()
        )
        Grid.columnDefinitions [
            ColumnDefinition(1.0, GridUnitType.Star, MinWidth=40.0, MaxWidth=80.0)
            ColumnDefinition(8.0, GridUnitType.Star)
            ColumnDefinition(1.0, GridUnitType.Star, MinWidth=40.0, MaxWidth=80.0)
        ]
        Grid.rowDefinitions "Auto, *"
        Grid.children [
            ImageViewControl.create [
                ImageViewControl.init (fun ivc ->
                    ivc.GestureRecognizers.Add(DragMoveGestureRecognizer())
                    ivc.GestureRecognizers.Add(ExclusivePinchGestureRecognizer())
                )
                ImageViewControl.focusable true
                ImageViewControl.row 0
                ImageViewControl.rowSpan 2
                ImageViewControl.column 0
                ImageViewControl.columnSpan 3
                ImageViewControl.horizontalAlignment HorizontalAlignment.Stretch
                ImageViewControl.verticalAlignment VerticalAlignment.Stretch
                ImageViewControl.image (state.image.source |> Option.map _.bitmap)
                ImageViewControl.fov state.image.fov
                ImageViewControl.distance state.image.distance
                ImageViewControl.direction (Quaternion.fromYawPitchRoll (state.image.yaw |> toRad) (state.image.pitch |> toRad) (state.image.roll |> toRad))
                ImageViewControl.pan state.image.pan
                ImageViewControl.viewEquirectangular state.image.useEquirectangular
                ImageViewControl.onPointerWheelChanged 
                    (fun args ->
                        if args.KeyModifiers.HasFlag(KeyModifiers.Shift) then
                            ZoomFov -args.Delta.Y
                        else
                            Zoom -args.Delta.Y
                        |> dispatch
                    )
                ImageViewControl.onDragMove (fun args ->
                    match args.Source with
                    | :? ImageViewControl as ivc ->
                        args.Handled <- true
                        if args.KeyModifiers.HasFlag KeyModifiers.Shift then
                            (
                                Vector(args.Delta.X, args.Delta.Y),
                                Vector(ivc.Bounds.Width, ivc.Bounds.Height)
                            )
                            |> Roll
                            |> dispatch
                        else
                            (
                                Vector(args.Delta.X, args.Delta.Y),
                                Vector(ivc.Bounds.Width, ivc.Bounds.Height)
                            )
                            |> Move
                            |> dispatch
                    |_ -> ()
                )
                ImageViewControl.onPinch (fun args ->
                    args.Handled <- true
                    Pinch (args.Scale, args.AngleDelta * 1.0<deg>) |> dispatch
                )
                ImageViewControl.onPinchEnded (fun args ->
                    args.Handled <- true
                    PinchEnd |> dispatch
                )
                ImageViewControl.onKeyDown (fun args ->
                    match args.Source with
                    | :? ImageViewControl as ivc ->
                        if args.KeyModifiers = KeyModifiers.None || args.KeyModifiers = KeyModifiers.Shift then
                            let k =
                                if args.KeyModifiers = KeyModifiers.Shift then
                                    20.0
                                else
                                    10.0
                            let move v =
                                (
                                    v,
                                    Vector(ivc.Bounds.Width, ivc.Bounds.Height)
                                )
                                |> Move
                                |> dispatch
                            let roll v =
                                (
                                    v,
                                    Vector(ivc.Bounds.Width, ivc.Bounds.Height)
                                )
                                |> Roll
                                |> dispatch
                            match args.Key with
                            | Key.Left ->
                                args.Handled <- true
                                move (Vector(k, 0.0))
                            | Key.Right ->
                                args.Handled <- true
                                move (Vector(-k, 0.0))
                            | Key.Up ->
                                args.Handled <- true
                                move (Vector(0.0, k))
                            | Key.Down ->
                                args.Handled <- true
                                move (Vector(0.0, -k))
                            | Key.Q ->
                                args.Handled <- true
                                roll (Vector(0.0, k))
                            | Key.E ->
                                args.Handled <- true
                                roll (Vector(0.0, -k))
                            | _ -> ()
                    | _ -> ()
                )
            ]
            Button.create [
                Button.classes ["nav-button"]
                Button.row 1
                Button.column 0
                Button.hotKey (KeyGesture(Key.Left, KeyModifiers.Alt))
                Button.onClick (fun _ -> dispatch PreviousImage)
                Button.content "◀"
                Button.verticalContentAlignment VerticalAlignment.Center
                Button.horizontalAlignment HorizontalAlignment.Stretch
                Button.verticalAlignment VerticalAlignment.Stretch
            ]
            Button.create [
                Button.classes ["nav-button"]
                Button.row 1
                Button.column 2
                Button.hotKey (KeyGesture(Key.Right, KeyModifiers.Alt))
                Button.onClick (fun _ -> dispatch NextImage)
                Button.content "▶"
                Button.verticalContentAlignment VerticalAlignment.Center
                Button.horizontalAlignment HorizontalAlignment.Stretch
                Button.verticalAlignment VerticalAlignment.Stretch
            ]
            Menu.create [
                Menu.row 0
                Menu.column 0
                Menu.columnSpan 3
                Menu.horizontalAlignment HorizontalAlignment.Stretch
                Menu.verticalAlignment VerticalAlignment.Top
                Menu.viewItems [
                    MenuItem.create [
                        MenuItem.header "File"
                        MenuItem.viewItems [
                            MenuItem.create [
                                MenuItem.header "Open"
                                MenuItem.hotKey (KeyGesture(Key.O, KeyModifiers.Control))
                                MenuItem.inputGesture (KeyGesture(Key.O, KeyModifiers.Control))
                                MenuItem.onClick (fun _ -> dispatch SelectImage)
                            ]
                            Separator.create []
                            MenuItem.create [
                                MenuItem.header "Exit"
                                MenuItem.onClick (fun _ -> dispatch Exit)
                            ]
                        ]
                    ]
                    MenuItem.create [
                        MenuItem.header "View"
                        MenuItem.viewItems [
                            MenuItem.create [
                                MenuItem.header "Reset View"
                                MenuItem.hotKey (KeyGesture(Key.R))
                                MenuItem.inputGesture (KeyGesture(Key.R))
                                MenuItem.onClick (fun _ -> ResetView |> dispatch )
                            ]
                            Separator.create []
                            MenuItem.create [
                                MenuItem.header "Flat (2D)"
                                MenuItem.toggleType MenuItemToggleType.Radio
                                MenuItem.isChecked (not state.image.useEquirectangular)
                                MenuItem.onClick (fun _ -> SetViewEquirectangular false |> dispatch)
                            ]
                            MenuItem.create [
                                MenuItem.header "Panorama (360° Spherical)"
                                MenuItem.toggleType MenuItemToggleType.Radio
                                MenuItem.isChecked state.image.useEquirectangular
                                MenuItem.onClick (fun _ -> SetViewEquirectangular true |> dispatch)
                            ]
                            Separator.create []
                            MenuItem.create [
                                MenuItem.header "Zoom in"
                                MenuItem.hotKey (KeyGesture(Key.OemPlus))
                                MenuItem.inputGesture (KeyGesture(Key.OemPlus))
                                MenuItem.onClick (fun _ -> Zoom -1.0 |> dispatch)
                            ]
                            MenuItem.create [
                                MenuItem.header "Zoom out"
                                MenuItem.hotKey (KeyGesture(Key.OemMinus))
                                MenuItem.inputGesture (KeyGesture(Key.OemMinus))
                                MenuItem.onClick (fun _ -> Zoom 1.0 |> dispatch)
                            ]
                            MenuItem.create [
                                MenuItem.header "Warp"
                                MenuItem.hotKey (KeyGesture(Key.OemPlus, KeyModifiers.Shift))
                                MenuItem.inputGesture (KeyGesture(Key.OemPlus, KeyModifiers.Shift))
                                MenuItem.onClick (fun _ -> ZoomFov -1.0 |> dispatch)
                            ]
                            MenuItem.create [
                                MenuItem.header "Dewarp"
                                MenuItem.hotKey (KeyGesture(Key.OemMinus, KeyModifiers.Shift))
                                MenuItem.inputGesture (KeyGesture(Key.OemMinus, KeyModifiers.Shift))
                                MenuItem.onClick (fun _ -> ZoomFov 1.0 |> dispatch)
                            ]
                            Separator.create []
                            MenuItem.create [
                                MenuItem.header "Fullscreen"
                                MenuItem.hotKey (KeyGesture(Key.F11))
                                MenuItem.inputGesture (KeyGesture(Key.F11))
                                MenuItem.toggleType MenuItemToggleType.CheckBox
                                MenuItem.isChecked state.fullScreen
                                MenuItem.onClick (fun _ -> ToggleFullScreen |> dispatch )
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

type MainWindow (args: string array) as this =
    inherit HostWindow()
    do
        base.Title <- "Zenkei Viewer"
        base.Icon <- WindowIcon(Avalonia.Platform.AssetLoader.Open(Uri("avares://ZenkeiViewer/Assets/ZenkeiViewer.ico")))
        base.Classes.Add("main-window") |> ignore

        let subscriptions state = 
            let onKeyDown (dispatch) =
                this.KeyDown.Subscribe(fun e ->
                    match e.Key with
                    | Key.Escape ->
                        dispatch Msg.ExitFullScreen
                    | _ -> ()
                )
            let onClosing (dispatch) =
                this.Closing.Subscribe(fun e ->
                    dispatch Msg.Exiting
                )
            [
                [nameof onKeyDown], onKeyDown
                [nameof onClosing], onClosing
            ]

        Elmish.Program.mkProgram (init this) (update this) (view this)
        |> Program.withHost this
        |> Program.withSubscription subscriptions
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.runWithAvaloniaSyncDispatch args

    let mutable inactiveTimer: IDisposable option = None
    let mutable ignorePointerMoved = false

    member this.Activate () =
        inactiveTimer |> Option.iter _.Dispose()
        inactiveTimer <- (DispatcherTimer.RunOnce((fun () -> this.InActivate()), TimeSpan.FromSeconds(3.0)) |> Some)
        this.PseudoClasses.Remove(":inactive") |> ignore

    member this.InActivate () =
        this.PseudoClasses.Add(":inactive")

    override this.OnPointerMoved (e: PointerEventArgs): unit = 
        if not ignorePointerMoved then
            this.Activate()
        ignorePointerMoved <- false
        base.OnPointerMoved(e: PointerEventArgs)

    override this.OnPointerCaptureLost (e: PointerCaptureLostEventArgs): unit = 
        ignorePointerMoved <- true
        base.OnPointerCaptureLost(e: PointerCaptureLostEventArgs)

    override this.OnPointerReleased (e: PointerReleasedEventArgs): unit = 
        this.Activate()
        base.OnPointerReleased(e)

    override this.OnKeyDown (e: KeyEventArgs): unit = 
        this.Activate()
        base.OnKeyDown(e: KeyEventArgs)

    override this.OnResized (e: WindowResizedEventArgs): unit = 
        this.Activate()
        base.OnResized(e: WindowResizedEventArgs)

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(Avalonia.Themes.Simple.SimpleTheme())
        this.Styles.Add(ZenkeiViewerXaml.ZenkeiViewerStyles())
        //this.RequestedThemeVariant <- Avalonia.Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            let args =
                match desktopLifetime.Args with
                | Null -> [||]
                | NonNull v -> v
            let mainWindow = MainWindow args
            desktopLifetime.MainWindow <- mainWindow
        | _ -> ()

module Program =
    [<EntryPoint>]
    [<STAThread>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .With(Win32PlatformOptions(
                RenderingMode = [
                    Win32RenderingMode.AngleEgl
                    Win32RenderingMode.Wgl
                ],
                WglProfiles = [|
                    OpenGL.GlVersion(OpenGL.GlProfileType.OpenGL, 3, 3)
                |])
            )
            .With(Win32.AngleOptions(
                GlProfiles = [|
                    OpenGL.GlVersion(OpenGL.GlProfileType.OpenGLES, 3, 0)
                |])
            )
            .With(X11PlatformOptions(
                RenderingMode = [
                    X11RenderingMode.Egl
                    X11RenderingMode.Glx
                ],
                GlProfiles = [|
                    OpenGL.GlVersion(OpenGL.GlProfileType.OpenGL, 3, 3, isCompatibilityProfile = true)
                    OpenGL.GlVersion(OpenGL.GlProfileType.OpenGLES, 3, 0)
                |]
            ))
            .StartWithClassicDesktopLifetime(args)

