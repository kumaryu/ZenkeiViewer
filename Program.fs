open System.Numerics
open System.Collections.Immutable
open FSharp.Control
open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Media.Imaging
open Avalonia.Input
open Avalonia.Input.GestureRecognizers
open Avalonia.Interactivity
open Avalonia.Rendering
open Avalonia.Platform.Storage
open Avalonia.FuncUI
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open System
open Avalonia.Threading

// Allow unsafe code for pointer operations
#nowarn 9

[<Measure>]
type deg
[<Measure>]
type rad

let toRad (x: float<deg>) : float<rad> = x * System.Math.PI * 1.0<rad> / 180.0<deg>
let toDeg (x: float<rad>) : float<deg> = x * 180.0<deg> / System.Math.PI / 1.0<rad>

let clamp minVal maxVal value =
    if value < minVal then minVal
    elif value > maxVal then maxVal
    else value

let angleFromTo fromDir toDir axis =
    let cross = Vector3.Cross(
        fromDir |> Vector3.Normalize,
        toDir |> Vector3.Normalize
    )
    let len = cross.Length()
    if len < 1e-6f then
        0.0<rad>
    else
        let angle = asin len
        let sign = if Vector3.Dot(cross, axis) < 0.0f then -1.0f else 1.0f
        (angle * sign |> float) * 1.0<rad>

module Quaternion =
    let fromYawPitchRoll yaw pitch roll =
        Quaternion.Concatenate(
            Quaternion.Concatenate(
                Quaternion.CreateFromAxisAngle(Vector3.UnitZ, float32 yaw),
                Quaternion.CreateFromAxisAngle(Vector3.UnitX, float32 pitch)
            ),
            Quaternion.CreateFromAxisAngle(Vector3.UnitY, float32 roll)
        )

module Matrix4x4 =
    let invert (mtx: Matrix4x4) =
        let success, inv = Matrix4x4.Invert(mtx)
        if success then
            inv
        else
            Matrix4x4.Identity

module Cmd =
    module OfTaskOnUIThread =
        let perform (task: 'a -> System.Threading.Tasks.Task<'b>)
                    (arg:'a)
                    (ofSuccess: 'b -> 'msg) : Cmd<'msg> =
            Cmd.OfTask.perform (fun arg ->
                Avalonia.Threading.Dispatcher.UIThread.InvokeAsync<'b>(fun () ->
                    TaskBuilder.task {
                        return! task arg
                    }
                )) arg ofSuccess

module GlConsts =
    let GL_TEXTURE_WRAP_S = 0x2802
    let GL_TEXTURE_WRAP_T = 0x2803
    let GL_CLAMP = 0x2900
    let GL_REPEAT = 0x2901
    let GL_CLAMP_TO_EDGE = 0x812F
    let GL_MAX_TEXTURE_SIZE = 0x0D33
    let GL_LINES = 0x0001

type TextureResource =
    | Empty
    | Staging of NewImage: Bitmap option * OldTexture: int option
    | Initialized of Texture: int * Image: Bitmap

type Resources = 
    { Image: TextureResource; Vertices: int option; Shader: int option }

type GlUniform1iDelegate = delegate of int * int -> unit
type GlUniform3fDelegate = delegate of int * float32 * float32 * float32 -> unit
type GlUniform4fDelegate = delegate of int * float32 * float32 * float32 * float32 -> unit
type GlUniformMatrix3fvDelegate = delegate of int * int * bool * voidptr -> unit

module Resources =
    let updateImage (resources: Resources) (image: Bitmap option) =
        match image with
        | None ->
            match resources.Image with
            | Empty -> { resources with Image = Empty }
            | Staging (_, oldTexture) -> { resources with Image = Staging (None, oldTexture) }
            | Initialized (texture, _) -> { resources with Image = Staging (None, Some texture) }
        | Some _ ->
            match resources.Image with
            | Empty -> { resources with Image = Staging (image, None) }
            | Staging (_, oldTexture) -> { resources with Image = Staging (image, oldTexture) }
            | Initialized (texture, _) -> { resources with Image = Staging (image, Some texture) }

type ImageViewControl() as this =
    inherit OpenGL.Controls.OpenGlControlBase()

    let mutable resources = { Image = Empty; Vertices = None; Shader = None }
    let mutable glUniform1i: GlUniform1iDelegate option = None
    let mutable glUniform3f: GlUniform3fDelegate option = None
    let mutable glUniform4f: GlUniform4fDelegate option = None
    let mutable glUniformMatrix3fv: GlUniformMatrix3fvDelegate option = None

    do
        let requestRendering observable =
            observable
            |> Observable.subscribe (fun _ ->
                this.RequestNextFrameRendering()
            ) |> ignore

        this.GetPropertyChangedObservable(ImageViewControl.FovProperty)
        |> requestRendering

        this.GetPropertyChangedObservable(ImageViewControl.DistanceProperty)
        |> requestRendering

        this.GetPropertyChangedObservable(ImageViewControl.DirectionProperty)
        |> requestRendering

        this.GetPropertyChangedObservable(ImageViewControl.PanProperty)
        |> requestRendering

        this.GetPropertyChangedObservable(ImageViewControl.ImageProperty)
        |> Observable.subscribe (fun arg ->
            resources <- Resources.updateImage resources (arg.NewValue :?> Bitmap option)
            this.RequestNextFrameRendering()
        ) |> ignore

        this.GetPropertyChangedObservable(ImageViewControl.ViewEquirectangularProperty)
        |> requestRendering

    let checkError (gl: OpenGL.GlInterface) =
        let err = gl.GetError()
        match err with
        | OpenGL.GlConsts.GL_NO_ERROR ->
            ()
        | OpenGL.GlConsts.GL_INVALID_ENUM ->
            printfn "OpenGL error: GL_INVALID_ENUM"
        | OpenGL.GlConsts.GL_INVALID_VALUE ->
            printfn "OpenGL error: GL_INVALID_VALUE"
        | OpenGL.GlConsts.GL_INVALID_OPERATION ->
            printfn "OpenGL error: GL_INVALID_OPERATION"
        | OpenGL.GlConsts.GL_STACK_OVERFLOW ->
            printfn "OpenGL error: GL_STACK_OVERFLOW"
        | OpenGL.GlConsts.GL_STACK_UNDERFLOW ->
            printfn "OpenGL error: GL_STACK_UNDERFLOW"
        | OpenGL.GlConsts.GL_OUT_OF_MEMORY ->
            printfn "OpenGL error: GL_OUT_OF_MEMORY"
        | OpenGL.GlConsts.GL_INVALID_FRAMEBUFFER_OPERATION ->
            printfn "OpenGL error: GL_INVALID_FRAMEBUFFER_OPERATION"
        | OpenGL.GlConsts.GL_CONTEXT_LOST ->
            printfn "OpenGL error: GL_CONTEXT_LOST"
        | _ ->
            printfn "OpenGL error: %d" err

    let setupImageResource (gl: OpenGL.GlInterface) resources =
        match resources.Image with
        | Empty ->
            resources, None
        | Staging (newImage, oldTexture) ->
            oldTexture |> Option.iter gl.DeleteTexture
            match newImage with
            | None ->
                { resources with Image = Empty }, None
            | Some bmp ->
                let setTextureImage (bitmap: Bitmap) =
                    let createWriteable (source: Bitmap) =
                        let newBmp = new WriteableBitmap(source.PixelSize, source.Dpi, Avalonia.Platform.PixelFormat.Rgba8888, Avalonia.Platform.AlphaFormat.Premul)
                        use locked = newBmp.Lock()
                        source.CopyPixels(locked, Avalonia.Platform.AlphaFormat.Premul)
                        newBmp
                    use tmpBmp = createWriteable bitmap

                    let tex = gl.GenTexture()
                    gl.ActiveTexture(OpenGL.GlConsts.GL_TEXTURE0)
                    gl.BindTexture(OpenGL.GlConsts.GL_TEXTURE_2D, tex)
                    use lockedBmp = tmpBmp.Lock()
                    gl.TexImage2D(
                        OpenGL.GlConsts.GL_TEXTURE_2D,
                        0, 
                        OpenGL.GlConsts.GL_RGBA,
                        lockedBmp.Size.Width,
                        lockedBmp.Size.Height,
                        0, 
                        OpenGL.GlConsts.GL_RGBA,
                        OpenGL.GlConsts.GL_UNSIGNED_BYTE, lockedBmp.Address)
                    gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, OpenGL.GlConsts.GL_TEXTURE_MIN_FILTER, OpenGL.GlConsts.GL_LINEAR)
                    gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, OpenGL.GlConsts.GL_TEXTURE_MAG_FILTER, OpenGL.GlConsts.GL_LINEAR)
                    gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, GlConsts.GL_TEXTURE_WRAP_S, GlConsts.GL_REPEAT)
                    gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, GlConsts.GL_TEXTURE_WRAP_T, GlConsts.GL_CLAMP_TO_EDGE)
                    tex
                let resizeBitmap (source : Bitmap) =
                    let mutable maxSize: int = 0
                    gl.GetIntegerv(GlConsts.GL_MAX_TEXTURE_SIZE, &maxSize)
                    if source.PixelSize.Width > maxSize || source.PixelSize.Height > maxSize then
                        source.CreateScaledBitmap(PixelSize(min maxSize source.PixelSize.Width, min maxSize source.PixelSize.Height), BitmapInterpolationMode.HighQuality)
                    else
                        source
                let tex =
                    bmp
                    |> resizeBitmap
                    |> setTextureImage
                { resources with Image = Initialized (tex, bmp) }, Some (tex, bmp)
        | Initialized (tex, bmp) ->
            resources, Some (tex, bmp)

    let setupVertexBuffer (gl: OpenGL.GlInterface) resources =
        match resources.Vertices with
        | Some buf ->
            resources, buf
        | None ->
            let vertexArray = gl.GenVertexArray()
            gl.BindVertexArray(vertexArray)
            let buf = gl.GenBuffer()
            gl.BindBuffer(OpenGL.GlConsts.GL_ARRAY_BUFFER, buf)
            use vertices = fixed [|
                -1.0f; -1.0f;
                 3.0f; -1.0f;
                -1.0f;  3.0f;
            |]
            gl.BufferData(OpenGL.GlConsts.GL_ARRAY_BUFFER, (6 * sizeof<float32>) |> nativeint, NativeInterop.NativePtr.toNativeInt vertices, OpenGL.GlConsts.GL_STATIC_DRAW)
            gl.VertexAttribPointer(0, 2, OpenGL.GlConsts.GL_FLOAT, 0, 0, 0)
            gl.EnableVertexAttribArray(0)
            { resources with Vertices = Some vertexArray }, vertexArray

    let shaderSource (glVersion:OpenGL.GlVersion) =
        let vshader = """
        in vec2 position;
        out vec2 v_uv;
        void main() {
            gl_Position = vec4(position, 0.0, 1.0);
            v_uv = position;
        }
        """
        let fshader = """
        in vec2 v_uv;
        uniform bool u_useEquirectangular;
        uniform sampler2D u_tex;
        uniform mat4 u_projectionWorldMatrix;
        uniform mat4 u_textureMatrix;
        uniform vec3 u_cameraWorldPos;
        out vec4 fragColor;
        const float PI = 3.14159265358979323846;
        const float SphereRadius = 1.0;
        const vec3 SpherePos = vec3(0.0, 0.0, 0.0);
        void main() {
            if (u_useEquirectangular) {
                vec4 viewPos = vec4(v_uv, 0.0, 1.0) * u_projectionWorldMatrix;
                vec3 viewVec = vec3(viewPos.xy, viewPos.z / viewPos.w) - u_cameraWorldPos;
                vec3 q = u_cameraWorldPos - SpherePos;
                float r = SphereRadius;
                float a = dot(viewVec, viewVec);
                float b = 2.0 * dot(viewVec, q);
                float c = dot(q, q) - (r * r);
                float d = b * b - 4.0 * a * c;

                if (d<0.0) {
                    fragColor = vec4(0.0, 0.0, 0.0, 1.0);
                }
                else {
                    float t = (-b + sqrt(d)) / (2.0 * a);
                    vec3 hitPos = viewVec * t + q;
                    vec4 hitVec = vec4((hitPos - SpherePos), 0.0) * u_textureMatrix;
                    vec2 uv0 = vec2(atan(hitVec.x, hitVec.y), asin(hitVec.z));
                    vec2 uv = vec2((uv0.x + PI) / 2.0, (PI / 2.0) - uv0.y) / PI;
                    fragColor = texture(u_tex, uv);
                }
            }
            else {
                vec4 viewPos0 = vec4(v_uv, 0.0, 1.0) * u_projectionWorldMatrix;
                vec3 viewVec = vec3(viewPos0.xy, viewPos0.z / viewPos0.w);
                vec4 uv0 = vec4(viewVec.xy, 0.0, 1.0) * u_textureMatrix;
                vec2 uv = vec2((uv0.x + 1.0) / 2.0, (1.0 - uv0.y) / 2.0);
                if (uv.x<0.0 || 1.0<uv.x || uv.y<0.0 || 1.0<uv.y) {
                    fragColor = vec4(0.0, 0.0, 0.0, 1.0);
                }
                else {
                    fragColor = texture(u_tex, uv);
                }
            }
        }
        """
        match glVersion.Type with
        | OpenGL.GlProfileType.OpenGL ->
            let prefix = """#version 330 core
            """
            (prefix + vshader), (prefix + fshader)
        | OpenGL.GlProfileType.OpenGLES ->
            let prefix = """#version 300 es
            precision mediump float;
            """
            (prefix + vshader), (prefix + fshader)
        | _ -> failwith "Unsupported OpenGL version"

    let setupShader (gl: OpenGL.GlInterface) resources =
        match resources.Shader with
        | Some buf ->
            resources, buf
        | None ->
            let vshaderSrc, fshaderSrc = shaderSource gl.ContextInfo.Version
            let vshader = gl.CreateShader(OpenGL.GlConsts.GL_VERTEX_SHADER)
            let err = gl.CompileShaderAndGetError(vshader, vshaderSrc)
            printfn "Vertex shader compile error: %s" err

            let fshader = gl.CreateShader(OpenGL.GlConsts.GL_FRAGMENT_SHADER)
            let err = gl.CompileShaderAndGetError(fshader, fshaderSrc)
            printfn "Fragment shader compile error: %s" err

            let program = gl.CreateProgram()
            gl.AttachShader(program, vshader)
            gl.AttachShader(program, fshader)
            gl.BindAttribLocationString(vshader, 0, "position")
            let err = gl.LinkProgramAndGetError(program)
            printfn "Shader program link error: %s" err

            gl.DeleteShader(vshader)
            gl.DeleteShader(fshader)

            { resources with Shader = Some program }, program

    static let fovProperty : StyledProperty<float<deg>> =
        AvaloniaProperty.Register<ImageViewControl, float<deg>>("Fov", 90.0<deg>)
    static let distanceProperty : StyledProperty<float> =
        AvaloniaProperty.Register<ImageViewControl, float>("Distance", 0.0)
    static let directionProperty : StyledProperty<Quaternion> =
        AvaloniaProperty.Register<ImageViewControl, Quaternion>("Direction", Quaternion.Identity)
    static let panProperty =
        AvaloniaProperty.Register<ImageViewControl, Vector>("Pan", Vector.Zero)
    static let imageProperty : StyledProperty<Bitmap option> =
        AvaloniaProperty.Register<ImageViewControl, Bitmap option>("Image", None)
    static let viewEquirectangularProperty =
        AvaloniaProperty.Register<ImageViewControl, bool>("ViewEquirectangular", false)

    interface ICustomHitTest with
        member this.HitTest(point: Point) =
            this.Bounds.Contains(point)

    static member FovProperty = fovProperty
    static member DistanceProperty = distanceProperty
    static member DirectionProperty = directionProperty
    static member PanProperty = panProperty
    static member ImageProperty = imageProperty
    static member ViewEquirectangularProperty = viewEquirectangularProperty

    member this.Fov
        with get () = this.GetValue(ImageViewControl.FovProperty)
        and set (value) =
            this.SetValue(ImageViewControl.FovProperty, value)
            |> ignore

    member this.Distance
        with get () = this.GetValue(ImageViewControl.DistanceProperty)
        and set (value) =
            this.SetValue(ImageViewControl.DistanceProperty, value)
            |> ignore

    member this.Direction
        with get () = this.GetValue(ImageViewControl.DirectionProperty)
        and set (value) =
            this.SetValue(ImageViewControl.DirectionProperty, value)
            |> ignore

    member this.Pan
        with get () = this.GetValue(ImageViewControl.PanProperty)
        and set (value) =
            this.SetValue(ImageViewControl.PanProperty, value)
            |> ignore

    member this.Image
        with get (): Bitmap option = this.GetValue(ImageViewControl.ImageProperty)
        and set (value: Bitmap option) =
            this.SetValue(ImageViewControl.ImageProperty, value)
            |> ignore

    member this.ViewEquirectangular
        with get () = this.GetValue(ImageViewControl.ViewEquirectangularProperty)
        and set (value) =
            this.SetValue(ImageViewControl.ViewEquirectangularProperty, value)
            |> ignore

    override this.OnOpenGlInit (gl) = 
        printfn "OpenGL initialized with version: %s" (gl.ContextInfo.Version.ToString())
        glUniform1i <- System.Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer<GlUniform1iDelegate>(gl.GetProcAddress("glUniform1i")) |> Some
        glUniform3f <- System.Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer<GlUniform3fDelegate>(gl.GetProcAddress("glUniform3f")) |> Some
        glUniform4f <- System.Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer<GlUniform4fDelegate>(gl.GetProcAddress("glUniform4f")) |> Some
        glUniformMatrix3fv <- System.Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer<GlUniformMatrix3fvDelegate>(gl.GetProcAddress("glUniformMatrix3fv")) |> Some
        base.OnOpenGlInit(gl)

    override this.OnOpenGlRender (gl, fb) = 
        let newResources, tex = setupImageResource gl resources
        let newResources, vertices = setupVertexBuffer gl newResources
        let newResources, shader = setupShader gl newResources
        resources <- newResources

        let renderScaling = (nonNull this.VisualRoot).RenderScaling
        let sz = PixelSize(max 1 (this.Bounds.Width * renderScaling |> int), max 1 (this.Bounds.Height * renderScaling |> int))
        gl.Viewport(0, 0, sz.Width, sz.Height)

        gl.BindFramebuffer(OpenGL.GlConsts.GL_FRAMEBUFFER, fb)
        checkError gl
        gl.ClearColor(0.5f, 0.5f, 0.5f, 1.0f)
        checkError gl
        gl.ClearDepth(1.0f)
        checkError gl
        gl.ClearStencil(0)
        checkError gl
        gl.Clear(OpenGL.GlConsts.GL_COLOR_BUFFER_BIT ||| OpenGL.GlConsts.GL_DEPTH_BUFFER_BIT ||| OpenGL.GlConsts.GL_STENCIL_BUFFER_BIT)
        checkError gl

        match tex with
        | None ->
            ()
        | Some (tex, bmp)  ->
            gl.ActiveTexture(OpenGL.GlConsts.GL_TEXTURE0)
            checkError gl
            gl.BindTexture(OpenGL.GlConsts.GL_TEXTURE_2D, tex)
            checkError gl

            gl.UseProgram(shader)
            checkError gl
            glUniform1i.Value.Invoke(gl.GetUniformLocationString(shader, "u_tex"), 0) // Set texture unit 0
            checkError gl

            glUniform1i.Value.Invoke(gl.GetUniformLocationString(shader, "u_useEquirectangular"), if this.ViewEquirectangular then 1 else 0)
            checkError gl
            
            let fov = this.Fov |> toRad |> single
            let aspect = this.Bounds.Width / this.Bounds.Height
            let distance = this.Distance |> single

            let uniformMatrix4 name (mtx: Matrix4x4) =
                use m = fixed [|
                    mtx.M11; mtx.M12; mtx.M13; mtx.M14;
                    mtx.M21; mtx.M22; mtx.M23; mtx.M24;
                    mtx.M31; mtx.M32; mtx.M33; mtx.M34;
                    mtx.M41; mtx.M42; mtx.M43; mtx.M44;
                |]
                gl.UniformMatrix4fv(gl.GetUniformLocationString(shader, name), 1, true, NativeInterop.NativePtr.toVoidPtr m)
                checkError gl

            if this.ViewEquirectangular then
                let forward = Vector3.UnitY
                let upward = Vector3.UnitZ
                let cameraPos = forward * -distance
                let worldViewMatrix = Matrix4x4.CreateLookTo(cameraPos, forward, upward)
                let viewProjectionMatrix = Matrix4x4.CreatePerspectiveFieldOfView(fov, aspect |> single, 1.0f, 10.0f)
                let projectionWorldMatrix = worldViewMatrix * viewProjectionMatrix |> Matrix4x4.invert
                glUniform3f.Value.Invoke(gl.GetUniformLocationString(shader,"u_cameraWorldPos"), cameraPos.X, cameraPos.Y, cameraPos.Z)
                checkError gl
                uniformMatrix4 "u_projectionWorldMatrix" projectionWorldMatrix

                this.Direction
                |> Quaternion.Inverse // Direction には視点の向きを設定しているが、回転行列としては対象物を回転させる必要があるため逆元を使う
                |> Matrix4x4.CreateFromQuaternion
                |> uniformMatrix4 "u_textureMatrix"
            else
                let forward = -Vector3.UnitZ
                let upward = Vector3.UnitY
                let imageAspect = (bmp.PixelSize.Width |> float) / (bmp.PixelSize.Height |> float)
                let cameraPos = forward * -distance
                let scale = 1.0 / (1.0 + this.Distance)
                let scaleToFit =
                    if imageAspect > aspect then
                        Matrix4x4.CreateScale(1.0f, aspect / imageAspect |> single, 1.0f)
                    else
                        Matrix4x4.CreateScale(imageAspect / aspect |> single, 1.0f, 1.0f)
                let worldViewMatrix =
                    Matrix4x4.CreateTranslation(this.Pan.X |> single, this.Pan.Y |> single, 1.0f) *
                    Matrix4x4.CreateScale(scale |> single, scale |> single, 1.0f) *
                    scaleToFit *
                    Matrix4x4.CreateLookTo(cameraPos, forward, upward)
                let viewProjectionMatrix = Matrix4x4.CreateOrthographicOffCenter(-1.0f, 1.0f, -1.0f, 1.0f, 1.0f, 10.0f)
                let projectionWorldMatrix = worldViewMatrix * viewProjectionMatrix |> Matrix4x4.invert
                glUniform3f.Value.Invoke(gl.GetUniformLocationString(shader,"u_cameraWorldPos"), cameraPos.X, cameraPos.Y, cameraPos.Z)
                checkError gl
                uniformMatrix4 "u_projectionWorldMatrix" projectionWorldMatrix

                Matrix4x4.Identity
                |> uniformMatrix4 "u_textureMatrix"

            gl.BindVertexArray(vertices)
            checkError gl

            gl.DrawArrays(OpenGL.GlConsts.GL_TRIANGLES, 0, 6)
            checkError gl

type DragState =
    | NotStarted
    | Pressed of Pointers: ImmutableHashSet<IPointer> * LastPoint: Point * StartTime: System.DateTime
    | Dragging of Pointers: ImmutableHashSet<IPointer> * LastPoint: Point * StartTime: System.DateTime

type DragMoveEventArgs (event, source: objnull, delta: Avalonia.Vector) =
    inherit RoutedEventArgs (event, source)

    member this.Delta = delta

type DragMoveGestureRecognizer () =
    inherit GestureRecognizer ()

    let mutable dragState = NotStarted
    static let dragMoveEvent = RoutedEvent<DragMoveEventArgs>("DragMove", RoutingStrategies.Bubble, typeof<DragMoveGestureRecognizer>)
    static member DragMoveEvent = dragMoveEvent

    override this.PointerPressed (e: PointerPressedEventArgs): unit = 
        let point = e.GetCurrentPoint(null)
        match dragState with
        | NotStarted ->
            match e.Pointer.Type with
            | PointerType.Mouse when point.Properties.IsLeftButtonPressed ->
                dragState <- Pressed (ImmutableHashSet.Create e.Pointer, point.Position, System.DateTime.Now)
            | PointerType.Touch ->
                dragState <- Pressed (ImmutableHashSet.Create e.Pointer, point.Position, System.DateTime.Now)
            | PointerType.Pen ->
                dragState <- Pressed (ImmutableHashSet.Create e.Pointer, point.Position, System.DateTime.Now)
            | _ ->
                // 他のポインタータイプは無視
                ()
        | Pressed (pointers, lastPoint, startTime) ->
            if pointers.Contains e.Pointer then
                // 既にドラッグ中のポインタータイプの場合は無視
                e.Handled <- true
            else
                // 新しいポインタータイプが追加された場合はドラッグ状態を更新
                dragState <- Pressed (pointers.Add e.Pointer, lastPoint, startTime)
        | Dragging (pointers, lastPoint, startTime) ->
            if pointers.Contains e.Pointer then
                // 既にドラッグ中のポインタータイプの場合は無視
                e.Handled <- true
            else
                // 新しいポインタータイプが追加された場合はドラッグ状態を更新
                dragState <- Dragging (pointers.Add e.Pointer, lastPoint, startTime)

    override this.PointerReleased (e: PointerReleasedEventArgs): unit = 
        match dragState with
        | NotStarted ->
            // ドラッグが開始されていない場合は無視
            e.Handled <- false
        | Pressed (pointers, lastPoint, startTime) ->
            if pointers.Contains e.Pointer then
                // ドラッグ中のポインタータイプがリリースされた場合
                if pointers.Count = 1 then
                    // 最後のポインターがリリースされた場合はドラッグ状態をリセット
                    dragState <- NotStarted
                    e.Handled <- false
                else
                    // 他のポインターが残っている場合はドラッグ状態を更新
                    dragState <- Pressed (pointers.Remove e.Pointer, lastPoint, startTime)
                    e.Handled <- true
            else
                // 他のポインタータイプがリリースされた場合は無視
                e.Handled <- false
        | Dragging (pointers, lastPoint, startTime) ->
            if pointers.Contains e.Pointer then
                // ドラッグ中のポインタータイプがリリースされた場合
                let point = e.GetCurrentPoint(null)
                if pointers.Count = 1 then
                    // 最後のポインターがリリースされた場合はドラッグ状態をリセット
                    dragState <- NotStarted
                else
                    // 他のポインターが残っている場合はドラッグ状態を更新
                    dragState <- Dragging (pointers.Remove e.Pointer, point.Position, startTime)
                e.Handled <- true
            else
                // 他のポインタータイプがリリースされた場合は無視
                e.Handled <- false

    override this.PointerCaptureLost (pointer: IPointer): unit = 
        match dragState with
        | NotStarted ->
            // ドラッグが開始されていない場合は無視
            ()
        | Pressed (pointers, lastPoint, startTime) ->
            if pointers.Contains pointer then
                if pointers.Count = 1 then
                    // 最後のポインターがリリースされた場合はドラッグ状態をリセット
                    dragState <- NotStarted
                else
                    // 他のポインターが残っている場合はドラッグ状態を更新
                    dragState <- Pressed (pointers.Remove pointer, lastPoint, startTime)
        | Dragging (pointers, lastPoint, startTime) ->
            if pointers.Contains pointer then
                if pointers.Count = 1 then
                    // 最後のポインターがリリースされた場合はドラッグ状態をリセット
                    dragState <- NotStarted
                else
                    // 他のポインターが残っている場合はドラッグ状態を更新
                    dragState <- Dragging (pointers.Remove pointer, lastPoint, startTime)

    override this.PointerMoved (e: PointerEventArgs): unit = 
        match dragState with
        | NotStarted ->
            // ドラッグが開始されていない場合は無視
            e.Handled <- false
        | Pressed (pointers, lastPoint, startTime) ->
            if pointers.Contains e.Pointer then
                let point = e.GetCurrentPoint(null)
                let delta = point.Position - lastPoint
                if delta.X * delta.X + delta.Y * delta.Y > 8.0 * 8.0 then
                    let args = DragMoveEventArgs(DragMoveGestureRecognizer.DragMoveEvent, this.Target, Vector(delta.X, delta.Y))
                    match this.Target with
                    | Null -> ()
                    | NonNull target -> target.RaiseEvent args
                    dragState <- Dragging (pointers, point.Position, startTime)
                else
                    ()
                e.Handled <- true
            else
                // 他のポインタータイプがリリースされた場合は無視
                e.Handled <- false
        | Dragging (pointers, lastPoint, startTime) ->
            if pointers.Contains e.Pointer then
                let point = e.GetCurrentPoint(null)
                let delta = point.Position - lastPoint
                let args = DragMoveEventArgs(DragMoveGestureRecognizer.DragMoveEvent, this.Target, Vector(delta.X, delta.Y))
                match this.Target with
                | Null -> ()
                | NonNull target -> target.RaiseEvent args
                dragState <- Dragging (pointers, point.Position, startTime)
                e.Handled <- true
            else
                // 他のポインタータイプがリリースされた場合は無視
                e.Handled <- false


[<AutoOpen>]
module ImageViewControl =
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.FuncUI.Builder

    let create(attrs: IAttr<ImageViewControl> list): IView<ImageViewControl> =
        ViewBuilder.Create<ImageViewControl>(attrs)

    type ImageViewControl with
        static member fov<'t when 't :> ImageViewControl>(value: float<deg>) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float<deg>>(ImageViewControl.FovProperty, value, ValueNone)

        static member distance<'t when 't :> ImageViewControl>(value: float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(ImageViewControl.DistanceProperty, value, ValueNone)

        static member direction<'t when 't :> ImageViewControl>(value: Quaternion) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<Quaternion>(ImageViewControl.DirectionProperty, value, ValueNone)

        static member pan<'t when 't :> ImageViewControl>(value) =
            AttrBuilder<'t>.CreateProperty(ImageViewControl.PanProperty, value, ValueNone)

        static member image<'t when 't :> ImageViewControl>(value: Bitmap option) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<Bitmap option>(ImageViewControl.ImageProperty, value, ValueNone)

        static member viewEquirectangular<'t when 't :> ImageViewControl>(value) =
            AttrBuilder<'t>.CreateProperty<bool>(ImageViewControl.ViewEquirectangularProperty, value, ValueNone)

        static member onDragMove<'t when 't :> InputElement>(func: DragMoveEventArgs-> unit, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription<DragMoveEventArgs>(DragMoveGestureRecognizer.DragMoveEvent, func, ?subPatchOptions = subPatchOptions)

module Viewer =
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout

    type Image = {
        bitmap: Bitmap
        file: IStorageFile
        folder: IStorageFolder option
        equirectangular: bool
    }

    type State = {
        image: Image option
        fov: float<deg>
        distance: float
        yaw: float<deg>
        pitch: float<deg>
        pan: Vector
        useEquirectangular: bool
    }

    type Msg =
    | NextImage
    | PreviousImage
    | SelectImage
    | OpenImage of Image option
    | OpenFile of IStorageFile
    | OpenFolder of IStorageFolder
    | Zoom of float
    | ZoomFov of float
    | DragMove of delta: Vector * size: Vector * renderScaling: float
    | SetViewEquirectangular of bool
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
            return { bitmap=new Bitmap(strm); file=file; folder=folder; equirectangular=useEquirectangular } |> Some
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
            if ext = ".jpg" || ext = ".png" || ext = ".webp" || ext = ".bmp" || ext = ".avif" then
                Some f
            else
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
                Platform.Storage.FilePickerFileType("Image Files", Patterns=["*.jpg"; "*.png"; "*.webp"; "*.bmp"; "*.avif"])
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
        match Array.tryHead args with
        | Some path ->
            let cmd = Cmd.OfTaskOnUIThread.perform (openImageByPath host) path OpenImage
            { fov=90.0<deg>; distance=0.0; image=None; yaw=0.0<deg>; pitch=0.0<deg>; pan=Vector.Zero; useEquirectangular=false }, cmd
        | None ->
            { fov=90.0<deg>; distance=0.0; image=None; yaw=0.0<deg>; pitch=0.0<deg>; pan=Vector.Zero; useEquirectangular=false }, Cmd.none

    let update (host: HostWindow) (msg: Msg) (state: State) =
        match msg with
        | NextImage ->
            match state.image with
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
                let cmd = Cmd.OfTaskOnUIThread.perform getNextImage () OpenImage
                state, cmd
            | _ ->
                state, Cmd.none
        | PreviousImage ->
            match state.image with
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
                let cmd = Cmd.OfTaskOnUIThread.perform getPreviousImage () OpenImage
                state, cmd
            | _ ->
                state, Cmd.none
        | SelectImage ->
            let cmd = Cmd.OfTaskOnUIThread.perform selectImageAsync host OpenImage
            state, cmd
        | OpenImage value ->
            state.image |> Option.iter (fun { bitmap=bmp } -> bmp.Dispose())
            match value with
            | None -> state, Cmd.none
            | Some img -> { state with image = Some img; useEquirectangular = img.equirectangular }, Cmd.none
        | OpenFile file ->
            let cmd = Cmd.OfTaskOnUIThread.perform openImageFileAsync file OpenImage
            state, cmd
        | OpenFolder folder ->
            let cmd = Cmd.OfTaskOnUIThread.perform openImageFileFromFolderAsync folder OpenImage
            state, cmd
        | Zoom delta ->
            let newDist = state.distance + delta * 0.05 |> max -0.9 |> min 5.0
            { state with distance = newDist }, Cmd.none
        | ZoomFov delta ->
            let newFov = state.fov + delta * 0.5<deg> |> max 10.0<deg> |> min 90.0<deg>
            { state with fov = newFov }, Cmd.none
        | DragMove (screenDelta , screenSize, renderScaling) ->
            if state.useEquirectangular then
                let fov = state.fov |> toRad |> single
                let aspect = screenSize.X / screenSize.Y |> single
                let distance = state.distance |> single

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
                let yaw = state.yaw + yawDelta
                let yaw =
                    if yaw > 180.0<deg> then
                        yaw - 360.0<deg>
                    else if yaw < -180.0<deg> then
                        yaw + 360.0<deg>
                    else
                        yaw
                let pitch = clamp -90.0<deg> 90.0<deg> (state.pitch + pitchDelta)
                { state with yaw = yaw; pitch = pitch }, Cmd.none
            else
                match state.image with
                | None ->
                    state, Cmd.none
                | Some { bitmap=bmp } ->
                    let forward = -Vector3.UnitZ
                    let upward = Vector3.UnitY
                    let aspect = screenSize.X / screenSize.Y
                    let imageAspect = (bmp.PixelSize.Width |> float) / (bmp.PixelSize.Height |> float)
                    let cameraPos = forward * single -state.distance
                    let scale = 1.0 / (1.0 + state.distance)
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
                        state.pan.X + float delta.X |> clamp -1.0 1.0,
                        state.pan.Y - float delta.Y |> clamp -1.0 1.0
                    )
                    { state with pan = pan }, Cmd.none
        | SetViewEquirectangular value ->
            { state with useEquirectangular = value }, Cmd.none
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
                    )
                    ImageViewControl.row 0
                    ImageViewControl.rowSpan 2
                    ImageViewControl.column 0
                    ImageViewControl.columnSpan 3
                    ImageViewControl.horizontalAlignment HorizontalAlignment.Stretch
                    ImageViewControl.verticalAlignment VerticalAlignment.Stretch
                    ImageViewControl.image (state.image |> Option.map _.bitmap)
                    ImageViewControl.fov state.fov
                    ImageViewControl.distance state.distance
                    ImageViewControl.direction (Quaternion.fromYawPitchRoll (state.yaw |> toRad) (state.pitch |> toRad) 0.0)
                    ImageViewControl.pan state.pan
                    ImageViewControl.viewEquirectangular state.useEquirectangular
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
                            (
                                Vector(args.Delta.X, args.Delta.Y),
                                Vector(ivc.Bounds.Width, ivc.Bounds.Height),
                                host.RenderScaling
                            )
                            |> DragMove
                            |> dispatch
                        |_ -> ()
                    )
                ]
                Button.create [
                    Button.classes ["nav-button"]
                    Button.row 1
                    Button.column 0
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
                                    MenuItem.onClick (fun _ -> dispatch SelectImage)
                                ]
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
                                    MenuItem.header "Flat (2D)"
                                    MenuItem.toggleType MenuItemToggleType.Radio
                                    MenuItem.isChecked (not state.useEquirectangular)
                                    MenuItem.onClick (fun _ -> SetViewEquirectangular false |> dispatch)
                                ]
                                MenuItem.create [
                                    MenuItem.header "360° Spherical"
                                    MenuItem.toggleType MenuItemToggleType.Radio
                                    MenuItem.isChecked state.useEquirectangular
                                    MenuItem.onClick (fun _ -> SetViewEquirectangular true |> dispatch)
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
        //base.Icon <- WindowIcon(System.IO.Path.Combine("Assets","Icons", "icon.ico"))
        base.Height <- 400.0
        base.Width <- 400.0
        base.Classes.Add("main-window") |> ignore

        Elmish.Program.mkProgram (Viewer.init this) (Viewer.update this) (Viewer.view this)
        |> Program.withHost this
        |> Program.withConsoleTrace
        |> Program.runWith args

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

    override this.OnPointerWheelChanged (e: PointerWheelEventArgs): unit = 
        this.Activate()
        base.OnPointerWheelChanged(e: PointerWheelEventArgs)

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
        this.Styles.Load "avares://ZenkeiViewer/Styles/Styles.axaml"
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
                    //Win32RenderingMode.Wgl
                    Win32RenderingMode.AngleEgl
                ],
                WglProfiles = [|
                    //OpenGL.GlVersion(OpenGL.GlProfileType.OpenGL, 3, 3)
                    OpenGL.GlVersion(OpenGL.GlProfileType.OpenGLES, 3, 0)
                |])
            )
            .StartWithClassicDesktopLifetime(args)

