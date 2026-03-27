module ImageViewControl

// Allow unsafe code for pointer operations
#nowarn 9

open System.Numerics
open FSharp.Control
open Avalonia
open Avalonia.Rendering
open SkiaSharp
open Common

module GlConsts =
    let GL_ZERO = 0
    let GL_ONE = 1
    let GL_TEXTURE_SWIZZLE_R = 0x8E42
    let GL_TEXTURE_SWIZZLE_G = 0x8E43
    let GL_TEXTURE_SWIZZLE_B = 0x8E44
    let GL_TEXTURE_SWIZZLE_A = 0x8E45
    let GL_TEXTURE_WRAP_S = 0x2802
    let GL_TEXTURE_WRAP_T = 0x2803
    let GL_CLAMP = 0x2900
    let GL_REPEAT = 0x2901
    let GL_CLAMP_TO_EDGE = 0x812F
    let GL_MAX_TEXTURE_SIZE = 0x0D33
    let GL_LINES = 0x0001
    let GL_RED = 0x1903
    let GL_GREEN = 0x1904
    let GL_BLUE = 0x1905
    let GL_ALPHA = 0x1906
    let GL_RGB = 0x1907
    let GL_RGBA = 0x1908
    let GL_RGB4 = 0x804F
    let GL_RGB5 = 0x8050
    let GL_RGB8 = 0x8051
    let GL_RGB10 = 0x8052
    let GL_RGB12 = 0x8053
    let GL_RGB16 = 0x8054
    let GL_RGBA2 = 0x8055
    let GL_RGBA4 = 0x8056
    let GL_RGB5_A1 = 0x8057
    let GL_RGBA8 = 0x8058
    let GL_RGB10_A2 = 0x8059
    let GL_RGBA12 = 0x805A
    let GL_RGBA16 = 0x805B
    let GL_RGBA32F = 0x8814
    let GL_RGB32F = 0x8815
    let GL_RGBA16F = 0x881A
    let GL_RGB16F = 0x881B
    let GL_RGBA32UI = 0x8D70
    let GL_RGB32UI = 0x8D71
    let GL_RGBA16UI = 0x8D76
    let GL_RGB16UI = 0x8D77
    let GL_RGBA8UI = 0x8D7C
    let GL_RGB8UI = 0x8D7D
    let GL_RGBA32I = 0x8D82
    let GL_RGB32I = 0x8D83
    let GL_RGBA16I = 0x8D88
    let GL_RGB16I = 0x8D89
    let GL_RGBA8I = 0x8D8E
    let GL_RGB8I = 0x8D8F
    let GL_RED_INTEGER = 0x8D94
    let GL_GREEN_INTEGER = 0x8D95
    let GL_BLUE_INTEGER = 0x8D96
    let GL_RGB_INTEGER = 0x8D98
    let GL_RGBA_INTEGER = 0x8D99
    let GL_BGR_INTEGER = 0x8D9A
    let GL_BGRA_INTEGER = 0x8D9B
    let GL_RG = 0x8227
    let GL_RG_INTEGER = 0x8228
    let GL_R8 = 0x8229
    let GL_R16 = 0x822A
    let GL_RG8 = 0x822B
    let GL_RG16 = 0x822C
    let GL_R16F = 0x822D
    let GL_R32F = 0x822E
    let GL_RG16F = 0x822F
    let GL_RG32F = 0x8230
    let GL_R8I = 0x8231
    let GL_R8UI = 0x8232
    let GL_R16I = 0x8233
    let GL_R16UI = 0x8234
    let GL_R32I = 0x8235
    let GL_R32UI = 0x8236
    let GL_RG8I = 0x8237
    let GL_RG8UI = 0x8238
    let GL_RG16I = 0x8239
    let GL_RG16UI = 0x823A
    let GL_RG32I = 0x823B
    let GL_RG32UI = 0x823C
    let GL_RGB565 = 0x8D62
    let GL_HALF_FLOAT = 0x140B
    let GL_UNSIGNED_BYTE_3_3_2 = 0x8032
    let GL_UNSIGNED_SHORT_4_4_4_4 = 0x8033
    let GL_UNSIGNED_SHORT_5_5_5_1 = 0x8034
    let GL_UNSIGNED_INT_8_8_8_8 = 0x8035
    let GL_UNSIGNED_INT_10_10_10_2 = 0x8036
    let GL_UNSIGNED_BYTE_2_3_3_REV = 0x8362
    let GL_UNSIGNED_SHORT_5_6_5 = 0x8363
    let GL_UNSIGNED_SHORT_5_6_5_REV = 0x8364
    let GL_UNSIGNED_SHORT_4_4_4_4_REV = 0x8365
    let GL_UNSIGNED_SHORT_1_5_5_5_REV = 0x8366
    let GL_UNSIGNED_INT_8_8_8_8_REV = 0x8367
    let GL_UNSIGNED_INT_2_10_10_10_REV = 0x8368

type TextureResource =
    | Empty
    | Staging of NewImage: SKBitmap option * OldTexture: int option
    | Initialized of Texture: int * Image: SKBitmap

type Resources = 
    { Image: TextureResource; Vertices: int option; Shader: int option }

type GlUniform1iDelegate = delegate of int * int -> unit
type GlUniform3fDelegate = delegate of int * float32 * float32 * float32 -> unit
type GlUniform4fDelegate = delegate of int * float32 * float32 * float32 * float32 -> unit
type GlUniformMatrix3fvDelegate = delegate of int * int * bool * voidptr -> unit

module Resources =
    let updateImage (resources: Resources) (image: SKBitmap option) =
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
            resources <- Resources.updateImage resources (arg.NewValue :?> SKBitmap option)
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
                let setTextureImage (bitmap: SKBitmap) =
                    let tex = gl.GenTexture()
                    gl.ActiveTexture(OpenGL.GlConsts.GL_TEXTURE0)
                    gl.BindTexture(OpenGL.GlConsts.GL_TEXTURE_2D, tex)
                    let internalFormat, format, datatype, texParams =
                        match bitmap.ColorType with
                        | SKColorType.Rgba8888 ->
                            OpenGL.GlConsts.GL_RGBA8, OpenGL.GlConsts.GL_RGBA, OpenGL.GlConsts.GL_UNSIGNED_BYTE, []
                        | SKColorType.Rgb888x ->
                            OpenGL.GlConsts.GL_RGBA8, OpenGL.GlConsts.GL_RGBA, OpenGL.GlConsts.GL_UNSIGNED_BYTE, []
                        | SKColorType.Bgra8888 ->
                            OpenGL.GlConsts.GL_RGBA8, OpenGL.GlConsts.GL_RGBA, OpenGL.GlConsts.GL_UNSIGNED_BYTE, [(GlConsts.GL_TEXTURE_SWIZZLE_B, GlConsts.GL_RED); (GlConsts.GL_TEXTURE_SWIZZLE_R, GlConsts.GL_BLUE)]
                        | SKColorType.Rgb565 ->
                            GlConsts.GL_RGB565, GlConsts.GL_RGB, GlConsts.GL_UNSIGNED_SHORT_5_6_5, []
                        | SKColorType.Alpha8 ->
                            GlConsts.GL_R8, GlConsts.GL_RED, OpenGL.GlConsts.GL_UNSIGNED_BYTE, [(GlConsts.GL_TEXTURE_SWIZZLE_G, GlConsts.GL_RED); (GlConsts.GL_TEXTURE_SWIZZLE_B, GlConsts.GL_RED); (GlConsts.GL_TEXTURE_SWIZZLE_A, GlConsts.GL_ONE)]
                        | SKColorType.Rg88 ->
                            GlConsts.GL_RG8, GlConsts.GL_RG, OpenGL.GlConsts.GL_UNSIGNED_BYTE, [(GlConsts.GL_TEXTURE_SWIZZLE_B, GlConsts.GL_ZERO); (GlConsts.GL_TEXTURE_SWIZZLE_A, GlConsts.GL_ONE)]
                        | SKColorType.Argb4444 ->
                            GlConsts.GL_RGBA4, OpenGL.GlConsts.GL_RGBA, GlConsts.GL_UNSIGNED_SHORT_4_4_4_4, []
                        | SKColorType.Rgba1010102
                        | SKColorType.Rgb101010x ->
                            GlConsts.GL_RGB10_A2, OpenGL.GlConsts.GL_RGBA, GlConsts.GL_UNSIGNED_INT_2_10_10_10_REV, []
                        | SKColorType.Bgra1010102
                        | SKColorType.Bgr101010x ->
                            GlConsts.GL_RGB10_A2, OpenGL.GlConsts.GL_RGBA, GlConsts.GL_UNSIGNED_INT_2_10_10_10_REV, [(GlConsts.GL_TEXTURE_SWIZZLE_B, GlConsts.GL_RED); (GlConsts.GL_TEXTURE_SWIZZLE_R, GlConsts.GL_BLUE)]
                        | SKColorType.Gray8 ->
                            GlConsts.GL_R8, GlConsts.GL_RED, OpenGL.GlConsts.GL_UNSIGNED_BYTE, [(GlConsts.GL_TEXTURE_SWIZZLE_G, GlConsts.GL_RED); (GlConsts.GL_TEXTURE_SWIZZLE_B, GlConsts.GL_RED); (GlConsts.GL_TEXTURE_SWIZZLE_A, GlConsts.GL_ONE)]
                        | SKColorType.RgbaF16
                        | SKColorType.RgbaF16Clamped ->
                            GlConsts.GL_RGBA16F, OpenGL.GlConsts.GL_RGBA, GlConsts.GL_HALF_FLOAT, []
                        | SKColorType.AlphaF16 ->
                            GlConsts.GL_R16F, GlConsts.GL_RED, GlConsts.GL_HALF_FLOAT, [(GlConsts.GL_TEXTURE_SWIZZLE_G, GlConsts.GL_RED); (GlConsts.GL_TEXTURE_SWIZZLE_B, GlConsts.GL_RED); (GlConsts.GL_TEXTURE_SWIZZLE_A, GlConsts.GL_ONE)]
                        | SKColorType.RgF16 ->
                            GlConsts.GL_RG16F, GlConsts.GL_RG, GlConsts.GL_HALF_FLOAT, [(GlConsts.GL_TEXTURE_SWIZZLE_B, GlConsts.GL_ZERO); (GlConsts.GL_TEXTURE_SWIZZLE_A, GlConsts.GL_ONE)]
                        | SKColorType.RgbaF32 ->
                            GlConsts.GL_RGBA32F, OpenGL.GlConsts.GL_RGBA, OpenGL.GlConsts.GL_FLOAT, []
                        | SKColorType.Alpha16 ->
                            GlConsts.GL_R16UI, GlConsts.GL_RED_INTEGER, OpenGL.GlConsts.GL_UNSIGNED_SHORT, [(GlConsts.GL_TEXTURE_SWIZZLE_G, GlConsts.GL_RED); (GlConsts.GL_TEXTURE_SWIZZLE_B, GlConsts.GL_RED); (GlConsts.GL_TEXTURE_SWIZZLE_A, GlConsts.GL_ONE)]
                        | SKColorType.Rg1616 ->
                            GlConsts.GL_RG16UI, GlConsts.GL_RG_INTEGER, OpenGL.GlConsts.GL_UNSIGNED_SHORT, []
                        | SKColorType.Rgba16161616 ->
                            GlConsts.GL_RGBA16UI, GlConsts.GL_RGBA_INTEGER, OpenGL.GlConsts.GL_UNSIGNED_SHORT, []
                        |_ ->
                            failwithf "Unsupported colorType %A" bitmap.ColorType
                    gl.TexImage2D(
                        OpenGL.GlConsts.GL_TEXTURE_2D,
                        0,
                        internalFormat,
                        bitmap.Info.Width,
                        bitmap.Info.Height,
                        0,
                        format,
                        datatype,
                        bitmap.GetPixels())
                    texParams
                    |> List.iter (fun (pname, param) ->
                        gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, pname, param)
                    )
                    gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, OpenGL.GlConsts.GL_TEXTURE_MIN_FILTER, OpenGL.GlConsts.GL_LINEAR)
                    gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, OpenGL.GlConsts.GL_TEXTURE_MAG_FILTER, OpenGL.GlConsts.GL_LINEAR)
                    gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, GlConsts.GL_TEXTURE_WRAP_S, GlConsts.GL_REPEAT)
                    gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, GlConsts.GL_TEXTURE_WRAP_T, GlConsts.GL_CLAMP_TO_EDGE)
                    tex
                let resizeBitmap (bitmap : SKBitmap) =
                    let mutable maxSize: int = 4096
                    gl.GetIntegerv(GlConsts.GL_MAX_TEXTURE_SIZE, &maxSize)
                    if bitmap.Info.Width > maxSize || bitmap.Info.Height > maxSize then
                        let newBmp = new SKBitmap(bitmap.Info.WithSize(min maxSize bitmap.Info.Width, min maxSize bitmap.Info.Height))
                        if bitmap.ScalePixels(newBmp, SKFilterQuality.Medium) then
                            newBmp
                        else
                            bitmap
                    else
                        bitmap
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
                vec3 viewVec = viewPos.xyz / viewPos.w - u_cameraWorldPos;
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
        AvaloniaProperty.Register<ImageViewControl, float<deg>>("Fov", 60.0<deg>)
    static let distanceProperty : StyledProperty<float> =
        AvaloniaProperty.Register<ImageViewControl, float>("Distance", 0.0)
    static let directionProperty : StyledProperty<Quaternion> =
        AvaloniaProperty.Register<ImageViewControl, Quaternion>("Direction", Quaternion.Identity)
    static let panProperty =
        AvaloniaProperty.Register<ImageViewControl, Vector>("Pan", Vector.Zero)
    static let imageProperty : StyledProperty<SKBitmap option> =
        AvaloniaProperty.Register<ImageViewControl, SKBitmap option>("Image", None)
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
        with get (): SKBitmap option = this.GetValue(ImageViewControl.ImageProperty)
        and set (value: SKBitmap option) =
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
                let viewProjectionMatrix = Matrix4x4.CreatePerspectiveFieldOfView(fov, aspect |> single, 1.0f + distance, 10.0f + distance)
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
                let imageAspect = (bmp.Info.Width |> float) / (bmp.Info.Height |> float)
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

        static member image<'t when 't :> ImageViewControl>(value: SKBitmap option) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<SKBitmap option>(ImageViewControl.ImageProperty, value, ValueNone)

        static member viewEquirectangular<'t when 't :> ImageViewControl>(value) =
            AttrBuilder<'t>.CreateProperty<bool>(ImageViewControl.ViewEquirectangularProperty, value, ValueNone)

