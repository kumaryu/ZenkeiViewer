module ImageViewControl

// Allow unsafe code for pointer operations
#nowarn 9

open System.Numerics
open FSharp.Control
open Avalonia
open Avalonia.Rendering
open SkiaSharp
open Common
open FSharp.Collections.Immutable

module private GlConsts =
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

type ResourceSource =
    | Texture of SKBitmap
    | FragmentShader of vertexShader: string * fragmentShader: string
    | VertexArray of size: int * vertices: single array

type ResourceEntry =
    {
        GLResource: int
        Resident: bool
    }

type private ResourceManager =
    {
        Resources: HashMap<ResourceSource, ResourceEntry>
    }

module private ResourceManager =
    let create() =
        { Resources = HashMap.empty }

    let private get source manager =
        manager.Resources
        |> HashMap.tryFind source
        |> Option.map (fun v -> v.GLResource)

    let private getOrCreateInternal resident (gl: OpenGL.GlInterface) source manager =
        let createFragmentProgram (gl: OpenGL.GlInterface) vertexShaderSrc fragmentShaderSrc =
            let vshader = gl.CreateShader(OpenGL.GlConsts.GL_VERTEX_SHADER)
            let err = gl.CompileShaderAndGetError(vshader, vertexShaderSrc) |> defaultIfNull ""
            if err <> "" then
                printfn "Vertex shader compile error: %s" err
            let fshader = gl.CreateShader(OpenGL.GlConsts.GL_FRAGMENT_SHADER)
            let err = gl.CompileShaderAndGetError(fshader, fragmentShaderSrc) |> defaultIfNull ""
            if err <> "" then
                printfn "Fragment shader compile error: %s" err
            let program = gl.CreateProgram()
            gl.AttachShader(program, vshader)
            checkError gl
            gl.AttachShader(program, fshader)
            checkError gl
            let err = gl.LinkProgramAndGetError(program) |> defaultIfNull ""
            if err <> "" then
                printfn "Shader program link error: %s" err
            gl.DeleteShader(vshader)
            checkError gl
            gl.DeleteShader(fshader)
            checkError gl
            gl.BindAttribLocationString(program, 0, "position")
            checkError gl
            program

        let createVertexArray (gl: OpenGL.GlInterface) size (vertices: single array)  =
            let vertexArray = gl.GenVertexArray()
            gl.BindVertexArray(vertexArray)
            checkError gl
            let buf = gl.GenBuffer()
            gl.BindBuffer(OpenGL.GlConsts.GL_ARRAY_BUFFER, buf)
            checkError gl
            use verticesPtr = fixed vertices
            gl.BufferData(OpenGL.GlConsts.GL_ARRAY_BUFFER, (vertices.Length * sizeof<single>) |> nativeint, NativeInterop.NativePtr.toNativeInt verticesPtr, OpenGL.GlConsts.GL_STATIC_DRAW)
            checkError gl
            vertexArray

        let createTexture (gl: OpenGL.GlInterface) (bitmap: SKBitmap) =
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
            let bitmap = resizeBitmap bitmap
            let tex = gl.GenTexture()
            gl.ActiveTexture(OpenGL.GlConsts.GL_TEXTURE0)
            checkError gl
            gl.BindTexture(OpenGL.GlConsts.GL_TEXTURE_2D, tex)
            checkError gl
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
            checkError gl
            texParams
            |> List.iter (fun (pname, param) ->
                gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, pname, param)
                checkError gl
            )
            gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, OpenGL.GlConsts.GL_TEXTURE_MIN_FILTER, OpenGL.GlConsts.GL_LINEAR)
            checkError gl
            gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, OpenGL.GlConsts.GL_TEXTURE_MAG_FILTER, OpenGL.GlConsts.GL_LINEAR)
            checkError gl
            gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, GlConsts.GL_TEXTURE_WRAP_S, GlConsts.GL_REPEAT)
            checkError gl
            gl.TexParameteri(OpenGL.GlConsts.GL_TEXTURE_2D, GlConsts.GL_TEXTURE_WRAP_T, GlConsts.GL_CLAMP_TO_EDGE)
            checkError gl
            tex

        match get source manager with
        | Some res -> res, manager
        | None ->
            let res =
                match source with
                | Texture bmp ->
                    createTexture gl bmp
                | FragmentShader (vsrc, fsrc) ->
                    createFragmentProgram gl vsrc fsrc
                | VertexArray (size, vertices) ->
                    createVertexArray gl size vertices
            res, { manager with Resources = HashMap.add source { GLResource=res; Resident=resident } manager.Resources }

    let getOrCreate gl source manager =
        getOrCreateInternal false gl source manager

    let getOrCreateResident gl source manager =
        getOrCreateInternal true gl source manager

    let cleanup (gl: OpenGL.GlInterface) keeps manager =
        let destroy source resource =
            match source with
            | Texture _ ->
                gl.DeleteTexture(resource.GLResource)
            | FragmentShader _ ->
                gl.DeleteProgram(resource.GLResource)
            | VertexArray _ ->
                gl.DeleteVertexArray(resource.GLResource)

        let keeps =
            manager.Resources
            |> Seq.filter (fun kv -> Seq.contains kv.Value.GLResource keeps || kv.Value.Resident)
            |> HashMap.ofSeq

        manager.Resources
        |> HashMap.except keeps.Keys
        |> Seq.iter (fun kv -> destroy kv.Key kv.Value)

        { manager with Resources = keeps }

type RenderMode =
    | Empty
    | Equirectangular of image: SKBitmap * fov: float<deg> * distance: float * direction: Quaternion
    | Planar of image: SKBitmap * pan: Vector * scale: float * rotation: float<deg>

type GlUniform1iDelegate = delegate of int * int -> unit
type GlUniform3fDelegate = delegate of int * float32 * float32 * float32 -> unit
type GlUniform4fDelegate = delegate of int * float32 * float32 * float32 * float32 -> unit
type GlUniformMatrix3fvDelegate = delegate of int * int * bool * voidptr -> unit

type ImageViewControl() as this =
    inherit OpenGL.Controls.OpenGlControlBase()

    let mutable resourceManager = ResourceManager.create()
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

        this.GetPropertyChangedObservable(ImageViewControl.RenderModeProperty)
        |> requestRendering

    let shaderSourceEquirectangular (glVersion:OpenGL.GlVersion) =
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
        uniform sampler2D u_tex;
        uniform mat4 u_projectionWorldMatrix;
        uniform mat4 u_textureMatrix;
        uniform vec3 u_cameraWorldPos;
        out vec4 fragColor;
        const float PI = 3.14159265358979323846;
        const float SphereRadius = 1.0;
        const vec3 SpherePos = vec3(0.0, 0.0, 0.0);
        void main() {
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

    let shaderSourcePlanar (glVersion:OpenGL.GlVersion) =
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
        uniform sampler2D u_tex;
        uniform mat4 u_projectionWorldMatrix;
        uniform mat4 u_textureMatrix;
        out vec4 fragColor;
        void main() {
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

    static let renderModeProperty =
        AvaloniaProperty.Register<ImageViewControl, RenderMode>("RenderMode", RenderMode.Empty)

    static member RenderModeProperty = renderModeProperty

    interface ICustomHitTest with
        member this.HitTest(point: Point) =
            this.Bounds.Contains(point)

    member this.RenderMode
        with get () = this.GetValue(ImageViewControl.RenderModeProperty)
        and set (value) =
            this.SetValue(ImageViewControl.RenderModeProperty, value)
            |> ignore

    override this.OnOpenGlInit (gl) = 
        glUniform1i <- System.Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer<GlUniform1iDelegate>(gl.GetProcAddress("glUniform1i")) |> Some
        glUniform3f <- System.Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer<GlUniform3fDelegate>(gl.GetProcAddress("glUniform3f")) |> Some
        glUniform4f <- System.Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer<GlUniform4fDelegate>(gl.GetProcAddress("glUniform4f")) |> Some
        glUniformMatrix3fv <- System.Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer<GlUniformMatrix3fvDelegate>(gl.GetProcAddress("glUniformMatrix3fv")) |> Some
        base.OnOpenGlInit(gl)

    override this.OnOpenGlDeinit (gl: OpenGL.GlInterface): unit = 
        resourceManager <- ResourceManager.cleanup gl [] resourceManager
        base.OnOpenGlDeinit(gl)

    override this.OnOpenGlLost (): unit = 
        resourceManager <- ResourceManager.create()
        base.OnOpenGlLost()

    override this.OnOpenGlRender (gl, fb) = 
        let renderScaling = (nonNull this.VisualRoot).RenderScaling
        let aspect = this.Bounds.Width / this.Bounds.Height

        let uniformMatrix4 shader name (mtx: Matrix4x4) =
            use m = fixed [|
                mtx.M11; mtx.M12; mtx.M13; mtx.M14;
                mtx.M21; mtx.M22; mtx.M23; mtx.M24;
                mtx.M31; mtx.M32; mtx.M33; mtx.M34;
                mtx.M41; mtx.M42; mtx.M43; mtx.M44;
            |]
            gl.UniformMatrix4fv(gl.GetUniformLocationString(shader, name), 1, true, NativeInterop.NativePtr.toVoidPtr m)
            checkError gl

        let sz = PixelSize(max 1 (this.Bounds.Width * renderScaling |> int), max 1 (this.Bounds.Height * renderScaling |> int))
        gl.Viewport(0, 0, sz.Width, sz.Height)

        gl.BindFramebuffer(OpenGL.GlConsts.GL_FRAMEBUFFER, fb)
        checkError gl

        let renderEquirectangular (image: SKBitmap, fov: float<deg>, distance: float, direction: Quaternion) =
            let resources = resourceManager
            let tex, resources = ResourceManager.getOrCreate gl (Texture image) resources
            let vertexArray, resources = ResourceManager.getOrCreateResident gl (VertexArray (6, [|-1.0f; -1.0f; 3.0f; -1.0f; -1.0f;  3.0f;|])) resources
            let shader, resources = ResourceManager.getOrCreateResident gl (FragmentShader (shaderSourceEquirectangular gl.ContextInfo.Version)) resources
            resourceManager <- resources

            gl.ActiveTexture(OpenGL.GlConsts.GL_TEXTURE0)
            checkError gl
            gl.BindTexture(OpenGL.GlConsts.GL_TEXTURE_2D, tex)
            checkError gl

            gl.UseProgram(shader)
            checkError gl
            glUniform1i.Value.Invoke(gl.GetUniformLocationString(shader, "u_tex"), 0) // Set texture unit 0
            checkError gl

            let fov = fov |> toRad |> single
            let distance = distance |> single
            let forward = Vector3.UnitY
            let upward = Vector3.UnitZ
            let cameraPos = forward * -distance
            let worldViewMatrix = Matrix4x4.CreateLookTo(cameraPos, forward, upward)
            let viewProjectionMatrix = Matrix4x4.CreatePerspectiveFieldOfView(fov, aspect |> single, 1.0f + distance, 10.0f + distance)
            let projectionWorldMatrix = worldViewMatrix * viewProjectionMatrix |> Matrix4x4.invert
            glUniform3f.Value.Invoke(gl.GetUniformLocationString(shader,"u_cameraWorldPos"), cameraPos.X, cameraPos.Y, cameraPos.Z)
            checkError gl
            uniformMatrix4 shader "u_projectionWorldMatrix" projectionWorldMatrix

            direction
            |> Quaternion.Inverse // Direction には視点の向きを設定しているが、回転行列としては対象物を回転させる必要があるため逆元を使う
            |> Matrix4x4.CreateFromQuaternion
            |> uniformMatrix4 shader "u_textureMatrix"

            gl.BindVertexArray(vertexArray)
            checkError gl
            gl.VertexAttribPointer(0, 2, OpenGL.GlConsts.GL_FLOAT, 0, 0, 0)
            checkError gl
            gl.EnableVertexAttribArray(0)
            checkError gl

            gl.DrawArrays(OpenGL.GlConsts.GL_TRIANGLES, 0, 6)
            checkError gl
            ()

        let renderPlanar (image: SKBitmap, pan: Vector, scale: float, rotation: float<deg>) =
            let resources = resourceManager
            let tex, resources = ResourceManager.getOrCreate gl (Texture image) resources
            let vertexArray, resources = ResourceManager.getOrCreateResident gl (VertexArray (6, [|-1.0f; -1.0f; 3.0f; -1.0f; -1.0f;  3.0f;|])) resources
            let shader, resources = ResourceManager.getOrCreateResident gl (FragmentShader (shaderSourcePlanar gl.ContextInfo.Version)) resources

            gl.ActiveTexture(OpenGL.GlConsts.GL_TEXTURE0)
            checkError gl
            gl.BindTexture(OpenGL.GlConsts.GL_TEXTURE_2D, tex)
            checkError gl

            gl.UseProgram(shader)
            checkError gl
            glUniform1i.Value.Invoke(gl.GetUniformLocationString(shader, "u_tex"), 0) // Set texture unit 0
            checkError gl

            let forward = -Vector3.UnitZ
            let upward = Vector3.UnitY
            let imageAspect = (image.Info.Width |> float) / (image.Info.Height |> float)
            let distance = (1.0 / scale) - 1.0 |> single
            let cameraPos = forward * -distance
            let scaleToFit =
                if imageAspect > aspect then
                    Matrix4x4.CreateScale(1.0f, aspect / imageAspect |> single, 1.0f)
                else
                    Matrix4x4.CreateScale(imageAspect / aspect |> single, 1.0f, 1.0f)
            let worldViewMatrix =
                Matrix4x4.CreateTranslation(pan.X |> single, pan.Y |> single, 1.0f) *
                Matrix4x4.CreateScale(scale |> single, scale |> single, 1.0f) *
                scaleToFit *
                Matrix4x4.CreateLookTo(cameraPos, forward, upward)
            let viewProjectionMatrix = Matrix4x4.CreateOrthographicOffCenter(-1.0f, 1.0f, -1.0f, 1.0f, 1.0f, 10.0f)
            let projectionWorldMatrix = worldViewMatrix * viewProjectionMatrix |> Matrix4x4.invert
            uniformMatrix4 shader "u_projectionWorldMatrix" projectionWorldMatrix

            Matrix4x4.Identity
            |> uniformMatrix4 shader "u_textureMatrix"

            gl.BindVertexArray(vertexArray)
            checkError gl
            gl.VertexAttribPointer(0, 2, OpenGL.GlConsts.GL_FLOAT, 0, 0, 0)
            checkError gl
            gl.EnableVertexAttribArray(0)
            checkError gl

            gl.DrawArrays(OpenGL.GlConsts.GL_TRIANGLES, 0, 6)
            checkError gl

            resourceManager <- ResourceManager.cleanup gl [tex; vertexArray; shader] resources
            ()

        match this.RenderMode with
        | Empty ->
            gl.ClearColor(0.5f, 0.5f, 0.5f, 1.0f)
            checkError gl
            gl.ClearDepth(1.0f)
            checkError gl
            gl.ClearStencil(0)
            checkError gl
            gl.Clear(OpenGL.GlConsts.GL_COLOR_BUFFER_BIT ||| OpenGL.GlConsts.GL_DEPTH_BUFFER_BIT ||| OpenGL.GlConsts.GL_STENCIL_BUFFER_BIT)
            checkError gl
        | Equirectangular (image, fov, distance, direction) ->
            renderEquirectangular(image, fov, distance, direction)
        | Planar (image, pan, scale, rotation) ->
            renderPlanar(image, pan, scale, rotation)

[<AutoOpen>]
module ImageViewControl =
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.FuncUI.Builder

    let create(attrs: IAttr<ImageViewControl> list): IView<ImageViewControl> =
        ViewBuilder.Create<ImageViewControl>(attrs)

    type ImageViewControl with
        static member renderMode<'t when 't :> ImageViewControl>(value: RenderMode) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<RenderMode>(ImageViewControl.RenderModeProperty, value, ValueNone)

