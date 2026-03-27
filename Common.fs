module Common

open System.Numerics

[<Measure>]
type deg
[<Measure>]
type rad

let toRad (x: float<deg>) : float<rad> = x * System.Math.PI * 1.0<rad> / 180.0<deg>
let toDeg (x: float<rad>) : float<deg> = x * 180.0<deg> / System.Math.PI / 1.0<rad>

let signf (value: float<'u>) =
    if value < 0.0<_> then -1.0
    elif value > 0.0<_> then 1.0
    else 0.0

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
    let fromYawPitchRoll (yaw: float<rad>) (pitch: float<rad>) (roll: float<rad>) =
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
