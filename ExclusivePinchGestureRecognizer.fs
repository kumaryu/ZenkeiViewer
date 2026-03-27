module ExclusivePinchGestureRecognizer

open Avalonia
open Avalonia.Input
open Avalonia.Input.GestureRecognizers
open Avalonia.FuncUI.Builder
open Common

[<CustomEquality; CustomComparison>]
type PressedPointer =
    {
        Pointer: IPointer
        StartTime: int64
        StartPoint: Point
        LastPoint: Point
    }
    override this.Equals(obj) =
        match obj with
        | :? PressedPointer as x ->
            x.Pointer.Id = this.Pointer.Id
        | _ ->
            false

    override this.GetHashCode (): int = 
        this.Pointer.Id.GetHashCode()

    interface System.IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? PressedPointer as x ->
                compare this.Pointer.Id x.Pointer.Id
            | _ ->
                invalidArg "obj" "Cannot compare PressedPointer with other types."

module PressedPointer =
    let create pointer startPoint =
        let timestamp = System.Diagnostics.Stopwatch.GetTimestamp()
        { Pointer = pointer; StartTime = timestamp; StartPoint = startPoint; LastPoint = startPoint }
type PointerState =
    {
        Pointer: IPointer
        StartTime: int64
        StartPoint: Point
        LastPoint: Point
    }

type PinchState =
    | NotStarted of Pointers: Map<int, PointerState>
    | DraggingWithRotation of Pointers: Map<int, PointerState>
    | DraggingWithScaling of Pointers: Map<int, PointerState>

// 回転と拡縮が排他操作な PinchGestureRecognizer
type ExclusivePinchGestureRecognizer () =
    inherit GestureRecognizer ()

    let mutable state = NotStarted Map.empty

    let onRotate (eventTarget:IInputElement) current oldest =
        let originalAngle = (atan2 (current.StartPoint.Y - oldest.StartPoint.Y) (current.StartPoint.X - oldest.StartPoint.X)) * 1.0<rad> |> toDeg
        let angle = (atan2 (current.LastPoint.Y - oldest.LastPoint.Y) (current.LastPoint.X - oldest.LastPoint.X)) * 1.0<rad> |> toDeg
        let angleDelta = angle - originalAngle
        let angleDelta =
            if angleDelta > 180.0<deg> then
                angleDelta - 360.0<deg>
            elif angleDelta < -180.0<deg> then
                angleDelta + 360.0<deg>
            else
                angleDelta

        let scaleOrigin = oldest.StartPoint + current.StartPoint / 2.0
        let args = PinchEventArgs(1.0, scaleOrigin, angle / 1.0<deg>, angleDelta / 1.0<deg>)
        eventTarget.RaiseEvent args

    let onScale (eventTarget:IInputElement) current oldest =
        let originalDistance = Vector.Distance(Vector(current.StartPoint.X, current.StartPoint.Y), Vector(oldest.StartPoint.X, oldest.StartPoint.Y))
        let distance = Vector.Distance(Vector(current.LastPoint.X, current.LastPoint.Y), Vector(oldest.LastPoint.X, oldest.LastPoint.Y))
        let distanceScale = distance / originalDistance
        let angle = (atan2 (current.LastPoint.Y - oldest.LastPoint.Y) (current.LastPoint.X - oldest.LastPoint.X)) * 1.0<rad> |> toDeg

        let scaleOrigin = oldest.StartPoint + current.StartPoint / 2.0
        let args = PinchEventArgs(distanceScale, scaleOrigin, angle / 1.0<deg>, 0.0)
        eventTarget.RaiseEvent args

    let pointerReleased (eventTarget:IInputElement | null) pointer =
        match state with
        | NotStarted pointers ->
            // ドラッグが開始されていない場合はドラッグ状態を更新
            if Map.containsKey pointer pointers then
                state <- Map.remove pointer pointers |> NotStarted
                true
            else
                false
        | DraggingWithRotation pointers ->
            if Map.containsKey pointer pointers then
                // ドラッグ中のポインタータイプがリリースされた場合
                let pointers = Map.remove pointer pointers
                if Map.isEmpty pointers then
                    // 最後のポインターがリリースされた場合はドラッグ状態をリセット
                    state <- NotStarted pointers
                    match eventTarget with
                    | Null -> ()
                    | NonNull target ->
                        let args = PinchEndedEventArgs()
                        target.RaiseEvent args
                else
                    // 他のポインターが残っている場合はドラッグ状態を更新
                    state <- DraggingWithRotation pointers
                true
            else
                // 他のポインタータイプがリリースされた場合は無視
                false
        | DraggingWithScaling pointers ->
            if Map.containsKey pointer pointers then
                // ドラッグ中のポインタータイプがリリースされた場合
                let pointers = Map.remove pointer pointers
                if Map.isEmpty pointers then
                    // 最後のポインターがリリースされた場合はドラッグ状態をリセット
                    state <- NotStarted pointers
                    match eventTarget with
                    | Null -> ()
                    | NonNull target ->
                        let args = PinchEndedEventArgs()
                        target.RaiseEvent args
                else
                    // 他のポインターが残っている場合はドラッグ状態を更新
                    state <- DraggingWithScaling pointers
                true
            else
                // 他のポインタータイプがリリースされた場合は無視
                false

    override this.PointerPressed (e: PointerPressedEventArgs): unit = 
        match e.Pointer.Type with
        | PointerType.Touch ->
            let pos = e.GetCurrentPoint(null).Position
            let pointerState = { Pointer=e.Pointer; StartTime = System.Diagnostics.Stopwatch.GetTimestamp(); StartPoint = pos; LastPoint = pos }
            match state with
            | NotStarted pointers -> 
                let pointers = Map.add e.Pointer.Id pointerState pointers
                if Map.count pointers >= 2 then
                    for p in Map.values pointers do
                        this.Capture(p.Pointer)
                    e.PreventGestureRecognition()
                state <- NotStarted pointers
            | DraggingWithScaling pointers ->
                this.Capture(e.Pointer)
                let pointers = Map.add e.Pointer.Id pointerState pointers
                state <- DraggingWithScaling pointers
                e.PreventGestureRecognition()
            | DraggingWithRotation pointers ->
                this.Capture(e.Pointer)
                state <- DraggingWithRotation pointers
                e.PreventGestureRecognition()
        | _ ->
            // 他のポインタータイプは無視
            ()

    override this.PointerReleased (e: PointerReleasedEventArgs): unit = 
        let handled = pointerReleased this.Target e.Pointer.Id
        e.Handled <- handled

    override this.PointerCaptureLost (pointer: IPointer): unit = 
        pointerReleased this.Target pointer.Id |> ignore

    override this.PointerMoved (e: PointerEventArgs): unit = 
        match e.Pointer.Type with
        | PointerType.Touch ->
            match state with
            | NotStarted pointers ->
                if Map.containsKey e.Pointer.Id pointers && Map.count pointers >= 2 then
                    let pos = e.GetCurrentPoint(null).Position
                    let current = { Map.find e.Pointer.Id pointers with LastPoint = pos }
                    let pointers = Map.add e.Pointer.Id current pointers
                    let oldest =
                        pointers
                        |> Map.remove e.Pointer.Id
                        |> Map.values
                        |> Seq.sortBy (fun p -> p.StartTime)
                        |> Seq.head
                    let originalDistance = Vector.Distance(Vector(current.StartPoint.X, current.StartPoint.Y), Vector(oldest.StartPoint.X, oldest.StartPoint.Y))
                    let distance = Vector.Distance(Vector(current.LastPoint.X, current.LastPoint.Y), Vector(oldest.LastPoint.X, oldest.LastPoint.Y))
                    let distanceScale = distance / originalDistance
                    let originalAngle = (atan2 (current.StartPoint.Y - oldest.StartPoint.Y) (current.StartPoint.X - oldest.StartPoint.X)) * 1.0<rad> |> toDeg
                    let angle = (atan2 (current.LastPoint.Y - oldest.LastPoint.Y) (current.LastPoint.X - oldest.LastPoint.X)) * 1.0<rad> |> toDeg
                    let angleDelta = angle - originalAngle
                    if abs (distanceScale - 1.0) > 0.1 then
                        state <- DraggingWithScaling pointers
                        for p in Map.values pointers do
                            this.Capture(p.Pointer)
                    elif abs angleDelta > 5.0<deg> then
                        state <- DraggingWithRotation pointers
                        for p in Map.values pointers do
                            this.Capture(p.Pointer)
                    else
                        state <- NotStarted pointers
                    e.Handled <- true
                    e.PreventGestureRecognition()
                else
                    e.Handled <- false
            | DraggingWithRotation pointers ->
                if Map.containsKey e.Pointer.Id pointers && Map.count pointers >= 2 then
                    let pos = e.GetCurrentPoint(null).Position
                    let current = { Map.find e.Pointer.Id pointers with LastPoint = pos }
                    let pointers = Map.add e.Pointer.Id current pointers
                    let oldest =
                        pointers
                        |> Map.remove e.Pointer.Id
                        |> Map.values
                        |> Seq.sortBy (fun p -> p.StartTime)
                        |> Seq.head
                    match this.Target with
                    | Null -> ()
                    | NonNull target ->
                        onRotate target current oldest

                    state <- DraggingWithRotation pointers
                    e.Handled <- true
                    e.PreventGestureRecognition()
                else
                    // 他のポインタータイプがリリースされた場合は無視
                    e.Handled <- false
            | DraggingWithScaling pointers ->
                if Map.containsKey e.Pointer.Id pointers && Map.count pointers >= 2 then
                    let pos = e.GetCurrentPoint(null).Position
                    let current = { Map.find e.Pointer.Id pointers with LastPoint = pos }
                    let pointers = Map.add e.Pointer.Id current pointers
                    let oldest =
                        pointers
                        |> Map.remove e.Pointer.Id
                        |> Map.values
                        |> Seq.sortBy (fun p -> p.StartTime)
                        |> Seq.head
                    match this.Target with
                    | Null -> ()
                    | NonNull target ->
                        onScale target current oldest

                    state <- DraggingWithScaling pointers
                    e.Handled <- true
                    e.PreventGestureRecognition()
                else
                    // 他のポインタータイプがリリースされた場合は無視
                    e.Handled <- false
        | _ ->
            // 他のポインタータイプは無視
            ()

type InputElement with
    static member onPinch<'t when 't :> InputElement>(func, ?subPatchOptions) =
        AttrBuilder<'t>.CreateSubscription(Gestures.PinchEvent, func, ?subPatchOptions = subPatchOptions)

    static member onPinchEnded<'t when 't :> InputElement>(func, ?subPatchOptions) =
        AttrBuilder<'t>.CreateSubscription(Gestures.PinchEndedEvent, func, ?subPatchOptions = subPatchOptions)

