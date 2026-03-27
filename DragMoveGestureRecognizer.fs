module DragMoveGestureRecognizer

open System.Collections.Immutable
open Avalonia
open Avalonia.Input
open Avalonia.Input.GestureRecognizers
open Avalonia.Interactivity
open Avalonia.FuncUI.Builder

type DragState =
    | NotStarted
    | Pressed of Pointers: ImmutableHashSet<IPointer> * LastPoint: Point * StartTime: System.DateTime
    | Dragging of Pointers: ImmutableHashSet<IPointer> * LastPoint: Point * StartTime: System.DateTime

type DragMoveEventArgs (event, source: objnull, delta: Avalonia.Vector, keyModifiers: KeyModifiers) =
    inherit RoutedEventArgs (event, source)

    member this.Delta = delta
    member this.KeyModifiers = keyModifiers

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
                    let args = DragMoveEventArgs(DragMoveGestureRecognizer.DragMoveEvent, this.Target, Vector(delta.X, delta.Y), e.KeyModifiers)
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
                let args = DragMoveEventArgs(DragMoveGestureRecognizer.DragMoveEvent, this.Target, Vector(delta.X, delta.Y), e.KeyModifiers)
                match this.Target with
                | Null -> ()
                | NonNull target -> target.RaiseEvent args
                dragState <- Dragging (pointers, point.Position, startTime)
                e.Handled <- true
            else
                // 他のポインタータイプがリリースされた場合は無視
                e.Handled <- false

type InputElement with
    static member onDragMove<'t when 't :> InputElement>(func: DragMoveEventArgs-> unit, ?subPatchOptions) =
        AttrBuilder<'t>.CreateSubscription<DragMoveEventArgs>(DragMoveGestureRecognizer.DragMoveEvent, func, ?subPatchOptions = subPatchOptions)

