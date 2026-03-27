namespace ZenkeiViewerXaml
open Avalonia.Styling
open Avalonia.Markup.Xaml

type ZenkeiViewerStyles () as this =
    inherit Styles ()
    do
        AvaloniaXamlLoader.Load(this)
