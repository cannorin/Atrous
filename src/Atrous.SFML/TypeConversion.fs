namespace Atrous

open Atrous.Math
open Atrous.Graphics

open UnitsOfMeasure

[<AutoOpen>]
module TypeConversion =
  type SGB = SFML.Graphics.BlendMode

  type Convert =
    static member inline toImpl (ArgbColor(a,r,g,b)) =
      SFML.Graphics.Color(byte r, byte g, byte b, byte a)
    static member inline fromImpl (c: SFML.Graphics.Color) =
      argb c.A c.R c.G c.B
    static member inline toImpl (v: Vect) =
      SFML.System.Vector2f(!!v.X |> float32, !!v.Y |> float32)
    static member inline fromImpl (v: SFML.System.Vector2f) =
      Vect.XY
        (v.X |> float |> (*) measure<pixel>)
        (v.Y |> float |> (*) measure<pixel>)
    static member inline toImpl (blendMode: BlendMode) =
      match blendMode with
        | BlendMode.Alpha -> SGB.Alpha
        | BlendMode.Add   -> SGB.Add
        | BlendMode.Mul   -> SGB.Multiply
        | BlendMode.Sub   ->
          new SGB(
            SGB.Factor.SrcAlpha,
            SGB.Factor.One,
            SGB.Equation.Subtract
          )
        | BlendMode.Inv   ->
          new SGB(
            SGB.Factor.OneMinusDstColor,
            SGB.Factor.Zero,
            SGB.Equation.Add
          )
        | _ -> SGB.None
