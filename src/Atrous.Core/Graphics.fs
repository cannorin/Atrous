module Atrous.Graphics
open Atrous.Math

[<Struct>]
type ArgbColor = ArgbColor of a:byte * r:byte * g:byte * b:byte

type BlendMode =
  | None  = 0
  | Alpha = 1
  | Add   = 2
  | Sub   = 3
  | Mul   = 4
  | Inv   = 5

let inline argb a r g b = ArgbColor (byte a, byte r, byte g, byte b)

type TextureCut<'Texture> = {
  texture: 'Texture
  cutArea: RectArea
}

module Texture =
  let inline cut size textureUpLeftLocation texture =
    {
      texture = texture
      cutArea =
        RectArea.FromSize(
          { Width = size.Width; Height = size.Height },
          textureUpLeftLocation,
          Alignment.UpLeft
        )
    }

  let inline cutAsIs size texture = cut size (Vect.XY 0.0<_> 0.0<_>) texture

  let cutMany size (textureUpLeftLocations: _[]) (texture: 'Texture) =
    let cuts = Array.zeroCreate(textureUpLeftLocations.Length)
    // TUNING: can be parallelized
    for i = 0 to textureUpLeftLocations.Length - 1 do
      cuts.[i] <- cut size textureUpLeftLocations.[i] texture
    cuts

[<Struct; StructuralEquality; StructuralComparison>]
type NoDepth = NoDepth
  with
    static member Zero = NoDepth

type LightSprite<'Texture, 'Depth when 'Depth: comparison> = {
  textureCut: TextureCut<'Texture>

  position: Vect
  size:  Size
  angle: int<degree>
  origin: Vect

  color: ArgbColor
  blendMode: BlendMode

  depth: 'Depth
}

module LightSprite =
  let inline fromTextureCut textureCut =
    { textureCut = textureCut
      position = zero
      size = textureCut.cutArea.Size
      angle = zero
      origin = zero
      color = argb 255 255 255 255
      blendMode = BlendMode.Alpha
      depth = zero }
  let inline withPosition position spr = { spr with position = position }
  let inline withSize size spr         = { spr with size = size }
  let inline withAngle angle spr       = { spr with angle = angle }
  let inline withOrigin origin spr     = { spr with origin = origin }
  let inline withOriginAlign align spr =
    let x, y = Alignment.align (spr.size.Width, spr.size.Height) align
    { spr with origin = Vect.XY x y }
  let inline withDrawArea area spr =
    { spr with
        position = !!area.Center
        size     = area.Size
        angle    = area.Inclination }
  let inline withColor color spr       = { spr with color = color }
  let inline withBlendMode mode spr    = { spr with blendMode = mode }
  let inline withDepth depth spr       = { spr with depth = depth }