module Atrous.Graphics

open Atrous
open Atrous.Math
open Atrous.Graphics

open SFML.Graphics

type TextureCut = TextureCut<Texture>
type LightSprite<'Depth when 'Depth: comparison> = LightSprite<Texture, 'Depth>

module LightSprite =
  open Geometry

  // TUNING: can be parallelized, maybe. ``Array.groupBy >> Parallel.iter``
  /// draw many ``LightSprite``s to ``target`` at once.
  /// minimizes draw call using ``VertexArray``.
  let drawMany (target: RenderTarget) (sprites: LightSprite<_> seq) =
    let sprites =
      // order by depth, then collect the sprites with the same texture
      // texture can't directly be compared, so use texture.NativeHandle
      sprites
        |> Seq.sortBy (fun s ->
          (s.depth, (if isNull s.textureCut.texture then 0u else s.textureCut.texture.NativeHandle), s.blendMode))
        |> Seq.toArray
    if sprites |> Array.isEmpty |> not then
      use quads = new VertexArray(PrimitiveType.Quads)
      let mutable currentTexture = sprites.[0].textureCut.texture
      let mutable currentBlendMode = sprites.[0].blendMode

      for sprite in sprites do
        if (isNull sprite.textureCut.texture <> isNull currentTexture
        || sprite.textureCut.texture.NativeHandle <> currentTexture.NativeHandle
        || sprite.blendMode <> currentBlendMode)
        && quads.VertexCount <> 0u then
          target.Draw(quads, RenderStates(Convert.toImpl currentBlendMode, Transform.Identity, currentTexture, null))
          quads.Clear()
          currentTexture <- sprite.textureCut.texture
          currentBlendMode <- sprite.blendMode
        
        let targetPoints =
          let orig = sprite.position + sprite.origin
          let area =
            RectArea.FromSize(sprite.size, align=Alignment.Center)
            |> move orig
            |> rotateFrom orig sprite.angle
          area.Points
        let texCoords = sprite.textureCut.cutArea.Points
        for j = 0 to 3 do
          let vertex =
            new Vertex(
              Convert.toImpl targetPoints.[j],
              Convert.toImpl sprite.color,
              Convert.toImpl texCoords.[j]
            )
          quads.Append vertex
      if quads.VertexCount <> 0u then
        target.Draw(quads, RenderStates(Convert.toImpl currentBlendMode, Transform.Identity, currentTexture, null))

module RectArea =
  let draw (lineColor: ArgbColor)
           fill
           (target: RenderTarget)
           (rect: RectArea) =
    use varr = new VertexArray()
    let points = rect.Points

    let indices =
      if not fill then
        varr.PrimitiveType <- PrimitiveType.LineStrip
        [| 0;1;2;3;0 |]
      else
        varr.PrimitiveType <- PrimitiveType.Quads
        [| 0..3 |]
    
    for i in indices do
      new Vertex(
        Convert.toImpl points.[i],
        Convert.toImpl lineColor
      ) |> varr.Append

    target.Draw(varr)

  let emptyTexture = new Texture(1u,1u)

  let inline toLightSprite
        (color: ArgbColor)
        (rect: RectArea) : LightSprite< ^a > =
    Texture.cutAsIs rect.Size emptyTexture
    |> LightSprite.fromTextureCut
    |> LightSprite.withColor color
    |> LightSprite.withDrawArea rect
  
  let drawFilledMany (color: ArgbColor) (target: RenderTarget) (rects: RectArea[]) =
    use varr = new VertexArray()
    varr.PrimitiveType <- PrimitiveType.Quads
    for rect in rects do
      for i = 0 to 3 do
        new Vertex(
          Convert.toImpl rect.Points.[i],
          Convert.toImpl color
        ) |> varr.Append
    target.Draw(varr)

module CircleArea =
  let draw (lineColor: ArgbColor)
           fill
           (target: RenderTarget)
           (circ: CircleArea) =
    use shape = new CircleShape(float32 circ.Radius)
    shape.OutlineThickness <- 1.0f
    shape.OutlineColor <- Convert.toImpl lineColor
    if fill then
      shape.FillColor <- Convert.toImpl lineColor
    else
      shape.FillColor <- Color.Transparent
    shape.Origin <- Vect.XY circ.Radius circ.Radius |> Convert.toImpl
    shape.Position <- Convert.toImpl circ.Center
    target.Draw(shape)