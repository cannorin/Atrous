module Atrous.Math

open System
open UnitsOfMeasure

[<Measure>]
type degree
[<Measure>]
type pixel

let inline degree x =
  let x' = (int x * measure<degree>) % 360<degree>
  if x' >= 0<degree> then x' else 360<degree> + x'

[<Literal>]
let radian = 0.0174532925199433 // Math.PI / 180.0

let inline cos (x: int<'degree>) : float<'a> = Math.Cos(radian * float x) * measure<'a>
let inline sin (x: int<'degree>) : float<'a> = Math.Sin(radian * float x) * measure<'a>
let inline tan (x: int<'degree>) : float<'a> = Math.Tan(radian * float x) * measure<'a>

let inline arccos (x: float<_>) : int<degree> = Math.Acos (float x) / radian |> int |> (*) measure<degree>
let inline arcsin (x: float<_>) : int<degree> = Math.Asin (float x) / radian |> int |> (*) measure<degree>
let inline arctan (x: float<_>) : int<degree> = Math.Atan (float x) / radian |> int |> (*) measure<degree>

let inline arctan2 (y: float<'a>) (x: float<'a>) : int<degree> =
  Math.Atan2(float y, float x) / radian |> int |> (*) measure<degree>

[<Struct>]
type Vect = {
  X: float<pixel> cached
  Y: float<pixel> cached
  Length: float<pixel> cached
  Angle: int<degree> cached
}

let inline private XYVect (x, y) =
  {
    X = cconst x; Y = cconst y;
    Length = cdelay (fun () -> sqrt (x * x + y * y));
    Angle = cdelay (fun () -> arctan2 y x)
  }

let inline private PCVect (length, angle) =
  let angle = degree angle
  {
    Length = cconst length; Angle = cconst angle;
    X = cdelay (fun () -> length * cos angle);
    Y = cdelay (fun () -> length * sin angle)
  }

type Vect with
  static member inline XY x y = 
    {
      X = cconst x; Y = cconst y;
      Length = cdelay (fun () -> sqrt (pow2 x + pow2 y));
      Angle = cdelay (fun () -> arctan2 y x)
    }
  static member inline PC length (angle: int<degree>) =
    let angle = degree angle
    {
      Length = cconst length; Angle = cconst angle;
      X = cdelay (fun () -> length * cos angle);
      Y = cdelay (fun () -> length * sin angle)
    }
  
  static member inline (+) (a, b) = XYVect (!!a.X + !!b.X, !!a.Y + !!b.Y)
  static member inline (-) (a, b) = XYVect (!!a.X - !!b.X, !!a.Y - !!b.Y)
  static member inline (*) (a, m) =
    {
      X = a.X |> Cached.map ((*) m)
      Y = a.Y |> Cached.map ((*) m)
      Length = a.Length |> Cached.map ((*) m)
      Angle = a.Angle
    }
  static member inline (/) (a, m) =
    {
      X = a.X |> Cached.map (fun x -> x / m)
      Y = a.Y |> Cached.map (fun y -> y / m)
      Length = a.Length |> Cached.map (fun l -> l / m)
      Angle = a.Angle
    }
  static member inline DotProduct a b =
    if    a.Length |> Cached.isImmediate && a.Angle |> Cached.isImmediate
       && b.Length |> Cached.isImmediate && b.Angle |> Cached.isImmediate then
      !!a.Length * !!b.Length * cos (!!b.Angle - !!a.Angle)
    else
      !!a.X * !!b.X + !!a.Y * !!b.Y
  static member inline CrossProduct a b =
    if    a.Length |> Cached.isImmediate && a.Angle |> Cached.isImmediate
       && b.Length |> Cached.isImmediate && b.Angle |> Cached.isImmediate then
      !!a.Length * !!b.Length * sin (!!b.Angle - !!a.Angle)
    else
      !!a.X * !!b.Y - !!a.Y * !!b.X
  static member inline ( .*. ) (a, b) = Vect.DotProduct a b
  static member inline ( >*< ) (a, b) = Vect.CrossProduct a b
  static member inline ( --> ) (a: Vect, b: Vect) = b - a
  static member inline ( <-- ) (a: Vect, b: Vect) = a - b

  // >

  static member Zero =
    {
      X = cconst zero; Y = cconst zero
      Length = cconst zero; Angle = cconst zero
    }

  /// ``length^2`` is cheaper than ``length``
  member this.LengthLength =
    if this.Length |> Cached.isImmediate then
      let length = !!this.Length
      pow2 length
    else
      let x = !!this.X
      let y = !!this.Y
      pow2 x + pow2 y
  member inline this.RotateBy angle =
    PCVect (!!this.Length, !!this.Angle + angle)
  member inline this.RotateFrom (orig, angle) =
    orig + (orig --> this).RotateBy angle
  member inline this.ZoomBy zoom =
    this * zoom
  member inline this.ZoomFrom (orig, zoom) =
    orig + (orig --> this).ZoomBy zoom
  member inline this.Move dv = this + dv

[<Struct>]
type Size = { Width: float<pixel>; Height: float<pixel> }
  with
    static member Zero = { Width = zero; Height = zero }
    static member inline (*) (this, zoom) = { Width = this.Width * zoom; Height = this.Height * zoom }
    static member inline (/) (this, zoom) = { Width = this.Width / zoom; Height = this.Height / zoom }

[<Struct>]
type CircleArea = { Center: Vect; Radius: float<pixel> }
  with
    static member Zero = { Center = zero; Radius = zero }
    static member inline (*) (this, zoom) = { Center = this.Center; Radius = this.Radius * zoom }
    static member inline (/) (this, zoom) = { Center = this.Center; Radius = this.Radius / zoom }
    member this.Size = { Width = this.Radius * 2.0; Height = this.Radius * 2.0 }
    member inline this.Includes point =
      (this.Center --> point).LengthLength < this.Radius * this.Radius
    member inline this.RotateBy (_: int<degree>) = this
    member inline this.RotateFrom (orig: Vect, angle) =
      { Radius = this.Radius; Center = this.Center.RotateFrom (orig, angle) }
    member inline this.ZoomBy zoom = this * zoom
    member inline this.ZoomFrom (orig, zoom) =
      { Radius = this.Radius * zoom; Center = orig + (orig --> this.Center).ZoomBy zoom }
    member inline this.Move dv =
      { Radius = this.Radius; Center = this.Center + dv }

module CircleArea =
  let inline create (center, radius) = { Center = center; Radius = radius }

///`````    
///            width           
///        A-----------B
///        |           |
/// height |     x     |
///        |   center  |
///        C-----------D
///``` 
[<Struct>]
type RectArea =
  {
    Size: Size
    Inclination: int<degree>
    Center: Vect cached
    A: Vect cached
    B: Vect cached
    C: Vect cached
    D: Vect cached
  }
  with
    ///```
    ///              origin               
    ///              ↓
    ///        A-----O-----B
    ///        |           |
    /// length |           |
    ///        |           |
    ///        C-----------D
    ///            width
    ///
    ///              ↩ angle (= 0)
    ///```
    static member FromOrigin (origin, width, length, angle) =
      let obc = cdelay <| fun () -> PCVect(width / 2.0, 0<degree>).RotateBy angle
      let bdc = cdelay <| fun () -> PCVect(length, 90<degree>).RotateBy angle
      {
        A = obc |> Cached.map (fun ob -> origin - ob)
        B = obc |> Cached.map (fun ob -> origin + ob)
        C = Do.cached {
          let! ob = obc
          let! bd = bdc
          return origin - ob + bd
        }
        D = Do.cached {
          let! ob = obc
          let! bd = bdc
          return origin + ob + bd
        }
        Center = bdc |> Cached.map (fun bd -> origin + bd / 2.0)
        Size = { Width = width; Height = length }
        Inclination = angle
      }
    
    static member FromSize (size: Size, ?location, ?align: Alignment) =
      let width = size.Width
      let height = size.Height
      let location = location ?| zero
      let align    = align    ?| Alignment.UpLeft

      let w = XYVect (width / 2.0, 0.0<pixel>)
      let h = XYVect (0.0<pixel>, height / 2.0)
      let a = align |> Alignment.align (width, height) |> XYVect
      let loc = location + a
      {
        A = cconst <| loc - w - h; B = cconst <| loc + w - h
        C = cconst <| loc - w + h; D = cconst <| loc + w + h
        Size = { Width = width; Height = height }
        Center = cconst loc; Inclination = zero
      }

    member this.Points = [| !!this.A; !!this.B; !!this.D; !!this.C |]
    member inline this.Width  = this.Size.Width
    member inline this.Height = this.Size.Height
    
    member this.Includes point =
      let this = this
      
      let inline checkTheta b =
        let p = if b then !!this.A else !!this.D
        let pp = p --> if b then !!this.B else !!this.C
        let pm = p --> point
        let inner = Vect.DotProduct pp pm
        let outer = Vect.CrossProduct pp pm
        let theta = atan2 outer inner * 2.0
        -Math.PI < theta && theta < Math.PI && theta <> 0.0

      checkTheta true && checkTheta false

    member this.RotateFrom (orig, angle) =
      let newSlopeAB = this.Inclination + angle
      let newA = (orig --> (!!this.A).RotateBy angle) + orig
      let newAB = XYVect (this.Width * cos newSlopeAB,
                          this.Width * sin newSlopeAB)
      let newAC =
        let nac = XYVect (this.Height * sin newSlopeAB,
                          this.Height * -(cos newSlopeAB))
        if this.Includes orig then
          Vect.Zero - nac
        else
          nac
      let newB = newA + newAB
      let newC = newA + newAC
      let newD = newC + newAB
      {
        A = cconst newA; B = cconst newB; C = cconst newC; D = cconst newD;
        Center = cdelay (fun () -> (newA + newB + newC + newD) / 4.0)
        Size = this.Size
        Inclination = newSlopeAB
      }

    member inline this.RotateBy angle = this.RotateFrom (!!this.Center, angle)

    member inline this.ZoomFrom (orig: Vect, zoom) =
      {
        Size = this.Size * zoom
        Center =
          this.Center |> Cached.map (fun c ->
            orig + (orig --> c).ZoomBy zoom)
        Inclination = this.Inclination
        A = this.A |> Cached.map (fun x -> x.ZoomFrom (orig, zoom))
        B = this.B |> Cached.map (fun x -> x.ZoomFrom (orig, zoom))
        C = this.C |> Cached.map (fun x -> x.ZoomFrom (orig, zoom))
        D = this.D |> Cached.map (fun x -> x.ZoomFrom (orig, zoom))
      }

    member inline this.ZoomBy zoom = this.ZoomFrom (!!this.Center, zoom)

    member inline this.Move dv =
      {
        A = this.A |> Cached.map ((+) dv)
        B = this.B |> Cached.map ((+) dv)
        C = this.C |> Cached.map ((+) dv)
        D = this.D |> Cached.map ((+) dv)
        Center = this.Center |> Cached.map ((+) dv)
        Size = this.Size
        Inclination = this.Inclination
      }

[<Struct>] 
type MixedArea = MixedArea of rects:RectArea array * circles:CircleArea array
  with
    static member Zero = MixedArea ([||], [||])
    static member inline FromRect (x: RectArea) = MixedArea ([|x|], [||])
    static member inline FromCircle (x: CircleArea) = MixedArea ([||], [|x|])

    static member inline (+) (MixedArea(ra, ca), MixedArea(rb, cb)) =
      MixedArea (Array.append ra rb, Array.append ca cb)
    static member inline (+) (MixedArea(ra, ca), b: CircleArea) =
      MixedArea (ra, Array.append ca [| b |])
    static member inline (+) (a: CircleArea, b: MixedArea) = b + a
    static member inline (+) (MixedArea(ra, ca), b: RectArea) =
      MixedArea (Array.append ra [| b |], ca)
    static member inline (+) (a: RectArea, b: MixedArea) = b + a
    
type RectArea with
  static member inline (+) (a: RectArea, b: RectArea) = MixedArea ([| a; b |], [||])
  static member inline (+) (a: RectArea, b: CircleArea) = MixedArea ([| a |], [| b |])
  static member inline (+) (a: CircleArea, b: RectArea) = b + a

type CircleArea with
  static member inline (+) (a: CircleArea, b: CircleArea) = MixedArea ([||], [| a; b |])

[<Sealed>]
type AreaUtils =
  static member inline collides (v: Vect, ca: CircleArea) =
    (ca.Center --> v).LengthLength < ca.Radius * ca.Radius
  static member inline collides (ca: CircleArea, v: Vect) = AreaUtils.collides(v, ca)
  static member inline collides (v: Vect, ra: RectArea) = ra.Includes v
  static member inline collides (ra: RectArea, v: Vect) = AreaUtils.collides(v, ra)
  static member inline collides (c1: CircleArea, c2: CircleArea) =
    let r = c1.Radius + c2.Radius
    (c1.Center --> c2.Center).LengthLength <= pow2 r
  static member inline collides (r1: RectArea, r2: RectArea) =
    r1.Points |> Array.exists (fun x -> AreaUtils.collides(x, r2))
  static member collides (ra: RectArea, ca: CircleArea) =
    let inline checkSides () =
      // TUNING: dirty seq early return
      seq {
        for i in 0..3 do
          let pq = ra.Points.[i] --> ra.Points.[(i+1) % 4]
          let pm = ra.Points.[i] --> ca.Center
          let dp = Vect.DotProduct pq pm
          let k = dp / pq.LengthLength
          if (0.0 <= k && k <= 1.0) then
            let d2 = pm.LengthLength - pow2 dp / pq.LengthLength
            if (d2 < pow2 ca.Radius) then
              yield true
      }
    ra.Points |> Array.exists (fun x -> AreaUtils.collides(x, ca))
      || AreaUtils.collides (ra, ca.Center)
      || checkSides () |> Seq.exists id
  static member inline collides (ca: CircleArea, ra: RectArea) = AreaUtils.collides(ra, ca)
  static member collides (MixedArea (xrs, xcs), MixedArea (yrs, ycs)) =
    // TUNING: dirty seq early return
    seq {
      for xr in xrs do
        for yr in yrs do
          yield AreaUtils.collides (xr, yr)
        for yc in ycs do
          yield AreaUtils.collides (xr, yc)
      for xc in xcs do
        for yr in yrs do
          yield AreaUtils.collides (xc, yr)
        for yc in ycs do
          yield AreaUtils.collides (xc, yc)
    } |> Seq.exists id
  static member inline collides (x: MixedArea, y: CircleArea) = AreaUtils.collides (x, MixedArea.FromCircle y)
  static member inline collides (x: CircleArea, y: MixedArea) = AreaUtils.collides (y, MixedArea.FromCircle x)
  static member inline collides (x: MixedArea, y: RectArea) = AreaUtils.collides (x, MixedArea.FromRect y)
  static member inline collides (x: RectArea, y: MixedArea) = AreaUtils.collides (y, MixedArea.FromRect x)
  static member inline _collides< ^Area, ^A, ^B when (^A or ^B or ^Area): (static member collides: ^A * ^B -> bool) > (a: ^A) (b: ^B) : bool =
    ((^A or ^B or ^Area): (static member collides: ^A * ^B -> bool) a,b)

module Geometry =
  let inline move dv (this: ^X) =
    (^X: (member Move: _ -> _) this,dv)

  let inline rotateBy angle (this: ^X) =
    (^X: (member RotateBy: _ -> _) this,angle)

  let inline rotateFrom orig angle (this: ^X) =
    (^X: (member RotateFrom: _ * _ -> _) this,orig,angle)

  let inline zoomBy zoom (this: ^X) =
    (^X: (member ZoomBy: _ -> _) this,zoom)

  let inline zoomFrom orig zoom (this: ^X) =
    (^X: (member ZoomFrom: _ * _ -> _) this,orig,zoom)

  let inline includes point (area: ^Area) =
    (^Area: (member Includes: _ -> bool) area,point)

  let inline collides (a: ^A) (b: ^B) =
    AreaUtils._collides<AreaUtils, ^A, ^B> a b