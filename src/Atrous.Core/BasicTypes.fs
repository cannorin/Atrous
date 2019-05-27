namespace Atrous

[<AutoOpen>]
module BasicTypes =

  [<System.Flags>]
  type Alignment =
    | Center = 1
    | Up = 2
    | Down = 4
    | Left = 8
    | Right = 16
    | UpLeft = 10
    | DownLeft = 12
    | UpRight = 18
    | DownRight = 20

  module Alignment =
    let inline align (width: float<'a>, height: float<'a>) alignment =
      let hw = width / 2.0 in let hh = height / 2.0 in
      let dx =
        if alignment |> Flag.contains Alignment.Left then
          hw
        else if alignment |> Flag.contains Alignment.Right then
          -hw
        else 
          0.0<_>
      let dy =
        if alignment |> Flag.contains Alignment.Up then
          hh
        else if alignment |> Flag.contains Alignment.Down then
          -hh
        else
          0.0<_>
      (dx, dy)

  [<Struct>]
  type ValueResult<'a, 'b> =
    | ValueOk    of ok:'a
    | ValueError of err:'b

  open System.Runtime.CompilerServices

  type arrayptr<'a>(xs: 'a[], i: int) =
    struct
      member __.Value = xs.[i]
      [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
      member __.Set v = xs.[i] <- v
      static member inline ( @= ) (ptr: 'a arrayptr, value: 'a) = ptr.Set value
    end

  [<StructuredFormatDisplay("{AsString}")>]
  type cached<'a> = class
    val mutable IsDelayed : bool
    val mutable private _Func : unit -> 'a
    val mutable private _Value : 'a
    new (value) = { _Value = value; IsDelayed = false; _Func = Unchecked.defaultof<_> }
    new (func) = { _Value = Unchecked.defaultof<_>; IsDelayed = true; _Func = func }
    member private this.Compute() =
      let v = this._Func()
      this._Value <- v
      this.IsDelayed <- false
      v
    member this.Value =
      if not this.IsDelayed then this._Value
      else this.Compute()
    member this.AsString =
      if not this.IsDelayed then sprintf "%A" this._Value else "(delayed)"
    static member (>>=) (this: cached<'t>, f: 't -> cached<'u>) =
      if not this.IsDelayed then f this._Value
      else cached<'u>(fun () -> !!(f !!this))
    static member inline Return (a: 't) = new cached<'t>(a)
    static member Map (this: cached<'t>, f: 't -> 'u) =
      if not this.IsDelayed then cached<'u>(f this._Value)
      else cached<'u>(fun () -> f !!this)
    static member inline get_Zero() : cached< ^x > = cached< ^x >.Return zero
  end

  type CachedBuilder() =
    member inline __.Bind (m: cached<_>, f) = m >>= f
    member inline __.Return x = cached<_>.Return x
    member inline __.ReturnFrom (m: cached<_>) = m
  
  let inline cconst (x: 'a) = new cached<'a>(x)
  let inline cdelay (f: unit -> 'a) = new cached<'a>(f)

  module Cached =
    let inline isDelayed   (x: cached<_>) = x.IsDelayed
    let inline isImmediate (x: cached<_>) = x.IsDelayed |> not
    let inline bind f (x: cached<_>) = x >>= f
    let inline map  f (x: cached<_>) = cached<_>.Map(x, f)

  module Do =
    let cached = CachedBuilder()