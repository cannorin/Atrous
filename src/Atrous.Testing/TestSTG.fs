module Atrous.TestSTG

open System
open SFML.Audio
open SFML.Graphics
open SFML.System
open SFML.Window

open Atrous
open Atrous.Math
open Atrous.Graphics
open Atrous.ObjectSystem
open Atrous.Key

open Geometry

type Kind =
  | Player = 0u       | Enemy = 1u
  | PlayerBullet = 2u | EnemyBullet = 3u
  | Entity = 4u       | System = 5u

type Msg =
  | Damage of int
  | AddPoint of int
  | Kill

type State = {
  active: bool
  position: Vect
  hitArea: CircleArea
  hp: int
  damage: int
} with
  static member Zero =
    { active = true; position = Vect.Zero;
      hitArea = CircleArea.Zero; hp = 0; damage = 0 }

type Circle = CircleArea * ArgbColor

let env (window: RenderWindow) : ObjectEnvironment<Kind, Msg, State, Circle> =
  ObjectEnvironment.create
    128
    (fun xs ->
      for area, color in xs do
        CircleArea.draw color false window area
    )

let inline systemObject script =
  ObjectFactory.create
    Kind.System
    (fun state sender msg ->
      match sender.kind, msg with
        | Kind.System, Msg.Kill -> { state with active = false }
        | _, _ -> state
    )
    script

let bulletCollisionWorker bulletKind targetKind =
  systemObject <|
    fun handle state env -> seq {
      while (!!state).active do
        let bullets = env.GetAliveObjectsOfKind bulletKind
        let targets = env.GetAliveObjectsOfKind targetKind
        for bullet in bullets do
          for target in targets do
            if bullet.state.hitArea.Move bullet.state.position
            |> collides <| target.state.hitArea.Move target.state.position then
              yield Op.sendToHandle target.handle <| Msg.Damage bullet.state.damage
        yield Op.wait
    }

let windowWatcher (window: RenderWindow) =
  systemObject <|
    fun handle state env -> seq {
      while (!!state).active do
        let objects =
          env.Kinds |> Seq.filter ((<>) Kind.System)
                    |> Seq.collect env.GetAliveObjectsOfKind
        for object in objects do
          let pos = object.state.position
          if !!pos.X < -100.0<_>
          || float !!pos.X > float window.Size.X + 100.0
          || !!pos.Y < -100.0<_>
          || float !!pos.Y > float window.Size.Y + 100.0 then
            yield Op.sendToHandle object.handle Msg.Kill
        yield Op.wait
    }

let bullet kind angle speed color =
  ObjectFactory.create
    kind
    (fun state sender msg ->
      match sender.kind, msg with
        | _, Damage d ->
          let newHp = state.hp - d
          if newHp > 0 then
            { state with hp = newHp }
          else
            { state with active = false }
        | _, Kill     -> { state with active = false }
        | _, _        -> state
    )
    (fun handle state env -> seq {
      while (!!state).active do
        state @= { !!state with position = (!!state).position + Vect.PC speed angle }
        yield ((!!state).hitArea |> move (!!state).position, color) |> annotate<Circle> |> Op.call
        yield Op.wait
    })

let player (window: RenderWindow) color =
  ObjectFactory.create
    Kind.Player
    (fun state sender msg ->
      state
    )
    (fun handle state env -> seq {
      while (!!state).active do
        if pressed Key.Z then
          let b = bullet Kind.PlayerBullet -90<degree> 10.0<pixel> (argb 255 255 0 0)
          yield b |> Op.createObj {
              State.Zero with
                position = (!!state).position
                hitArea  = CircleArea.create (Vect.Zero, 10.0<_>)
            }
        yield ((!!state).hitArea |> move (!!state).position, color) |> annotate<Circle> |> Op.call
        yield Op.wait
    })

let stage (window: RenderWindow) =
  systemObject <|
    fun handle state env -> seq {
      let pl = player window (argb 255 0 255 0)
      yield pl |> Op.createObj {
        State.Zero with
          position = Vect.XY 200.0<_> 200.0<_>
          hitArea  = CircleArea.create (Vect.Zero, 20.0<_>)
      }
      while (!!state).active do
        yield Op.wait
    }

let init (window: RenderWindow) =
  let env = env window
  let stage = stage window
  let windowWatcher = windowWatcher window
  let bulletCollisionWorker = bulletCollisionWorker Kind.PlayerBullet Kind.Enemy
  for x in [stage; windowWatcher; bulletCollisionWorker] do
    env.Add(x, State.Zero) |> ignore
  env