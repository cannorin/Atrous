open System
open SFML.Audio
open SFML.Graphics
open SFML.System
open SFML.Window

open Atrous
open Atrous.Math
open Atrous.Graphics

[<EntryPoint>]
let main argv =
  let window =
    new RenderWindow(
      new VideoMode(800u, 600u),
      "test",
      Styles.Close
    )
  window.SetVerticalSyncEnabled(true)

  window.Closed.Add <| fun _ -> window.Close()

  let game = Atrous.TestSTG.init window 

  while window.IsOpen do
    window.DispatchEvents()
    window.Clear()
    game.Update()
    window.Display()
  
  0
