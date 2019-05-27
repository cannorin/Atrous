namespace Atrous

type Key = SFML.Window.Keyboard.Key

module Key =
  let [<Literal>] LSuper = Key.LSystem
  let [<Literal>] RSuper = Key.RSystem
  let inline pressed k = SFML.Window.Keyboard.IsKeyPressed k

type Joystick = uint32

module Joystick =
  type J = SFML.Window.Joystick

  type Button =
    | A = 0 | B = 1 | X = 2 | Y = 3
    | LB = 4 | RB = 5
    | LStick = 6 | RStick = 7
    | Back = 8 | Start = 9 | Home = 10
    | Up = 11 | Down = 12 | Left = 13 | Right = 14
    
  let inline leftX (joystick: Joystick) =
    J.GetAxisPosition(joystick, J.Axis.X) |> float
  let inline leftY (joystick: Joystick) =
    J.GetAxisPosition(joystick, J.Axis.Y) |> float
  let inline rightX (joystick: Joystick) =
    J.GetAxisPosition(joystick, J.Axis.U) |> float
  let inline rightY (joystick: Joystick) =
    J.GetAxisPosition(joystick, J.Axis.R) |> float
  let inline leftTrigger (joystick: Joystick) =
    J.GetAxisPosition(joystick, J.Axis.Z) |> float
  let inline rightTrigger (joystick: Joystick) =
    J.GetAxisPosition(joystick, J.Axis.R) |> float

  let inline connected (joystick: Joystick) = J.IsConnected(joystick)

  let inline getAll () : Joystick[] =
    [|
        for i = 0u to 7u do
          if J.IsConnected i then
            yield i
    |]

  let inline pressed (button: int) (joystick: Joystick) =
    J.IsButtonPressed(joystick, uint32 button)