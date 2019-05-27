module Atrous.ObjectSystem

open System
open System.Collections.Generic

[<Struct>]
type ObjectHandle<'Kind> =
  internal ObjectHandle of k:'Kind * pid:int
with
  member this.kind = match this with ObjectHandle (k, _) -> k

[<Struct; RequireQualifiedAccess>]
type MessageTarget<'Kind> =
  | SpecificHandle of handles:ObjectHandle<'Kind>
  | Kind           of kind:'Kind
  | All

[<Struct>]
type ObjectInfo<'Kind, 'ObjectState> = {
  handle: ObjectHandle<'Kind>
  state:  'ObjectState
}

/// defines the set of write operations which can be performed by objects.
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
[<RequireQualifiedAccess>]
type ScriptOp<'Kind, 'Message, 'ObjectState, 'MediaApi> =
  | Call of 'MediaApi
  | Send of (MessageTarget<'Kind> * 'Message)
  | CreateObject of ( IObjectFactory<'Kind, 'Message, 'ObjectState, 'MediaApi>
                    * 'ObjectState )
  | Wait

and Script<'Kind, 'Message, 'ObjectState, 'MediaApi> =
  seq<ScriptOp<'Kind, 'Message, 'ObjectState, 'MediaApi>>

/// defines a way to create an object.
and IObjectFactory<'Kind, 'Message, 'ObjectState, 'MediaApi> = interface
  abstract member Kind: 'Kind
  /// receives a message and generates the next state.
  abstract member Receive:
       state:       'ObjectState
     * message:     (ObjectHandle<'Kind> * 'Message)
    -> 'ObjectState
  /// initializes the script.
  abstract member CreateScript:
       selfHandle: ObjectHandle<'Kind>
     * selfState:  'ObjectState arrayptr
     * env:        IEnvironmentReader<'Kind, 'ObjectState>
    -> Script<'Kind, 'Message, 'ObjectState, 'MediaApi>
end

/// allows read-only access to the environment.
and IEnvironmentReader<'Kind, 'ObjectState> = interface
  abstract member GetAliveObjectsOfKind: 'Kind -> ObjectInfo<'Kind, 'ObjectState>[]
  abstract member Kinds: 'Kind[]
  abstract member IsAlive: ObjectHandle<'Kind> -> bool
  abstract member GetObjectState: ObjectHandle<'Kind> -> 'ObjectState
  abstract member TryGetObjectState: ObjectHandle<'Kind> -> 'ObjectState option
end

module ObjectFactory =
  let inline create
    (kind: 'Kind)
    (receive: 'ObjectState -> ObjectHandle<'Kind> -> 'Message -> 'ObjectState)
    (createScript: _ -> _ -> _ -> Script<_, _, _, 'MediaApi>) =
    { new IObjectFactory<'Kind, 'Message, 'ObjectState, 'MediaApi> with
      member __.Kind = kind
      member __.Receive (state, (handle, msg))    = receive state handle msg
      member __.CreateScript (handle, state, env) = createScript handle state env }

type internal ObjectGroup<'Kind, 'Message, 'ObjectState, 'MediaApi>
  (kind: 'Kind, parentEnv: IEnvironmentReader<'Kind, 'ObjectState>, capacity: int) =
  class
    let indices = [| 0 .. capacity-1 |]
    let lastPIDs = [| -capacity .. -1 |]
    let isActive = Array.zeroCreate capacity
    let freeIndices = Stack indices
    let states  = Array.zeroCreate capacity
    let objects = Array.zeroCreate capacity
    let scripts = Array.zeroCreate capacity
    let calls = Array.zeroCreate capacity
    let newObjects = Array.zeroCreate capacity
    let interactions = Array.zeroCreate capacity

    member inline private __.IndexToHandle index =
      ObjectHandle(kind, lastPIDs.[index])

    member inline private __.Concat (xs: _ seq[]) =
      seq {
        for i = 0 to capacity-1 do
          if isActive.[i] then
            yield! xs.[i]
      }
    member this.Calls = this.Concat calls
    member this.NewObjects = this.Concat newObjects
    member this.Interactions =
      seq {
        for i = 0 to capacity-1 do
          if isActive.[i] then
            yield! interactions.[i]
                   |> Seq.map (Tuple.map2 id (fun msg -> this.IndexToHandle i, msg))
      }

    member inline private __.GetNewPID i =
      let newLast = lastPIDs.[i] + capacity
      lastPIDs.[i] <- newLast
      newLast

    member this.IsAlive     pid = lastPIDs.[pid % capacity] = pid
    member this.GetState    pid = states.[pid % capacity]
    member this.TryGetState pid =
      let index = pid % capacity
      if lastPIDs.[index] = pid then
        Some states.[index]
      else None

    member this.Processes =
      [|
        for i = 0 to capacity-1 do
          if isActive.[i] then
            yield { handle = this.IndexToHandle i; state = states.[i] }
      |]

    member inline private __.Allocate
      (index: int)
      (objf: IObjectFactory<'Kind, 'Message, 'ObjectState, 'MediaApi>)
      (initState: 'ObjectState) =
      let handle = ObjectHandle(objf.Kind, __.GetNewPID index)
      let aptr = arrayptr(states, index)
      states.[index] <- initState
      objects.[index] <- objf
      scripts.[index] <- objf.CreateScript(handle, aptr, parentEnv).GetEnumerator()
      calls.[index] <- Seq.empty
      newObjects.[index] <- Seq.empty
      interactions.[index] <- Seq.empty
      isActive.[index] <- true
      handle

    member inline private __.Free index =
      isActive.[index] <- false
      dispose scripts.[index]
      freeIndices.Push(index)

    member this.Add
      (objf: IObjectFactory<'Kind, 'Message, 'ObjectState, 'MediaApi>, initState: 'ObjectState) =
      if freeIndices.Count > 0 then
        let i = freeIndices.Pop()
        this.Allocate i objf initState |> ValueSome
      else ValueNone

    member this.AddMany (xs: (_*_)[]) =
      let length = xs.Length
      let allocatedCount = min length freeIndices.Count
      let ret = Array.zeroCreate allocatedCount
      for i = 0 to allocatedCount - 1 do
        let index = freeIndices.Pop()
        let objf, initState = xs.[i]
        ret.[i] <- this.Allocate index objf initState
      if length = allocatedCount then ValueOk ret else ValueError ret

    member private __.UpdateItem index =
      if isActive.[index] then
        let scr = scripts.[index]
        let call = ResizeArray()
        let intr = ResizeArray()
        let nobj = ResizeArray()

        let rec pull () =
          if isNull scr || not <| scr.MoveNext() then false
          else
            match scr.Current with
              | ScriptOp.Call s -> call.Add s; pull()
              | ScriptOp.Send i -> intr.Add i; pull()
              | ScriptOp.CreateObject o -> nobj.Add o; pull()
              | ScriptOp.Wait -> true

        if not <| pull () then
          __.Free index
        else
          calls.[index] <- call :> _
          interactions.[index] <- intr :> _
          newObjects.[index] <- nobj :> _

    //member this.Update() = for i = 0 to capacity-1 do this.UpdateItem i
    member this.Update() = Array.Parallel.iter this.UpdateItem indices

    member inline private this.ReceiveIndex msgs index =
      if isActive.[index] then
        states.[index] <-
          Seq.fold (fun state msg -> objects.[index].Receive(state, msg)) states.[index] msgs
    member this.ReceivePID (p, msgs: (ObjectHandle<'Kind> * 'Message) seq) =
      let index = p % capacity
      if lastPIDs.[index] = p then
        states.[index] <-
          Seq.fold (fun state msg -> objects.[index].Receive(state, msg)) states.[index] msgs
    //member this.ReceiveAll msgs = for i = 0 to capacity-1 do this.ReceiveIndex msgs i
    member this.ReceiveAll msgs = Array.Parallel.iter (this.ReceiveIndex msgs) indices
  end

let private etov x = LanguagePrimitives.EnumToValue x |> int

[<AbstractClass>]
type ObjectEnvironment<'Kind, 'Message, 'ObjectState, 'MediaApi when 'Kind: enum<uint32> and 'Kind: equality>
  (capacity: int) as this =
  class
    let kinds  = [| for v in Enum.GetValues typeof<'Kind> do yield v :?> 'Kind |]
    let length = kinds.Length
    let groups =
      [| for i = 0 to length - 1 do
          yield new ObjectGroup<'Kind, 'Message, 'ObjectState, 'MediaApi>(kinds.[i], this, capacity)
      |]
    
    interface IEnvironmentReader<'Kind, 'ObjectState> with
      member this.Kinds = kinds
      member this.GetAliveObjectsOfKind k =
        groups.[etov k].Processes
      member this.IsAlive (ObjectHandle(k, p)) =
        groups.[etov k].IsAlive(p)
      member this.GetObjectState (ObjectHandle(k, p)) =
        groups.[etov k].GetState p
      member this.TryGetObjectState (ObjectHandle(k, p)) =
        groups.[etov k].TryGetState p

    abstract member ProcessCalls: 'MediaApi seq -> unit

    member inline private this.UpdateGroups() = for group in groups do group.Update()
    member inline private this.ProcessMessages() =
      groups
        |> Seq.collect (fun g -> g.Interactions)
        |> Seq.groupBy item1
        |> Seq.iter (fun (target, xs) ->
            match target with
              | MessageTarget.SpecificHandle (ObjectHandle(k, p)) ->
                groups.[etov k].ReceivePID(p, xs |> Seq.map snd)
              | MessageTarget.Kind k ->
                groups.[etov k].ReceiveAll(xs |> Seq.map snd)
              | MessageTarget.All ->
                for group in groups do
                  group.ReceiveAll(xs |> Seq.map snd)
          )
    member this.Add (objf: IObjectFactory<'Kind, 'Message, 'ObjectState, 'MediaApi>, initState: 'ObjectState) =
      groups.[etov objf.Kind].Add(objf, initState)
    member this.AddManyWithoutCheck (xs: (IObjectFactory<_, _, _, _> * _) seq) =
      xs |> Seq.groupBy (fun (x, _) -> etov x.Kind)
         |> Seq.iter (fun (kind, xs) ->
            groups.[kind].AddMany(xs |> Seq.toArray) |> ignore
          )
    member this.AddMany xs = xs |> Array.map this.Add

    member inline private this.AddNewObjects() =
      groups
        |> Seq.collect (fun g -> g.NewObjects)
        |> this.AddManyWithoutCheck

    member this.Update() =
      this.UpdateGroups()
      this.ProcessMessages()
      this.AddNewObjects()
      this.ProcessCalls(groups |> Seq.collect (fun g -> g.Calls))
  end

module ObjectEnvironment =
  let inline create
    (capacity: int)
    (processCalls: 'MediaApi seq -> unit)
    : ObjectEnvironment<'Kind, 'Message, 'ObjectState, 'MediaApi> =
    { new ObjectEnvironment<'Kind, 'Message, 'ObjectState, 'MediaApi>(capacity) with
      override __.ProcessCalls xs = processCalls xs }

/// defines the set of write operations which can be performed by objects.
module Op =
  /// waits until the next frame.
  let inline wait<'a, 'b, 'c, 'd> : ScriptOp<'a, 'b, 'c, 'd> = ScriptOp.Wait
  /// calls a media API.
  let inline call mediaApi = ScriptOp.Call mediaApi
  /// creates a new object with a specified initial state from an object factory.
  let inline createObj initialState factory = ScriptOp.CreateObject (factory, initialState)
  /// sends a message to specified handles.
  let inline sendToHandle  handle msg = ScriptOp.Send (MessageTarget.SpecificHandle handle, msg)
  /// sends a message to objects of the specified kind.
  let inline sendToKind    kind   msg = ScriptOp.Send (MessageTarget.Kind kind, msg)
  /// sends a message to all objects.
  let inline sendToAll msg = ScriptOp.Send (MessageTarget.All, msg)