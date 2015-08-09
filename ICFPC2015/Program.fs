// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module DataPart

open System.Text
open FSharp.Data
open Microsoft.FSharp.Text
open Nessos.UnionArgParser

open Printf
open System

let write fmt = ksprintf (fun x -> System.Console.WriteLine x) fmt
let fmt = sprintf
let default_fmt x = fmt "%O" x

let trace x = let () = write "%A" x in x
let traceHex x = let () = write "%X" x in x

//type EIntMap<'x> = FSharpx.Collections.Experimental.IntMap<'x>

//module EIntMap = FSharpx.Collections.Experimental.IntMap

type EIntMap<'x> = IntMap<'x>
module EIntMap = IntMap

type CLIArguments = 
    | [<AltCommandLine("-f")>] Input of string
    | [<AltCommandLine("-t")>] TimeLimit of int
    | [<AltCommandLine("-m")>] MemoryLimit of int
    | [<AltCommandLine("-p")>] PowerPhrase of string
with 
    interface IArgParserTemplate with
        member s.Usage = ""

let parser = UnionArgParser.Create<CLIArguments>()

type InputData = JsonProvider< "sample.json" >
type OutputData = JsonProvider< """ [{ "problemId": 42, "seed": 1337, "tag": "Hello", "solution":  "cthulhu fhtagn!" }] """>

type InputUnit = InputData.Unit
type InputCell = InputData.Member

let printSeq f seq = seq |> Seq.map f 
                         |> String.concat ", "
                         
type CellState = FULL | EMPTY
    with override s.ToString() = fmt "%A" s

type Field = { width: int; height: int; data: CellState EIntMap EIntMap }


type Hex = { x: int; y: int; z: int}
    (*  
        convert cube to odd-r offset
        col = x + (z - (z&1)) / 2
        row = z
        
        # convert odd-r offset to cube
        x = col - (row - (row&1)) / 2
        z = row
        y = -x-z
    *)

    with member this.col with get() = this.x + (this.z - (this.z &&& 1)) /2
         member this.row with get() = this.z
         static member FromOffset (col, row) = 
            let x = col - (row - (row&&&1)) / 2 in
            let z = row in
            let y = (-x) - z in
            { x = x; y = y; z = z }

[<CustomEquality; NoComparison>]
type Unit = { pivot : Hex; filled : Hex list; rotation : int }
    with override x.Equals(yobj) =
               match yobj with
                   | :? Unit as u -> (x.pivot = u.pivot && x.filled = u.filled)
                   | _ -> false
         override s.GetHashCode () = hash (s.pivot, s.filled)

let readUnit (input: InputUnit) =
    let readHex (m: InputCell) = Hex.FromOffset (m.X,  m.Y) in
    { pivot = readHex input.Pivot;
      filled = input.Members |> Seq.map readHex |> Seq.toList;
      rotation = 0 }

let translate { x = dx; y = dy; z = dz } {x = x; y = y; z = z;} =
    { x = x + dx; y = y + dy; z = z + dz}

let ( |.~> ) = translate

let diff { x = sx; y = sy; z = sz } { x = dx; y = dy; z = dz } =
    {x = dx - sx; y = dy - sy; z = dz - sz }

let ( <~.~> ) = diff

let translateUnitByDir dir {pivot = pivot; filled = filled; rotation = rotation} = 
    { pivot = pivot |> translate dir; filled = filled |> List.map (translate dir); rotation = rotation  } 

let translateUnit dest {pivot = pivot; filled = filled; rotation = rotation} = 
    let trans = diff pivot dest in
    { pivot = dest; filled = filled |> List.map (translate trans); rotation = rotation } 

let ( |.~>* ) = translateUnit

let reverse pt = {x = -pt.x; y = -pt.y; z = -pt.z }

let ( ~-. ) = reverse

let origin = { x = 0; y = 0; z = 0 }

type RotateDirection = CW | CCW

let rotateHex dir pivot pt  = 
    let trans = reverse pivot in         // 
    let tpt = translate trans pt in      // move everything to origin
    let opt = match dir with  // rotating in origin is simple
                  | CW -> { x = -tpt.z; y = -tpt.x; z = -tpt.y }
                  | CCW -> { x = -tpt.y; y = -tpt.z; z = -tpt.x }
    in
    translate pivot opt // move rotated points back

let rotateUnitOneTurn dir {pivot = pivot; filled = filled; rotation = rotation}  = 
    { pivot = pivot; filled = filled |> List.map (rotateHex dir pivot); 
        rotation = if dir = CW then (rotation + 1) % 6 else (6 + rotation - 1) % 6 }

let rotateUnit dir amount unit = 
    [1..amount] |> Seq.fold (fun u _ -> rotateUnitOneTurn dir u) unit

let concatBits (x: int32) (y: int32) = 
    let ys: int64 = ((int64 y) <<< 32) in ys ||| (int64 x)
let unconcatBits (v: int64) = 
    let x = v &&& 0xffffffffL in
    let y = (v >>> 32) &&& 0xffffffffL in
    (int32 x, int32 y)   

let getCell (x: int32) (y: int32) (f: Field) = f.data 
                                               |> EIntMap.tryFind x 
                                               |> Option.bind (fun column -> column |> EIntMap.tryFind y)
                                               |> Option.fill EMPTY

let setCell x y st f = f.data
                       |> EIntMap.tryFind x
                       |> Option.fill EIntMap.Empty
                       |> fun column -> match st with FULL -> column.Add(y, st) | EMPTY -> column.Remove(y)
                       |> fun column -> f.data.Add(x, column)
                       |> fun newData -> { f with data = newData }                      

let emptyField w h = { width = w; height = h; data = EIntMap.empty }

let mkField (idata: InputData.Root) = 
    let empty = ref (emptyField idata.Width idata.Height) in
    idata.Filled 
    |> Seq.iter (fun (cell: InputData.Member) -> empty := setCell cell.X cell.Y FULL !empty);
    !empty

let printField f = 
    let builder = new StringBuilder() in
    for j = 0 to f.height do 
        for i = 0 to f.width do 
            getCell i j f |> (function EMPTY -> "_" | FULL -> "@") |> builder.Append |> ignore
        done 
        builder.AppendLine() |> ignore
    done
    builder.ToString()

let apply_unit {filled = filled} field =
    let validCoords (hex: Hex) =
        let x = hex.col in
        let y = hex.row in 
        x >= 0 && x < field.width && y >= 0 && y < field.height in
    let apply field hex = field |> Option.filter (fun _ -> validCoords hex) 
                                |> Option.filter (fun f -> getCell hex.col hex.row f = EMPTY)
                                |> Option.map (setCell hex.col hex.row FULL)
    in
    filled |> Seq.fold apply (Some field) 

let unit_start unit field = 
    let unit_upper = unit.filled |> Seq.minBy (fun h -> h.row) in
    let unit_upper_needed = Hex.FromOffset(unit_upper.col, 0) in
    let pivot = translate (diff unit_upper unit_upper_needed) unit.pivot in
    let unit = translateUnit pivot unit in

    let unit_leftmost = unit.filled |> Seq.minBy (fun h -> h.col) in
    let unit_rightmost = unit.filled |> Seq.maxBy (fun h -> h.col) in

    let unit_width = unit_rightmost.col - unit_leftmost.col + 1 in
    let f_width = field.width in
    let field_middle = field.width / 2 in
    let unit_middle = (unit_leftmost.col + unit_rightmost.col + 1)/2 in

    match (f_width &&& 1, unit_width &&& 1) with
        | (0, 0) | (1, 1) | (1, 0) -> 
                            let vshift = Hex.FromOffset (field_middle - unit_middle, 0) in
                            let pivot = translate vshift unit.pivot in

                            translateUnit pivot unit
        | (0, 1) ->         let vshift = Hex.FromOffset (field_middle - unit_middle - 1, 0) in
                            let pivot = translate vshift unit.pivot in

                            translateUnit pivot unit

let flip f a b = f b a

let (^<|^) f x = f <| x // right assotiative, has lower priority than original
let (^|>^) x f = x |> f

let enumerate_all_positions field unit = 
    seq { for col in 0 .. field.width - 1 do 
            for row in 0 .. field.height - 1 do 
                for rot in 0..5 do
                    let target = Hex.FromOffset(col, row) in
                    yield (unit |> translateUnit target |> rotateUnit CW rot ) }
    |> Seq.map ^<|^ fun unit -> apply_unit unit field ^|>^ Option.map (fun f -> (f, unit))
    |> Seq.filter Option.isSome
    |> Seq.map Option.get 

let sameType (a: 'a) (b: 'a) = a

let field_score field =
    let a = -40 in 
    let b = 100 in
    let c = -30 in
    let d = -70 in
    let e = -20 in
    let howHigh (col: CellState EIntMap) = if col.IsEmpty then 0 else field.height - col.MinimumKey in
    let minHeight, maxHeight = 
                    field.data
                    |> Seq.map (fun kv -> kv.Value |> howHigh)
                    |> (fun h -> (Seq.min h, Seq.max h))
    in 
    let heightAdjust = if maxHeight > 3 * field.height / 4 then 3 * maxHeight else maxHeight in
    let heightAdjust2 = if maxHeight = field.height then 1000 * heightAdjust else heightAdjust in
    let lineComplete j = [0..field.width-1] |> Seq.forall (fun i -> getCell i j field = FULL) in
    let completeLines = [0..field.height-1] |> Seq.countWith lineComplete |> int in
    let bumpiness = Seq.zip [0..field.width-2] [1..field.width-1] 
                    |> Seq.map (
                        fun (x0, x1) ->
                            let hx0 = field.data |> EIntMap.tryFind x0 |> Option.map howHigh |> Option.fill 0 in
                            let hx1 = field.data |> EIntMap.tryFind x1 |> Option.map howHigh |> Option.fill 0 in
                            abs (hx1 - hx0) 
                       )
                    |> Seq.max 
    in
    let holes = field.data 
                |> Seq.map (fun column -> abs(howHigh (column.Value) - column.Value.Count)) 
                |> Seq.sum
    in
    (a * heightAdjust2 + b * completeLines + c * bumpiness + d * holes + e * (maxHeight - minHeight))

let ( |^ ) a b = fun x -> x |> a |> b

type MList<'x> = System.Collections.Generic.List<'x>

let handle_lock field =
    let lines = [0..field.height-1] 
              |> Seq.map (fun line -> [0..field.width-1] |> Seq.map (fun col -> getCell col line field) |> fun s -> new MList<CellState>(s)) 
              |> (fun s -> new MList<MList<CellState>>(s)) |> fun ml -> ml.Reverse(); ml
    in
    let () = [0..field.height-1] 
            |> Seq.iter (fun ix -> if Seq.forall (fun e -> e = FULL) lines.[ix] then lines.[ix] <- null else () ) 
    in
    let _ = lines.RemoveAll(fun x -> x = null) in
    let emptyLine = [0..field.width-1] |> Seq.map(fun _ -> EMPTY) |> fun s -> new MList<CellState>(s) in
    while(lines.Count < field.height) do let _ = lines.Add(emptyLine) in () done;
    let _ = lines.Reverse() in
    let mutable ret = { width = field.width; height = field.height; data = IntMap.Empty } in
    for j = 0 to field.height-1 do
        for i = 0 to field.width-1 do
            ret <- setCell i j (lines.[j].[i]) ret
        done
    done
    ret


let local_random seed = 
    let a = 1103515245L in 
    let m = 1L <<< 32 in 
    let c = 12345L in
    (a * seed + c) % m

let random_seq seed = // WARNING: an infinite sequence :-)
    let seed = int64 seed in
    seq { let current = ref seed in while(true) do yield !current; current := local_random !current; done  }
    |> Seq.map (fun number -> let shifted = number >>> 16 in shifted &&& 0x7fffL)
    


type ShiftDirection = W | E | SW | SE
type Move = MRotate of RotateDirection | MShift of ShiftDirection | Finished

let apply_move move = 
    match move with
        | MShift E  -> translateUnitByDir { x = 1;  y = -1; z = 0 }
        | MShift W  -> translateUnitByDir { x = -1; y = 1;  z = 0 }
        | MShift SE -> translateUnitByDir { x = 0;  y = -1; z = 1 }
        | MShift SW -> translateUnitByDir { x = -1; y = 0;  z = 1 }
        | MRotate dir -> rotateUnitOneTurn dir
        | Finished -> fun x -> x

let valid field unit =
    apply_unit unit field |> Option.isSome

let valid_zipped(field, unit) = valid field unit

let apply_move_zipped (move, unit) = apply_move move unit // for C#
   
let seq_of_units (data: InputData.Root) seed = let unitArray = data.Units |> Array.map readUnit in
                                               let unitArraySize = unitArray.GetLength(0) in
                                               random_seq seed |> Seq.map ^<|^ fun n -> unitArray.[int n % unitArraySize] 

let allMoves = let shifts = [E;W;SE;SW] |> Seq.map MShift in
               let rotates = [CW; CCW] |> Seq.map MRotate in
               Seq.append shifts rotates
let distance hex0 hex1 = [abs (hex0.x - hex1.x); abs (hex0.y - hex1.y); abs (hex0.z - hex1.z)] |> Seq.max
let rdistance unit0 unit1 = let rot = unit0.rotation - unit1.rotation in Seq.max [abs(rot); 6 - abs(rot)]
let heuristic unit0 unit1 = rdistance unit0 unit1 + distance unit0.pivot unit1.pivot
let neighbours unit field = allMoves |> Seq.map (fun move -> (move, apply_move move unit)) |> Seq.filter (fun (m,u) -> valid field u)

let finishingMove unit field = allMoves |> Seq.map (fun move -> (apply_move move unit, move)) 
                                        |> Seq.tryFind (fun (unit, move) -> not (valid field unit))
                                        |> Option.map snd

let canBeFinished unit field = finishingMove unit field |> Option.isSome


type MHashSet<'x> = System.Collections.Generic.HashSet<'x>
type MHashMap<'x, 'y> = System.Collections.Generic.Dictionary<'x, 'y>

type MQueue<'x> = System.Collections.Generic.Queue<'x>

let reachabilitySet unit field = let mutable pending = new MQueue<Unit>() in
                                 let mutable visited = new MHashSet<Unit>() in
                                 
                                 let process_unit u = if(visited.Contains u) 
                                                      then ()
                                                      else (pending.Enqueue u; visited.Add u |> ignore)   
                                 let _ = pending.Enqueue(unit) in
                                 let _ = visited.Add(unit) in

                                 while (not (pending.Count = 0)) do
                                    let element = pending.Dequeue() in
                                    neighbours element field |> Seq.map snd |> Seq.iter process_unit
                                 done
                                 visited

let bestFS src dst field = let mutable opened = new ConcurrentPriorityQueue.ConcurrentPriorityQueue<Unit, int>() in
                           let mutable opened_set = new MHashSet<Unit>() in
                           let mutable gscores = new MHashMap<Unit, int>() in
                           let mutable scores = new MHashMap<Unit, int>() in
                           let mutable parents = new MHashMap<Unit, Move * Unit>() in
                           let getFScore unit = if scores.ContainsKey unit 
                                                then scores.[unit] 
                                                else let score = heuristic unit dst in scores.[unit] <- score; score
                           let getScore unit = - (gscores.[unit] + getFScore unit)
                           let mutable closed =  new MHashSet<Unit>() in
                           let add_opened u = if(not (opened_set.Contains u) && not (closed.Contains u)) 
                                              then( opened.Enqueue(u, getScore u); opened_set.Add(u); () ) in
                           let () = gscores.[src] <- 0 in
                           let () = add_opened src in
                           let mutable current_distance = 0xFFFFFFF in
                           let mutable current: Unit = src in
                           while current <> dst && opened.Count <> 0 do
                                current <- opened.Dequeue();                                
                                ignore (closed.Add current);
                                let next_guys = neighbours current field |> Seq.filter (fun (m,u) -> not (closed.Contains u)) in
                                next_guys |> Seq.iter (fun (move, elem) ->
                                    if not (opened_set.Contains elem) 
                                    then parents.[elem] <- (move, current);gscores.[elem] <- gscores.[current] + 1; add_opened elem;
                                    else if(gscores.[current] + 1 < gscores.[elem]) 
                                         then parents.[elem] <- (move, current);gscores.[elem] <- gscores.[current] + 1;
                                         else ()
                                )
                           done;
                           if current <> dst
                           then []
                           else (seq{
                              let mutable node = (finishingMove current field |> Option.get, current) in
                              yield node;
                              while (snd node) <> src do
                                 let _ = node <- parents.[snd node] in
                                 yield node;
                           } |> Seq.toList |> List.rev)


let enumerate_finishable field unit = let reachable = reachabilitySet unit field in
                                       enumerate_all_positions field unit 
                                       |> Seq.filter (fun (_, u) -> (reachable.Contains u)) 
                                       |> Seq.filter (fun (_, unit) -> canBeFinished unit field)
                                    

let best_moves unit field = enumerate_finishable field unit 
                         |> Seq.sortBy ( fun (field, u) -> (- field_score field + u.rotation))

let best_move unit field = best_moves unit field |> Seq.map (fun res -> bestFS unit (snd res) field) |> Seq.tryFind (fun lst -> lst <> []) |> Option.fill []

let best_move_zipped(unit, field) = best_move unit field

let doit argv = 
    let args = parser.Parse(argv) in
    let inputFile = args.GetResults <@ Input @> in
    let timeLimit = args.TryGetResult <@ TimeLimit @> in
    let memoryLimit = args.TryGetResult <@ MemoryLimit @> in
    let pPhrases = args.GetResults <@ PowerPhrase @> in
    
    let inputData = inputFile |> Seq.map InputData.Load

    let mkSeedData input field seed  = seq_of_units input seed |> Seq.take input.SourceLength |> fun units -> (input.Id, seed, field, units) in
    let mkData i = let field = mkField i in
                   i.SourceSeeds |> Seq.map (mkSeedData i field)
    in

    let fields = inputData |> Seq.map mkData in
    fields

    (*
    
{p, ', !, ., 0, 3}	move W
{b, c, e, f, y, 2}	move E
{a, g, h, i, j, 4}	move SW
{l, m, n, o, space, 5}    	move SE
{d, q, r, v, z, 1}	rotate clockwise
{k, s, t, u, w, x}	rotate counter-clockwise
\t, \n, \r	(ignored)

    **)

let string_from_units moves = moves 
                                |> Seq.map ^<|^ function
                                    | MShift E  -> 'b'
                                    | MShift W  -> '!'
                                    | MShift SE -> '5'
                                    | MShift SW -> 'j'
                                    | MRotate CW -> 'd'
                                    | MRotate CCW -> 'k'
                                    | Finished -> '\n'
                                |> String.Concat

let solve field unit =
    let unit = unit_start unit field in
    let moves = best_move unit field in
    if(moves.IsEmpty) 
    then None
    else (moves |> Seq.map fst |> string_from_units, moves |> List.map snd |> List.last) |>  Some


let solveAll field units = 
    let mutable field' = field in
    let mutable str = "" in
    let mutable stop = false in
    for unit in units do
        if stop then ()
        else
                let s = solve field' unit in
                match s with 
                    | Some (pstr, unit') -> (str <- str ^ "\n" ^ pstr; field' <- apply_unit unit' field' |> Option.map handle_lock |> Option.get)
                    | None -> stop <- true
    done;
    str

let result id seed tag str = OutputData.Root(id, seed, tag, str)

let now = System.DateTime.Now

let mkResult (id, seed, field, units) = 
    let str = solveAll field units in
    let tag = "Stupid bot ^___^ 5 - " + (now.Hour.ToString()) + ":" + (now.Minute.ToString()) in
    result id seed tag str

[<EntryPoint>]
let main argv = 
    let tasks = doit argv in
    tasks |> Seq.concat |> Seq.map mkResult |> Seq.map (fmt "%A") |> String.concat "," |> write "[\n%s\n]"
    0 // return an integer exit code
