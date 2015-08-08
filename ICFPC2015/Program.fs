// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module DataPart

open System.Text
open FSharp.Data
open Microsoft.FSharp.Text
open Nessos.UnionArgParser

open Printf

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

type Unit = { pivot : Hex; filled : Hex list }

let readUnit (input: InputUnit) =
    let readHex (m: InputCell) = Hex.FromOffset (m.X,  m.Y) in
    { pivot = readHex input.Pivot;
      filled = input.Members |> Seq.map readHex |> Seq.toList }

let translate { x = dx; y = dy; z = dz } {x = x; y = y; z = z;} =
    { x = x + dx; y = y + dy; z = z + dz}

let ( |.~> ) = translate

let diff { x = sx; y = sy; z = sz } { x = dx; y = dy; z = dz } =
    {x = dx - sx; y = dy - sy; z = dz - sz }

let ( <~.~> ) = diff

let translateUnit dest {pivot = pivot; filled = filled} = 
    let trans = diff pivot dest in
    { pivot = dest; filled = filled |> List.map (translate trans)  } 

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

let rotateUnitOneTurn dir {pivot = pivot; filled = filled}  = 
    { pivot = pivot; filled = filled |> List.map (rotateHex dir pivot) }

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
    let unit_upper = unit.filled |> Seq.maxBy (fun h -> h.row) in
    let unit_upper_needed = Hex.FromOffset(unit_upper.col, 0) in
    let pivot = translate (diff unit_upper unit_upper_needed) unit.pivot in
    let unit = translateUnit pivot unit in

    let unit_leftmost = unit.filled |> Seq.minBy (fun h -> h.col) in
    let unit_rightmost = unit.filled |> Seq.maxBy (fun h -> h.col) in

    let unit_width = unit_rightmost.col - unit_leftmost.col in
    let field_middle = field.width / 2 in
    let unit_middle = (unit_leftmost.col + unit_rightmost.col)/2 in
    let vshift = Hex.FromOffset (field_middle - unit_middle, 0) |> trace in
    let pivot = translate vshift unit.pivot in

    translateUnit pivot unit

let flip f a b = f b a

let enumerate_all_positions field unit = 
    seq { for col in 0 .. field.width - 1 do 
            for row in 0 .. field.height - 1 do 
                for rot in 0..5 do
                    let target = Hex.FromOffset(col, row) in
                    yield (unit |> translateUnit target |> rotateUnit CW rot ) }
    |> Seq.map (flip apply_unit field) 
    |> Seq.filter Option.isSome
    |> Seq.map Option.get 

let sameType (a: 'a) (b: 'a) = a

let field_score field =
    let a = -40 in 
    let b = 100 in
    let c = -20 in
    let d = -10 in
    let howHigh (col: CellState EIntMap) = field.height - col.MinimumKey in
    let maxHeight = field.data
                    |> Seq.maxBy (fun kv -> kv.Value |> howHigh)
                    |> (fun kv -> kv.Value |> howHigh)
    in 
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
    let biggestHole = field.data 
                   |> Seq.map (fun column -> abs(howHigh (column.Value) - column.Value.Count)) 
                   |> Seq.max
    in
    printField field |> write "\n%s\n";
    (a * maxHeight + b * completeLines + c * bumpiness + d * biggestHole) |> trace

let ( |^ ) a b = fun x -> x |> a |> b

let local_random seed = 
    let a = 1103515245L in 
    let m = 1L <<< 32 in 
    let c = 12345L in
    (a * seed + c) % m

let random_seq seed = // WARNING: an infinite sequence :-)
    seq { let current = ref seed in while(true) do yield !current; current := local_random !current; done  }
    |> Seq.map (fun number -> let shifted = number >>> 16 in shifted &&& 0x7fffL)

let doit argv = 
    let args = parser.Parse(argv) in
    let inputFile = args.GetResults <@ Input @> in
    let timeLimit = args.TryGetResult <@ TimeLimit @> in
    let memoryLimit = args.TryGetResult <@ MemoryLimit @> in
    let pPhrases = args.GetResults <@ PowerPhrase @> in
    
    let inputData = inputFile |> Seq.map InputData.Load

    let fields = inputData |> Seq.map (fun i -> (mkField i, i.Units |> Seq.map readUnit)) in
    let positions = fields |> Seq.map (fun (field, units) -> units |> Seq.collect (enumerate_all_positions field) |> Seq.sortBy field_score |> fun f -> (Seq.last f, units)) in
    positions |> Seq.toList
            
[<EntryPoint>]
let main argv = 
    let _ = doit argv in
    0 // return an integer exit code
