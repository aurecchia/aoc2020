
let parseInput (s: string) =
    s.ToCharArray () |> List.ofArray |> List.splitAt 7

let bisect pred (min, max) i =
    let diff = (max - min + 1) / 2
    if pred i then (min, max - diff) else (min + diff, max)

let getRow = (Seq.fold (bisect (fun i -> i = 'F')) (0, 127)) >> fst
let getCol = (Seq.fold (bisect (fun i -> i = 'L')) (0, 7)) >> fst

let getSeat (rowSplits, colSplits) = (getRow rowSplits, getCol colSplits)
let seatId (row, col) = row * 8 + col

let bookedSeats lines =
    lines
    |> Seq.map parseInput
    |> Seq.map getSeat
    |> Seq.map seatId


[<EntryPoint>]
let main _ =
    let readLines path = System.IO.File.ReadLines path

    let bookedSeats = readLines "input" |> bookedSeats |> Seq.sortDescending |> List.ofSeq

    bookedSeats
    |> List.head
    |> printfn "The largest seat id is: %A"

    bookedSeats
    |> List.windowed 2
    |> List.pick (function
                  | a :: b :: [] when a = b + 2 -> Some (b + 1)
                  | _ -> None)
    |> printfn "%A"

    0 // return an integer exit code