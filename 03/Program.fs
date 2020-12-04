let readLines path = System.IO.File.ReadLines path

let rec treesHit x (right, down) (lines: string list) =
    if down >= lines.Length then
        0
    else
        let remainingLines = List.skip down lines
        let line = remainingLines.Head
        let newX = (x + right) % line.Length
        let add = if line.[newX] = '#' then 1 else 0

        add + treesHit newX (right, down) remainingLines


[<EntryPoint>]
let main argv =
    let lines = readLines "input" |> List.ofSeq

    lines
    |> treesHit 0 (3, 1)
    |> printfn "I hit %d trees in the first part!"

    let moves = [
       (1, 1)
       (3, 1)
       (5, 1)
       (7, 1)
       (1, 2)
    ]

    moves
    |> List.map (fun move -> treesHit 0 move lines)
    |> List.reduce (*)
    |> printfn "I hit %d trees in the second part!"

    0