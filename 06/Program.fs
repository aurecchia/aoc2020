
open System

let parse op init lines =
    let folder (groups: 'a list) (line: string) =
        if String.IsNullOrWhiteSpace(line) then
            init :: groups
        else
            let answers = line.ToCharArray () |> Set.ofArray
            let current :: rest = groups

            (op answers current) :: rest

    lines
    |> Seq.fold folder [ init ]

[<EntryPoint>]
let main _ =
    let readLines path = System.IO.File.ReadLines path

    readLines "input"
    |> parse Set.union Set.empty
    |> List.sumBy (fun g -> g.Count)
    |> printfn "%A"

    readLines "input"
    |> parse (fun e l -> e :: l) []
    |> List.map Set.intersectMany
    |> List.sumBy (fun g -> g.Count)
    |> printfn "%A"

    0