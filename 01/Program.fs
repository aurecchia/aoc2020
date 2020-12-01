// --- Day 1: Report Repair ---
//
// After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island. Surely, Christmas will go on without you.
//
// The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.
//
// To save your vacation, you need to get all fifty stars by December 25th.
//
// Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
//
// Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.
//
// Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
//
// For example, suppose your expense report contained the following:
//
// 1721
// 979
// 366
// 299
// 675
// 1456
//
// In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.
//
// Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?

open System
open System.IO

let readLines (path: string) = seq {
    use reader = new StreamReader (path)
    while not reader.EndOfStream do
        yield reader.ReadLine ()
}

let combinations items = seq {
    for i in items do
        for j in items do
            yield (i, j)
}

let firstPart () =
    readLines "input"
    |> Seq.map Int32.Parse
    |> combinations
    |> Seq.find (fun (a, b) -> a + b = 2020)
    |> (fun (a, b) -> a * b)
    |> printfn "First part: %A"

let combinations2 items = seq {
    for i in items do
        for j in items do
            for k in items do
                yield (i, j, k)
}

let secondPart () =
    readLines "input"
    |> Seq.map Int32.Parse
    |> combinations2
    |> Seq.find (fun (a, b, c) -> a + b + c = 2020)
    |> (fun (a, b, c) -> a * b * c)
    |> printfn "Second part: %A"

// From https://stackoverflow.com/a/4495708
let rec combinations3 acc size set = seq {
  match size, set with
  | n, x::xs ->
      if n > 0 then yield! combinations3 (x::acc) (n - 1) xs
      if n >= 0 then yield! combinations3 acc n xs
  | 0, [] -> yield acc
  | _, [] -> () }

let bothPartsAndBetter n =
    readLines "input"
    |> Seq.map Int32.Parse
    |> List.ofSeq
    |> combinations3 [] n
    |> Seq.find (fun items -> List.sum items = 2020)
    |> List.fold (*) 1
    |> printfn "With %d items: %A" n

let withTime f =
     let startTime = DateTimeOffset.UtcNow
     f()
     let endTime = DateTimeOffset.UtcNow
     let time = endTime - startTime
     printfn "Took %d seconds" time.Seconds

[<EntryPoint>]
let main argv =
    withTime (firstPart)
    withTime (secondPart)
    printfn "================"
    withTime (fun () -> bothPartsAndBetter 2)
    withTime (fun () -> bothPartsAndBetter 3)

    0