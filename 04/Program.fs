open System
open System.Text.RegularExpressions

type Passport = Set<(string * string)>

let parsePassport lines =
    let splitTuple2 (c: char) (s: string) =
        let parts = s.Split(c, 2)
        (parts.[0], parts.[1])

    let folder (current: Passport, passports: Passport list) (line: string) =
        if String.IsNullOrWhiteSpace(line) then
            (Set.empty, current :: passports)
        else
            let keys = line.Split(' ')
                       |> Seq.map (splitTuple2 ':')

            (current + Set(keys), passports)

    lines
    |> Seq.fold folder (Set.empty, List.empty)
    |> (fun (l, r) -> if not (Set.isEmpty l) then l :: r else r)

let simpleValidator (p: Passport) =
    let required = Set([ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ])
    Set.isSubset required (Set.map fst p)

let rulesValidator (p: Passport) =
    let matchingPred pattern predicate value =
        let m = Regex.Match(value, $"^{pattern}$")
        m.Success && predicate (List.tail [ for g in m.Groups -> g.Value ])

    let matching pattern = matchingPred pattern (fun _ -> true)
    let between min max = List.forall (fun i -> i |> Int32.Parse |> (fun x -> x >= min && x <= max))

    let year = matchingPred "(\d{4})"
    let height unit = matchingPred ("(\d+)" + unit)

    let requirements = [
        ("byr", year (between 1920 2002))
        ("iyr", year (between 2010 2020))
        ("eyr", year (between 2020 2030))
        ("hgt", (fun i -> (height "cm" (between 150 193) i) ||
                          (height "in" (between 59 76) i)))
        ("hcl", matching "#[0-9a-f]{6}")
        ("ecl", matching "amb|blu|brn|gry|grn|hzl|oth")
        ("pid", matching "[0-9]{9}")
    ]

    requirements
    |> List.forall (fun (key, check) ->
        p |> Set.exists (fun (pKey, value) ->
            (key = pKey) && check (value)))

[<EntryPoint>]
let main _ =
    let readLines path = System.IO.File.ReadLines path

    let pipeline isValid lines =
        lines
        |> parsePassport
        |> Seq.where isValid
        |> Seq.length

    readLines "input"
    |> pipeline simpleValidator
    |> printfn "First: There are %d valid passports"

    readLines "input"
    |> pipeline rulesValidator
    |> printfn "Second: There are %d valid passports"

    0 // return an integer exit code