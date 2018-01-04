module Bowling

open System
open Xunit

let rec score bowls = 
    match bowls with
    | [] -> 0
    | a::b::c::rest when a = 10 -> a + b + c + score (b::c::rest) // Strike
    | a::b::c::rest when a + b = 10 -> a + b + c + score (c::rest) // Spare
    | a::b::rest -> a + b + (score rest)
    | [a] -> a
    | _ -> failwith (sprintf "Invalid bowls: %A" bowls)
    
[<Fact>]
let ``Single simple bowl`` () =
    Assert.Equal(1, [1] |> score)

[<Fact>]
let ``Spare bowl`` () =
    Assert.Equal(12, [5;5;1] |> score)       

[<Fact>]
let ``Strike bowl`` () =
    Assert.Equal(16, [10;1;2] |> score)       
