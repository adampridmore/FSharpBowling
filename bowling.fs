module Bowling

open Xunit

let score bowls = 
    let rec scoreFrames bowls frameIndex =
        let isStrike a = a = 10
        let isSpare (a,b) = a + b = 10
        match frameIndex, bowls with
        | _ , [] -> 0
        | frameIndex, _ when frameIndex > 9 -> 0
        | _ , a::(b::c::_ as rest) when a |> isStrike -> 10 + b + c + scoreFrames rest (frameIndex + 1)
        | _ , a::b::(c::_ as rest) when (a,b) |> isSpare -> 10 + c + scoreFrames rest (frameIndex + 1)
        | _ , a::b::rest -> a + b + scoreFrames rest (frameIndex + 1)
        | _ -> failwith (sprintf "Invalid bowls: %A" bowls)

    scoreFrames bowls 0
       
[<Fact>]
let ``Simple bowl`` () =
    Assert.Equal(2, [1;1] |> score)

[<Fact>]
let ``Spare bowl`` () =
    Assert.Equal(14, [5;5;1;2] |> score)

[<Fact>]
let ``Strike bowl`` () =
    Assert.Equal(16, [10;1;2] |> score)       

[<Fact>]
let ``End game spare`` () =
    let bowls = List.concat [(Array.create 18 0) |> Array.toList ;[5;5;1] ]
    Assert.Equal(11, bowls |> score)

[<Fact>]
let ``End game strike`` () =
    let bowls = List.concat [(Array.create 18 0) |> Array.toList ;[10;1;2] ]
    Assert.Equal(13, bowls |> score)

[<Fact>]
let ``Perfect Game`` () =
    let bowls = (Array.create 12 10) |> Array.toList
    Assert.Equal(300, bowls |> score)