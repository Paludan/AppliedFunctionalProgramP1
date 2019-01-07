#load "Types.fsx"

open DrawingTrees.Types

open System

[<EntryPoint>]
let main argv =
    let a = [[(1.0, 2.0); (3.0, 4.0)]; [(5.0, 6.0); (7.0, 8.0)];
        [(9.0, 10.0); (11.0, 12.0); (13.0, 14.0)]]
    let var = fitlistl a
    printfn "Hello World from F#!"
    0 // return an integer exit code
