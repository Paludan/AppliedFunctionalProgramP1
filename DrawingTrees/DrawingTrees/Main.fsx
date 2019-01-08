﻿#load "Types.fsx"
#load "Drawing.fsx"

open DrawingTrees.Types
open DrawingTrees.Drawing

open System

[<EntryPoint>]
let main argv =
    let val1 =  Node("x",[Node("y",[Node("y1",[]);Node("y2",[]);Node("y3",[])]);Node("z",[]);Node("z",[]);Node("z",[]);Node("a",[Node("a1",[]);Node("a2",[]);Node("a3",[])])])

    let tree = design val1

    let path = String.concat "" 
                [System.IO.Directory.GetCurrentDirectory() + string System.IO.Path.DirectorySeparatorChar;
                string System.DateTime.UtcNow.Day;
                string System.DateTime.UtcNow.Month;
                string System.DateTime.UtcNow.Year;
                string System.DateTime.UtcNow.Hour;
                string System.DateTime.UtcNow.Minute;
                string System.DateTime.UtcNow.Second;
                ".ps"]

    let dps1= drawPS (1400,1000) path 40 tree

    printf "Path to image: %A" path
    0 // return an integer exit code
