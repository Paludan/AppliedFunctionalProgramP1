#load "Types.fsx"
#load "Drawing.fsx"

open DrawingTrees.Types
open DrawingTrees.Drawing

open System

[<EntryPoint>]
let main argv =

    let ast = 
        P ([],
            [Block
               ([VarDec (ATyp (ITyp,Some 4),"n"); VarDec (ATyp (ITyp,Some 1),"y")],
                [Do
                   (DrawingTrees.Types.GC 
                      [(Apply ("ApplyPrim",[Apply ("<>",[]); Access (AVar "n"); N 0]),
                        [PrintLn
                           (Apply
                              ("ApplyPrim",[Apply ("toString",[]); Access (AVar "n")]));
                         PrintLn
                           (Apply
                              ("ApplyPrim",[Access (AVar "y"); Apply ("toString",[])]));
                         Ass
                           (AVar "y",
                            Apply
                              ("ApplyPrim",
                               [Apply ("*",[]); Access (AVar "n"); Access (AVar "y")]));
                         Ass
                           (AVar "n",
                            Apply ("ApplyPrim",[Apply ("-",[]); Access (AVar "n"); N 1]))])]);
                 PrintLn (Apply ("ApplyPrim",[Apply ("toString",[]); Access (AVar "n")]));
                 PrintLn (Apply ("ApplyPrim",[Apply ("toString",[]); Access (AVar "y")]))])])

    let path = String.concat "" 
                [System.IO.Directory.GetCurrentDirectory() + string System.IO.Path.DirectorySeparatorChar;
                string System.DateTime.UtcNow.Day;
                string System.DateTime.UtcNow.Month;
                string System.DateTime.UtcNow.Year;
                string System.DateTime.UtcNow.Hour;
                string System.DateTime.UtcNow.Minute;
                string System.DateTime.UtcNow.Second;
                ".ps"]

    let dps1= drawPS (2000,2000) path 40 (design (ProgramConverter ast))

    printf "Path to image: %A" path
    0 // return an integer exit code
