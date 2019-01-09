Open the DrawingTrees.fsproj file in your IDE.
Compiler and run the program, the PostScript file will be located
in the solution folder.


If something goes wrong for what ever reason:
1. Load the Types.fsx file in FSI (without namespace/module)
2. Load the Drawing.fsx file (without namespace/module)
3. Load the following in FSI:
        let ast = 
            P ([],
                [Block
                ([VarDec (ATyp (ITyp,Some 4),"n"); VarDec (ATyp (ITyp,Some 1),"y")],
                    [Do
                    (GC 
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
4. Write the following in FSI: drawPS (2000,2000) "C:\\Your\\path\\file.ps" 40 (design (ProgramConverter ast))