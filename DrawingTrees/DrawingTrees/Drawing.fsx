namespace DrawingTrees

#load "Types.fsx"
open DrawingTrees.Types

module Drawing =
    let rec drawTree (x:int,y:int) n path (Node((label,p:float), subtree)) =
        let write1 = ((string x) + " " + (string (y - 10)) + " moveto\n" + 
                        " (" + label + ") dup stringwidth pop 2 div neg 0 rmoveto show\n")
        if (List.isEmpty subtree) then System.IO.File.AppendAllText (path, write1 )
                                       "-"
        else    let write2      = ((string x) + " " + (string (y - 20)) + " moveto\n " + 
                                    (string x) + " " + (string (y - 40)) + " lineto\n")
                let plist       = List.map (fun (Node((l,p), nl)) -> int (p * float n)) subtree
                let startp      = List.head plist
                let newplist    = List.map (fun p -> (p+x)) plist
                let write3      = ((string (x + startp)) + " " + (string (y - 40)) + " moveto\n " + 
                                    (string (x - startp)) + " " + (string (y - 40)) + " lineto\n")
                let write4      = List.fold (fun s p -> 
                                    s + (string p) + " " + (string (y - 40)) + " moveto\n " + 
                                    (string p) + " " + (string (y - 80)) + " lineto\n") ""  newplist

                System.IO.File.AppendAllText (path, (write1 + write2 + write3 + write4) )
                "!+" + drawsubTree (y - 80) n path (newplist, subtree)

    and drawsubTree y n path (plist,subtree)=
            match (plist,subtree) with
            |(p::pt,r::rt) -> "*" + (string p) + " " + (string y) + "\n" + (drawTree (p,y) n path r) + drawsubTree y n path (pt, rt)
            |(_,_) -> ""

    let drawTop (x,y) path = 
        let write = ( "%!\n<</PageSize[" + (string x) + " " + (string  y) + 
                        "]/ImagingBBox null>> setpagedevice\n1 1 scale\n" +
                        (string (x/2)) + " " + (string (y-1)) + " translate\nnewpath\n/Times-Roman findfont 10 scalefont setfont\n")
        System.IO.File.AppendAllText (path, write )

    let drawBottom path =
        System.IO.File.AppendAllText (path,"stroke\nshowpage\n")

    let  drawPS (pagex,pagey) path zoomsize tree=
        (drawTop (pagex,pagey) path) |>ignore
        (drawTree (0,0) zoomsize path tree) |>ignore
        drawBottom path |>ignore