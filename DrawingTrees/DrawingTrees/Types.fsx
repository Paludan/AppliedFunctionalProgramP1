    open System.Windows.Forms.VisualStyles.VisualStyleElement.StartPanel
    open System.Runtime.InteropServices
    open System.Net.WebRequestMethods
    open System.Net.WebRequestMethods
    open System.IO
    open System.Runtime.InteropServices

namespace DrawingTrees
module Types =
    type 'a Tree = Node of 'a * ('a Tree list)
    type Extent = (float * float) list

    let movetree (Node((label, x), subtrees), x' : float) =
        Node((label, x+x'), subtrees)

    let moveextent (e : Extent, x) = List.map (fun (p,q) -> (p+x,q+x)) e

    let rec merge (ex : Extent * Extent) : Extent =
        match ex with
        | ([], qs) -> qs
        | (ps, []) -> ps
        | ((p,_)::ps, (_,q)::qs) -> (p,q) :: merge (ps, qs)

    let mergelist es = 
        List.fold (fun acc x ->
                merge (acc, x)
             ) [] es

    let rmax (p : float, q : float) = if p > q then p else q

    (* fit: Extent*Extent -> float  ) *)
    let rec fit e =
        match e with
        | ([], _) | (_, []) -> 0.0
        | ( ((_,p)::ps),((q,_)::qs) ) -> rmax(fit (ps,qs), p - q + 1.0)


    let fitlistl es =
        let rec fitlistl' es =
            match es with
            | (acc, []) -> []
            | (acc, (e::ex)) -> let x = (fit (acc, e)) 
                                x :: (fitlistl' ( (merge(acc,moveextent (e,x))),ex))
        fitlistl' ([], es)

    let fitlistr es =
        let rec fitlistr' es =
            match es with
            | (acc, []) -> []
            | (acc, (e::ex)) -> let x = -(fit (e, acc)) 
                                x :: (fitlistr' ( (merge(moveextent (e,x), acc)),ex))
        List.rev (fitlistr' ([], List.rev es))

    let mean (x,y) = (x+y) / 2.0

    let fitlist es = List.map mean (List.zip (fitlistl es) (fitlistr es))

    let design tree =
        let rec design' (Node(label, subtrees)) =
            let (trees, extents) = List.unzip ( List.map design' subtrees)
            let positions        = fitlist extents
            let ptrees           = List.map movetree (List.zip trees positions)
            let pextents         = List.map moveextent (List.zip extents positions)
            let resultextent     = (0.0, 0.0) :: mergelist pextents
            let resulttree       = Node((label, 0.0), ptrees)
            (resulttree, resultextent)
        fst (design' tree)

    let rec reflect (Node(v, subtrees)) =
        Node(v, List.map reflect (List.rev subtrees))

    let rec reflectpos (Node((v,x), subtrees)) =
        Node((v, -x), List.map reflectpos subtrees)

    //generate example of large tree
    let val1 =  Node("x",
    [Node("y",[Node("y1",[]);Node("y2",[]);Node("y3",[])]);
    Node("z",[]);Node("z",[]);Node("z",[]);
    Node("a",[Node("a1",[]);Node("a2",[]);Node("a3",[])])]);;

    let val2 =  Node("x", [Node("y",[]);Node("y",[]);Node("z",[])]);;

    let val3 = Node("b1",
    [Node("b2",[Node("b5",[]);Node("b6",[]);Node("b7",[])]);
    Node("b3",[]);Node("b9",[]);Node("b10",[val2;val2]);
    Node("b4",[Node("b11",[]);Node("b12",[]);Node("b13",[val2])])]);;

    let val4 = Node("c", [Node("c1",[val1;Node("!",[])]);Node("c2",[val3;Node("!",[])]);Node("c3",[val2;Node("!",[])])]);;

    let tree = design val4

    let rec drawTree (x:int,y:int) n path (Node((label,p:float), subtree)) =
        let write1=((string x)+" "+(string (y-10))+" moveto\n"+" ("+label+") dup stringwidth pop 2 div neg 0 rmoveto show\n")
        if (List.isEmpty subtree) then File.AppendAllText (path, write1 )
                                       "-"
        else    let write2=((string x)+" "+(string (y-20))+" moveto\n "+(string x)+" "+(string (y-40))+" lineto\n")
                let plist = List.map (fun (Node((l,p), nl)) -> int (p*(float)n)) subtree
                let startp = List.head plist
                let newplist = List.map (fun p -> (p+x)) plist
                let write3= ((string (x+startp))+" "+(string (y-40))+" moveto\n "+(string (x-startp))+" "+(string (y-40))+" lineto\n")
                let write4= List.fold (fun s p -> s+(string p)+" "+(string (y-40))+" moveto\n "+ (string p)+" "+(string (y-80))+" lineto\n") ""  newplist
                //File.WriteAllLines("C:\\Users\\s172016\\Downloads\\1.txt", [write1+write2])
                File.AppendAllText (path, (write1+write2+write3+write4) )
                //write1+write2+write3+write4 
                "!+"+drawsubTree (y-80) n path (newplist, subtree)
    and drawsubTree y n path (plist,subtree)=
            match (plist,subtree) with
            |(p::pt,r::rt) -> "*"+(string p)+" "+(string y)+"\n"+(drawTree (p,y) n path r)+ drawsubTree y n path (pt, rt)// drawsubTree y n (pt, rt)//string p
            |(_,_) -> "";;


    let drawTop (x,y) path= 
        let write =( "%!\n<</PageSize["+(string x)+" "+(string  y)+"]/ImagingBBox null>> setpagedevice\n1 1 scale\n"+
        (string (x/2))+" "+(string (y-1))+" translate\nnewpath\n/Times-Roman findfont 10 scalefont setfont\n")
        File.AppendAllText (path, write )
        " Have written the top "


    let drawBottom path=
        File.AppendAllText (path,"stroke\nshowpage\n")
        "Have written the bottom"

    let  drawPS (pagex,pagey) path zoomsize tree=
        (drawTop (pagex,pagey) path) |>ignore
        (drawTree (0,0) zoomsize path tree) |>ignore
        drawBottom path |>ignore
        "!!!"


    
    
    let path = "C:\\Users\\s172016\\Downloads\\10.ps"

    let dps1= drawPS (1400,1000) path 40 tree;;

   
    //let dt1=drawTop (1400,1000) path
     
    //let db1= drawBottom path

    //let test = drawTree (0,0) 40 path tree;;

    //printfn "%s" test


