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

    // Michael R. Hansen 06-01-2018
    // This file is obtained by an adaption of the file MicroC/Absyn.fs by Peter Sestoft
    //

    type Exp =
             | N  of int                   (* Integer constant            *)
             | B of bool                   (* Boolean constant            *)
             | Access of Access            (* x    or  ^p    or  a[e]     *)
             | Addr of Access              (* &x   or  &p^   or  &a[e]    *)
             | Apply of string * Exp list  (* Function application        *)

    and Access =
              | AVar of string             (* Variable access        x    *) 
              | AIndex of Access * Exp     (* Array indexing         a[e] *)
              | ADeref of Exp              (* Pointer dereferencing  p^   *)

    type Stm  =
              | PrintLn of Exp               (* Print                          *) 
              | Ass of Access * Exp          (* x:=e  or  p^:=e  or  a[e]:=e   *)
              | Return of Exp option         (* Return from function           *)   
              | Alt of GuardedCommand        (* Alternative statement          *) 
              | Do of GuardedCommand         (* Repetition statement           *) 
              | Block of Dec list * Stm list (* Block: grouping and scope      *)
              | Call of string * Exp list    (* Procedure call                 *)
               
    and GuardedCommand = GC of (Exp * Stm list) list (* Guarded commands    *)

    and Dec = 
             | VarDec of Typ * string        (* Variable declaration               *)
             | FunDec of Typ option * string * Dec list * Stm
                                             (* Function and procedure declaration *) 

    and Typ  = 
             | ITyp                          (* Type int                    *)
             | BTyp                          (* Type bool                   *)
             | ATyp of Typ * int option      (* Type array                  *)
             | PTyp of Typ                   (* Type pointer                *)
             | FTyp of Typ list * Typ option (* Type function and procedure *)

    type Program = P of Dec list * Stm list   (* Program                 *)
   
    let rec ExpHandler exp = 
        match exp with 
        | N i -> Node(i.ToString(),[])
        | B b -> Node(b.ToString(),[])
        | Access a -> Node("ContOf",[AccessHandler a])
        | Addr a -> Node("Addr",[AccessHandler a])
        | Apply (str, el) -> Node(str,List.map ExpHandler el)

    and AccessHandler a =
        match a with 
        | AVar str -> Node("Var\n\""+str+"\"",[])
        | AIndex (a, e) -> Node("AIndex",[AccessHandler a; ExpHandler e])
        | ADeref e -> Node("ADeref",[ExpHandler e])

    let rec StmHandler stm =
        match stm with
        | PrintLn e -> Node("Print\nLn",[ExpHandler e])
        | Ass (a,e) -> Node("Ass",[AccessHandler a; ExpHandler e])
        | Return (Some(e)) -> Node("Return",[ExpHandler e])
        | Return None -> Node("Return",[])
        | Alt gc -> Node("Alt",GCHandler gc)
        | Do gc -> Node("While",GCHandler gc)
        | Block (dl, sl) -> Node("Block", List.append (List.map DecHandler dl) [Node("Seq",List.map StmHandler sl)])
        | Call (str, el) -> Node(str, List.map ExpHandler el)


    and GCHandler gclist = 
        match gclist with
        | GC gclist -> List.fold(fun acc (e, sl) -> 
                       List.append [ExpHandler e] [Node("Seq",List.map StmHandler sl)]
                       ) [] gclist

    and DecHandler d =
        match d with
        | VarDec (t,str) -> Node("VarDec",[Node(str, []); TypHandler t])
        | FunDec (Some(t), str, dl, s) -> Node("FunDec", [])
        | FunDec (None, _, _, _) -> Node("No Function", [])

    and TypHandler (t) = 
        match t with
        | ITyp -> Node("IntTyp",[])
        | BTyp -> Node("BoolTyp",[])
        | ATyp (t,i) -> Node("ArrTyp",[TypHandler t; Node(i.ToString(),[])]) 
        | PTyp t -> Node("PointTyp",[TypHandler t])
        | FTyp (tl, Some(t)) -> Node("TypFunc",List.append (List.map TypHandler tl) [TypHandler t])     
        | FTyp (tl, None) -> Node("TypFunc", List.map TypHandler tl)

    let ProgramConverter (p:Program) =
        match p with
        | P (dl, sl) -> Node("Program",List.append (List.map DecHandler dl) (List.map StmHandler sl))
