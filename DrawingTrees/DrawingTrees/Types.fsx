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