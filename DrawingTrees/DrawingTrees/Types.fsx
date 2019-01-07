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
        let rec fitlistl' es =
            match es with
            | (acc, []) -> []
            | (acc, (e::ex)) -> let x = (fit (acc, e)) 
                                x :: (fitlistl' ( (merge(acc,moveextent (e,x))),ex))
        fitlistl' (es, [])


    let mean (x,y) = (x+y) / 2.0

    let fitlist es = List.map map mean (zip (fitlistl es, fitlistr es))