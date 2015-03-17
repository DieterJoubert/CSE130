let rec merge l1 l2 = 
    match(l1, l2) with
    |([],l) -> l
    | (l,[]) -> l
    |(h1::t1, h2::t2) -> if(h1 >= h2) then h2::h1::merge t1 t2
                    else h1::h2::merge t1 t2

let split list n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i-1) (h :: acc) t  in
    aux n [] list;;