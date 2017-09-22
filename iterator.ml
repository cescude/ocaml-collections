module Iterator = struct
  type 'a t = Gen of (unit -> 'a option)

  let make fn = Gen fn

  let empty = make (fun () -> None)

  let next = function
    | Gen fn -> fn ()

  let append r s = make (fun () ->
      match (next r) with
      | None -> next s  (* next r is empty, so go with next s *)
      | other -> other) (* next r produced a value, so return it *)

  let rec force s = match next s with
    | Some _ -> force s
    | None -> ()
  
  let continually fn =
    make (fun () -> Some (fn ()))

  let constantly value =
    make (fun () -> Some value)
  
  let from n =
    let idx = ref (n-1) in
    make (fun () -> idx := !idx+1; Some !idx)

  let range m n =
    let idx = ref m in
    make (fun () ->
        let current = !idx in
        match current < n with
        | true  -> (idx := !idx+1; Some current)
        | false -> None)

  let upto n = range 0 n

  let of_list lst =
    let head = ref lst in
    make (fun () ->
        match !head with
        | value :: tl -> head := tl; Some value
        | [] -> None)

  let rec to_list s =
    let rec forcer acc = match next s with
      | Some v -> forcer (v :: acc)
      | None -> List.rev acc
    in
    forcer []

  let rec fold a fn s = 
    match next s with
    | Some v -> fold (fn a v) fn s
    | None -> a

  let map fn s = make (fun () ->
      match next s with
      | Some value -> Some (fn value)
      | None -> None)

  let each fn s =
    map (fun v -> (fn v); v) s

  let zip r s = make (fun () ->
      match (next r, next s) with
      | (Some r', Some s') -> Some (r', s')
      | _ -> None)

  let zip_with_index s = zip s (from 0)
  
  let filter fn s =
    let rec driver () = match next s with
      | Some value when fn value -> Some value
      | Some value -> driver ()
      | None -> None
    in
    make driver

  let rec drop count s = if count <= 0
    then s
    else (next s; drop (count - 1) s)

  let take count s =
    let taken = ref 0
    in
    make (fun () ->
        match !taken < count with
        | true -> taken := !taken + 1; next s
        | false -> None
      )

  let drop_while fn s =
    let dropping = ref true in
    let rec dropper () =
      match (!dropping, next s) with
      | (true, Some v) when fn v -> dropper ()
      | (true, Some v) -> dropping := false; Some v
      | (_, otherwise) -> otherwise
    in
    make dropper

  let take_while fn s =
    let taking = ref true in
    let rec taker () =
      if !taking then match next s with
        | Some v when fn v -> Some v
        | Some v -> taking := false; taker ()
        | None -> None
      else None
    in
    make taker
end
