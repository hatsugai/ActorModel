open Actor

type msg = Request | Propose of int | Accept of int

let show_msg msg =
  match msg with
    Propose k -> Printf.sprintf "Propose.%d" k
  | Accept k -> Printf.sprintf "Accept.%d" k
  | Request -> Printf.sprintf "Request"

let acceptor () =
  let rec state_0 sender msg =
    match msg with
      Propose k ->
       (A (state_1 k), Printf.sprintf "Acc %d" k, [(sender, Accept k)])
    | _ -> error "acceptor"

  and state_1 k sender msg =
    match msg with
      Propose _ ->
       (A (state_1 k), Printf.sprintf "Acc %d" k, [(sender, Accept k)])
    | _ -> error "acceptor"

  in
  A state_0

let proposer k =
  let rec state_0 sender msg =
    match msg with
      Request -> (A state_1, "Q1", [(0, Propose k)])
    | _ -> error "proposer"

  and state_1 sender msg =
    match msg with
      Accept k -> (A (state_2 k), Printf.sprintf "Q2 %d" k, [])
    | _ -> error "proposer"

  and state_2 k sender msg =
    error "proposer"

  in
  A state_0

let state0 =
  ([(acceptor (), "P0");
    (proposer 1, "Q0");
    (proposer 2, "Q0")],
   [(0, 1, Request); (0, 2, Request)])

let cs = unfold state0

let () = viz "consensus" show_msg cs
