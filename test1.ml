open Actor

type msg = M of int

let show_msg (M k) = Printf.sprintf "M.%d" k

let actor_P () =
  let rec state_0 sender msg =
    (A state_1, "P1", [(1, M 13); (2, M 23); (2, M 37)])
  and state_1 sender msg =
    (A state_Z, "PZ", [])
  and state_Z sender msg =
    error "actor_P state_Z"
  in
  A state_0

let actor_Q () =
  let rec state_0 j sender (M k) =
    (A (state_0 k), Printf.sprintf "Q %d" k, [])
  in
  A (state_0 0)

let state0 =
  ([(actor_P (), "P0");
    (actor_Q (), "Q0");
    (actor_Q (), "Q0")],
   [(0, 0, M 0)])

let cs = unfold state0

let () = viz "test1" show_msg cs
