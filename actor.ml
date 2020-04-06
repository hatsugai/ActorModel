open Printf

exception Error of string

type aid = int
type 'm actor = A of (aid -> 'm -> 'm actor * string * (aid * 'm) list)

let error s = raise (Error s)

let step (actor_list, msg_pool) =
  let rec scan_actor k ys xs trans =
    match xs with
      [] -> trans
    | (A actor, s)::xs' ->
       let rec scan_msg_pool vs us trans =
         match us with
           [] -> scan_actor (k+1) ((A actor, s)::ys) xs' trans
         | (sid, rid, msg)::us' ->
            let trans' =
              if rid = k then
                let (A actor', s', send) = actor sid msg in
                let actor_list' = List.rev_append ys ((A actor', s')::xs') in
                let send_tag =
                  List.map (fun (aid, msg) -> (k, aid, msg)) send
                in
                let msg_pool' = vs @ us' @ send_tag in
                ((rid, msg), (actor_list', msg_pool'))::trans
              else
                trans
            in
            scan_msg_pool ((sid, rid, msg)::vs) us' trans'
       in
       scan_msg_pool [] msg_pool trans
  in
  scan_actor 0 [] actor_list []

let bfs s0 next =
  let que = Queue.create () in
  let rec loop next_id cs =
    if Queue.is_empty que then
      cs
    else
      let (state, id) = Queue.take que in
      let trans =
        List.mapi
          (fun i (label, target) ->
            Queue.add (target, next_id + i) que;
            (label, target, next_id + i))
          (next state)
      in
      loop (next_id + List.length trans) ((state, id, trans)::cs)
  in
  Queue.add (s0, 0) que;
  loop 1 []

let unfold s0 = bfs s0 step

let viz name show_msg cs =
  let print_state ch (actor_list, msg_pool) =
    List.iter
      (fun (_, s) -> Printf.fprintf ch "%s " s)
    actor_list;
    Printf.fprintf ch "\\n";
    List.iter
      (fun (sid, rid, msg) ->
        Printf.fprintf ch "%d -> %d: %s\\n" sid rid (show_msg msg))
      msg_pool
  in
  let emit_states ch =
    List.iter
      (fun (state, id, trans) ->
        Printf.fprintf ch "%d [label=\"%d\\n" id id;
        print_state ch state;
        Printf.fprintf ch "\"];\n")
      cs
  in
  let emit_transitions ch =
    let emit_tr sid ((aid, msg), target, tid) =
      Printf.fprintf ch "%d -> %d [label=\"%d %s\"];\n"
        sid tid aid (show_msg msg);
    in
    List.iter
      (fun (state, id, trans) ->
        List.iter (emit_tr id) trans)
      cs
  in
  let ch = open_out (name ^ ".dot") in
  fprintf ch "digraph {\n";
  emit_states ch;
  emit_transitions ch;
  fprintf ch "}\n";
  close_out ch;
  let command = sprintf "dot -Tpdf -o %s.pdf %s.dot" name name in
  let _ = Unix.system command in
  ()
