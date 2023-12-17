open Base
open Stdio

let get_last_elem l =
  match List.last l with
    | Some x -> x 
    | None -> 0

let rec r l ~acc =
  match l with
    | [] -> List.fold acc ~init:0 ~f:(+)
    | _ :: tl -> 
      let new_acc = List.map2_exn (l |> List.rev |> List.tl_exn |> List.rev) tl ~f:(fun a b -> b - a) in
      if List.for_all new_acc ~f:(fun x -> x = 0) then
        (List.fold acc ~init:0 ~f:(+)) + get_last_elem l
      else
        r new_acc ~acc:(acc @ [get_last_elem l])


let solve l =
  l |> List.map ~f:(fun x -> String.split x ~on:' ' |> List.map ~f:Int.of_string) 
  |> List.map ~f:(fun x -> r x ~acc:[])
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string
  |> print_endline