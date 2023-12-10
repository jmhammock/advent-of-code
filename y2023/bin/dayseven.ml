open Base
open Stdio

let parse input =
  input |> String.split ~on:' ' |> (fun x -> (List.nth_exn x 0, List.nth_exn x 1))

let group hand ~init =
  let rec group' hand acc =
    match hand with
    | [] -> acc
    | hd :: tl -> 
      let to_process = List.filter tl ~f:(fun x -> Char.equal hd x |> not) in
      group' to_process ((List.filter hand ~f:(fun x -> Char.equal x hd) |> List.length) :: acc)
  in group' hand init

type hand = {
  hand: string;
  bid: int;
  rank: int;
}

let rec find_index l x i =
  match l with
  | [] -> raise (Invalid_argument "find_index")
  | hd :: tl -> if Char.equal hd x then i else find_index tl x (i + 1)


let sort hands =
  hands |> List.sort ~compare:(fun x y -> 
    if x.rank > y.rank then -1 
    else if x.rank < y.rank then 1
    else match List.zip (String.to_list x.hand) (String.to_list y.hand) with
      | Ok z -> 
        let rec compare' z =
          match z with
          | [] -> 0
          | (a, b) :: tl -> 
            if Char.equal a b then compare' tl
            else if 
              let cards = ['A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'] in
              (find_index cards a 0) < (find_index cards b 0) then -1
            else 1
        in compare' z
      | Unequal_lengths -> assert false)

let hand_type (hand, bid) =
  let hand' = hand |> String.to_list in
  match hand' |> group ~init:[] |> List.filter ~f:(fun x -> x > 1) with
  | [] -> {hand; bid = Int.of_string bid; rank = 1}
  | [2] -> {hand; bid = Int.of_string bid; rank = 2} 
  | [2; 2] -> {hand; bid = Int.of_string bid; rank = 3} 
  | [3] -> {hand; bid = Int.of_string bid; rank = 4}
  | [2; 3] -> {hand; bid = Int.of_string bid; rank = 5} 
  | [3; 2] -> {hand; bid = Int.of_string bid; rank = 5}
  | [4] -> {hand; bid = Int.of_string bid; rank = 6} 
  | [5] -> {hand; bid = Int.of_string bid; rank = 7} 
  | _ -> raise (Invalid_argument hand)
  
let solve l =
  try
    l |> List.map ~f:parse 
      |> List.map ~f:hand_type 
      |> sort 
      |> List.rev 
      |> List.mapi ~f:(fun i x -> x.bid * (i + 1))
      |> List.fold ~init:0 ~f:(+)
      |> printf "%d\n"
  with Invalid_argument x -> printf "Invalid argument: %s\n" x
