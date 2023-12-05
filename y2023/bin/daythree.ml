let is_digit c = c >= '0' && c <= '9'
let is_dot c = c = '.'
let is_symbol c = c |> is_digit |> not && c |> is_dot |> not

let positions_to_check (row, col) =
  [
    (row, col + 1); 
    (row, col - 1); 
    (row - 1, col); 
    (row - 1, col + 1);
    (row - 1, col - 1);
    (row + 1, col);
    (row + 1, col + 1);
    (row + 1, col - 1)
  ] |> List.filter(fun (r, c) -> r >= 0 && c >= 0 && r < 140 && c < 140)

let has_symbol_adjacent pos matrix =
  pos |> positions_to_check |> List.map(fun (r, c) -> matrix.(r).(c) |> is_symbol) |> List.filter(fun x -> x) |> List.length > 0

let find_forward (row, col) matrix =
  let rec aux col =
    if col = 140 || matrix.(row).(col) |> is_digit |> not then (row, col - 1)
    else aux (col + 1) 
  in aux col 

let find_backward (row, col) matrix =
  let rec aux col =
    if col < 0 || matrix.(row).(col) |> is_digit |> not then (row, col)
    else aux (col - 1) 
  in aux col

let range a b = 
  let rec aux a b l = 
    if a = b then l
    else aux a (b - 1) (b :: l)
  in aux a b []

let find_number (row, col) matrix =
  let (_, c1) = find_backward (row, col) matrix in
  let (_, c2) = find_forward (row, col) matrix in
  range c1 c2 |> List.map(fun c -> matrix.(row).(c)) |> List.to_seq |> String.of_seq

let solve l =
  let ref_list = ref [] in
  let t = l |> List.map(fun s -> s |> String.to_seq |> List.of_seq ) in
  let m = Array.make_matrix (List.length t) (List.length (List.hd t)) ' ' in
  t |> List.iteri (fun i l -> 
    l |> List.iteri (fun j c -> 
      m.(i).(j) <- c));
  let rec find_numbers row col =
    if row > 139 then ()
    else if col >= 139 then find_numbers (row + 1) 0
    else if m.(row).(col) |> is_digit && has_symbol_adjacent (row, col) m then (
      let (r, c) = find_forward (row, col) m in
      let number = find_number (row, col) m in
      ref_list := number :: !ref_list;
      find_numbers r (c + 1))
    else find_numbers row (col + 1) in
  find_numbers 0 0;
  !ref_list |> List.map int_of_string |> List.fold_left (+) 0 |> string_of_int |> print_endline