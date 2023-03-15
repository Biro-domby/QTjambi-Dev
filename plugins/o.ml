(* Génération d'un puzzle aléatoire de taille n x n avec k couleurs *)
let generate_puzzle n k =
  let puzzle = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      puzzle.(i).(j) <- Random.int k
    done
  done;
  puzzle

(* Vérification si un puzzle est résolu *)
let is_solved puzzle =
  let n = Array.length puzzle in
  let rec check i j =
    if i = n then true
    else if j = n then check (i + 1) 0
    else if puzzle.(i).(j) = 0 then false
    else
      let color = puzzle.(i).(j) in
      let rec check_row x =
        x < n && (puzzle.(i).(x) = color || check_row (x + 1))
      in
      let rec check_column x =
        x < n && (puzzle.(x).(j) = color || check_column (x + 1))
      in
      check_row j && check_column i && check i (j + 1)
  in
  check 0 0

(* Résolution d'un puzzle avec la méthode de backtrack *)
let solve puzzle =
  let n = Array.length puzzle in
  let rec solve i j =
    if i = n then true
    else if j = n then solve (i + 1) 0
    else if puzzle.(i).(j) <> 0 then solve i (j + 1)
    else
      let rec try_color c =
        if c = n then false
        else begin
          puzzle.(i).(j) <- c;
          if solve i (j + 1) then true
          else begin
            puzzle.(i).(j) <- 0;
            try_color (c + 1)
          end
        end
      in
      try_color 1
  in
  solve 0 0

(* Affichage du puzzle *)
let print_puzzle puzzle =
  let n = Array.length puzzle in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      print_int puzzle.(i).(j);
      if (j + 1) mod 3 = 0 && j <> n - 1 then
        print_string " | "
      else
        print_string " ";
    done;
    if (i + 1) mod 3 = 0 && i <> n - 1 then
      print_endline "\n-----------------------------"
    else
      print_newline ();
  done

(* Programme principal *)
let () =
  let n = 12 in
  let k = 10 in
  let puzzle = generate_puzzle n k in
  print_endline "Puzzle généré :";
  print_puzzle puzzle;
  if solve puzzle then begin
    print_endline "Puzzle résolu :";
    print_puzzle puzzle
  end
  else
    print_endline "Impossible de résoudre le puzzle."