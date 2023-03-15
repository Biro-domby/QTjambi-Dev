(* Génération d'un puzzle aléatoire de taille n x n avec k couleurs *)
let generate_puzzle n k =
  let colors = [| 'B'; 'V'; 'R'; 'J'; 'N'; 'O'; 'G'; 'M'; 'C'; 'o' |] in
  let puzzle = Array.make_matrix n n ' ' in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      puzzle.(i).(j) <- colors.(Random.int k)
    done
  done;
  puzzle

(* Vérification si un puzzle est résolu *)
let is_solved puzzle =
  let n = Array.length puzzle in
  let rec check i j =
    if i = n then true
    else if j = n then check (i + 1) 0
    else if puzzle.(i).(j) = ' ' then false
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
  let colors = [| 'B'; 'V'; 'R'; 'J'; 'N'; 'O'; 'G'; 'M'; 'C'; 'o' |] in
  let rec solve i j =
    if i = n then true
    else if j = n then solve (i + 1) 0
    else if puzzle.(i).(j) <> ' ' then solve i (j + 1)
    else
      let rec try_color c =
        if c = n then false
        else begin
          puzzle.(i).(j) <- colors.(c);
          if solve i (j + 1) then true
          else begin
            puzzle.(i).(j) <- ' ';
            try_color (c + 1)
          end
        end
      in
      try_color 0
  in
  solve 0 0

(* Affichage du puzzle *)
let print_puzzle puzzle =
  let n = Array.length puzzle in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      print_char puzzle.(i).(j);
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
(* Programme principal *)
let rec main () =
  print_endline "--   Menu Principal--  ";
  print_endline "Que voulez-vous faire ?";
  print_endline "1. Générer un puzzle puis afficher";
  print_endline "2. Générer et résoudre le puzzle puis afficher";
  print_endline "3. Quitter";

  try
    match read_int () with
    | 1 ->
        let n = 12 in
        let k = 10 in
        let puzzle = generate_puzzle n k in
        print_endline "Puzzle généré :";
        print_puzzle puzzle;
        print_endline "Voulez-vous résoudre le puzzle (o/n) ?";
        (match read_line () with
         | "o" -> if solve puzzle then begin
             print_endline "Puzzle résolu :";
             print_puzzle puzzle
           end
             else
               print_endline "Impossible de résoudre le puzzle.";
             main ()
         | _ -> main ())
    | 2 ->
        let n = 12 in
        let k = 10 in
        let puzzle = generate_puzzle n k in
        if solve puzzle then begin
          print_endline "Puzzle généré et résolu :";
          print_puzzle puzzle
        end
        else
          print_endline "Impossible de résoudre le puzzle.";
        print_endline "Taper n pour retourner au menu principal";
        (match read_line () with
         | "n" -> main ()
         | _ -> main ())
    | 3 ->
        print_endline "Au revoir!";
        exit 0
    | _ ->
        print_endline "Option invalide.";
        main ()
  with
  | Failure _ ->
      print_endline "Option invalide.";
      main ()

let () = main ()

