let rec gcdExt_aux1 = function
  | (a, 0) -> [a]
  | (a, b) -> ( a / b ) :: ( gcdExt_aux1 (b, (a mod b)) ) ;;


let rec gcdExt_aux2 = function
  | [] -> (0, 1)
  | q :: t -> 
      begin
        let (x, y) = gcdExt_aux2 t in 
          (y, x - q*y)
      end;;


  let gcdExt a b = 
    begin
      let l = gcdExt_aux1 (a, b) in
        let (u, v) = gcdExt_aux2 ( List.tl ( List.rev (  List.tl l  ) )  ) in
          let (x, y) = gcdExt_aux2 ( List.tl ( List.rev (  l  ) )  ) in
            (u, x)
    end

let rec print_list = function 
  | [] -> print_string "\n"
  | [x] -> 
      begin 
        print_int x;
        print_string "\n"
      end
  | x :: xs -> 
      begin         
        print_int x;
        print_string " ";
        print_list xs
      end;;


let (x, y) = gcdExt 462382 4354352 in
  print_string ( (string_of_int x) ^ " " ^ (string_of_int y) ^ "\n" );