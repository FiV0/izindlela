
exception IndexOutOfBounds;;

type 'a dynArray = { mutable used: int ; mutable arr: 'a array};;

let create () = { used = 0 ; arr = [||] };;

let empty da = if da.used = 0 then true else false ;;

let size da = da.used;;

let set da i e = if da.used = 0 || ( i < 0 || da.used <= i)
                 then raise IndexOutOfBounds
                 else Array.set da.arr i e ;;

let get da i = if da.used = 0 || ( i < 0 || da.used <= i)
               then raise IndexOutOfBounds
               else Array.get da.arr i;;

let add da e = match da.used with
               | 0 -> da.arr <- Array.make 10 e; da.used <- 1
               | n -> (if n = Array.length da.arr then
                      let tmp = Array.make (2*n) da.arr.(0) in
                      Array.blit da.arr 0 tmp 0 da.used;
                      da.arr <- tmp); 
                      Array.set da.arr da.used e;
                      da.used <- da.used + 1;;

let iter f da = 
  let rec iter_aux : int -> unit = fun i ->
    if i < da.used then begin
      f da.arr.(i);
      iter_aux (i+1) end else () in
  iter_aux 0;;

let fold_left f e da =
  let rec fold_left_aux f e da i = 
    if i = da.used then e 
    else fold_left_aux f (f e da.arr.(i)) da (i+1) in
  fold_left_aux f e da 0;;

                      
                      
