
(* TD2 Types enumeres *)


(* ╺┓          ┏┓╻┏━┓┏┳┓┏┓ ┏━┓┏━╸┏━┓   ┏━╸┏━╸┏┓╻┏━╸┏━┓╻┏━┓╻ ╻┏━╸┏━┓ *)
(*  ┃    ╺━╸   ┃┗┫┃ ┃┃┃┃┣┻┓┣┳┛┣╸ ┗━┓   ┃╺┓┣╸ ┃┗┫┣╸ ┣┳┛┃┃┓┃┃ ┃┣╸ ┗━┓ *)
(* ╺┻╸         ╹ ╹┗━┛╹ ╹┗━┛╹┗╸┗━╸┗━┛   ┗━┛┗━╸╹ ╹┗━╸╹┗╸╹┗┻┛┗━┛┗━╸┗━┛ *)

type number = Integer of int | Real of float;;

 (* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢺  *)
 (* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠼⠄ *)

let float_of_number (n: number): float =
    match n with
        | Integer (c: int) -> float_of_int c
        | Real (c: float) -> c;;


float_of_number (Integer 0)   ;;
float_of_number (Real 0.)  ;;
float_of_number (Integer 10)  ;;
float_of_number (Real 10.) ;;

 (* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⠊⡱ *)
 (* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠮⠤ *)

let add_numbers (n: number) (p: number) : number =
    match (n,p) with
        | (Integer (n_: int), Integer (p_: int)) -> Integer(n_ + p_)
        | (Integer (n_: int), Real (p_: float)) -> Real((float_of_int n_) +. p_)
        | (Real (n_: float), Integer (p_: int)) -> Real(n_ +. (float_of_int p_))
        | (Real (n_: float), Real (p_: float)) -> Real(n_ +. p_);;

add_numbers (Integer 0)  (Integer 0) = Integer(0);;
add_numbers (Integer 42) (Real 0.)   = Real(42.);;
add_numbers (Real 7.)    (Integer 0) = Real(7.);;
add_numbers (Real 42.)   (Real 73.)  = Real(115.);;


 (* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢉⡹ *)
 (* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠤⠜ *)

type frac = { num : int; denom : int };;

let float_of_frac (fraction: frac) : float =
    (float_of_int fraction.num) /. (float_of_int fraction.denom);;

float_of_frac {num=4; denom=2} = 2.;;
float_of_frac {num=7; denom=7} = 1.;;
float_of_frac {num=5; denom=0} = infinity;; (* ça fait mal à nos maths ;( *)
float_of_frac {num=5; denum=2} = 2.5;;



 (* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢇⣸ *)
 (* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶  ⠸ *)

let add_fracs (f1 : frac) (f2: frac) : frac =
    {num = f1.num*f2.denom + f1.denom*f2.num; denom = f1.denom*f2.denom};;


add_fracs {num=4; denom=2} {num=7; denom=7} = {num=42; denom=28};;
add_fracs {num=7; denom=7} {num=2; denom=4} = {num=42; denom=28};;
aid_fracs {num=0; denom=6} {num=4; denom=2} = {num=24; denom=12};;
add_fracs {num=5; denom=2} {num=4; denom=2} = {num=18; denom=4};;

42
