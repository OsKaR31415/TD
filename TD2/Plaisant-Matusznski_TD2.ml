
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
float_of_frac {num=5; denom=2} = 2.5;;



 (* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢇⣸ *)
 (* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶  ⠸ *)

let add_fracs (f1 : frac) (f2: frac) : frac =
    {num = f1.num*f2.denom + f1.denom*f2.num; denom = f1.denom*f2.denom};;


add_fracs {num=4; denom=2} {num=7; denom=7} = {num=42; denom=14};;
add_fracs {num=7; denom=7} {num=2; denom=4} = {num=42; denom=28};;
add_fracs {num=0; denom=6} {num=4; denom=2} = {num=24; denom=12};;
add_fracs {num=5; denom=2} {num=4; denom=2} = {num=18; denom=4};;



 (* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⣏⡉ *)
 (* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠤⠜ *)

type number = Integer of int | Real of float | Rational of frac;;


let float_of_number (n: number): float =
    match n with
        | Integer (c: int) -> float_of_int c
        | Real (c: float) -> c
        | Rational (c: frac) -> float_of_frac c;;

float_of_number (Integer 5) = 5.;;
float_of_number (Real 7.3) = 7.3;;
float_of_number (Rational {num=10; denom=5}) = 2.;;
float_of_number (Rational {num=9; denom=5}) = 1.8;;


let add_numbers (n: number) (p: number) : number =
    match (n, p) with
        | (Integer (n_: int), _p) -> (match _p with
            | Integer (p_: int) -> Integer(n_ + p_)
            | Real (p_: float) -> Real((float_of_int n_) +. p_)
            | Rational (p_: frac) -> Real((float_of_int n_) +. (float_of_frac p_)))
        | (Real (n_: float), _p) -> (match _p with
            | Integer(p_: int) -> Real(n_ +. (float_of_int p_))
            | Real(p_: float) -> Real(n_ +. p_)
            | Rational(p_: frac) -> Real(n_ +. (float_of_frac p_)))
        | (Rational (n_: frac), _p) -> (match _p with
            | Integer(p_: int) -> Real((float_of_frac n_) +. (float_of_int p_))
            | Real(p_: float) -> Real((float_of_frac n_) +. p_)
            | Rational(p_: frac) -> Real((float_of_frac n_) +. (float_of_frac p_)));;

add_numbers (Integer 1) (Integer 2) = (Integer 3);;
add_numbers (Integer 1) (Real 0.5) = (Real 1.5);;
add_numbers (Integer 1) (Rational {num=1; denom=2}) = (Real 1.5);;
add_numbers (Real 0.5)  (Integer 1) = (Real 1.5);;
add_numbers (Real 1.25) (Real 0.25) = (Real 1.5);;
add_numbers (Real 1.25) (Rational {num=1; denom=4}) = (Real 1.5);;
add_numbers (Rational {num=1; denom=2}) (Integer 1) = (Real 1.5);;
add_numbers (Rational {num=5; denom=4}) (Real 0.25) = (Real 1.5);;
add_numbers (Rational {num=5; denom=4}) (Rational {num=1; denom=4}) = (Real 1.5);;


(* ┏━┓         ┏━╸┏━╸╻ ╻┏━┓   ╺┳┓┏━╸   ┏━╸╻┏━┓┏━╸╻ ╻╻  ┏━┓╺┳╸╻┏━┓┏┓╻ *)
(* ┏━┛   ╺━╸   ┣╸ ┣╸ ┃ ┃┗━┓    ┃┃┣╸    ┃  ┃┣┳┛┃  ┃ ┃┃  ┣━┫ ┃ ┃┃ ┃┃┗┫ *)
(* ┗━╸         ╹  ┗━╸┗━┛┗━┛   ╺┻┛┗━╸   ┗━╸╹╹┗╸┗━╸┗━┛┗━╸╹ ╹ ╹ ╹┗━┛╹ ╹ *)

type trafficLight = GreenLight | OrangeLight | RedLight;;

 (* ⣏⡉ ⢇⡸ ⣏⡉ ⡎⠑ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢺  *)
 (* ⠧⠤ ⠇⠸ ⠧⠤ ⠣⠔ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠼⠄ *)

let next_light (light: trafficLight): trafficLight =
    match light with
    | GreenLight -> OrangeLight
    | OrangeLight -> RedLight
    | RedLight -> GreenLight;;

next_light GreenLight = OrangeLight;;
next_light OrangeLight = RedLight;;
next_light RedLight = GreenLight;;


 (* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⠊⡱ *)
 (* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠮⠤ *)

let light_time (light: trafficLight): int =
    match light with
    | GreenLight -> 50
    | OrangeLight -> 10
    | RedLight -> 40;;

 (* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢉⡹ *)
 (* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠤⠜ *)

type trafficTimer = {light: trafficLight; remaining_time: int};;

let traffic_step (current_timer: trafficTimer): trafficTimer =
    if (current_timer.remaining_time = 0)
    then {
        light=(next_light current_timer.light);
        remaining_time=(light_time (next_light current_timer.light)) }
    else {
        light=current_timer.light;
        remaining_time=current_timer.remaining_time-1
    };;

traffic_step {light=OrangeLight; remaining_time=9} = {light=OrangeLight; remaining_time=8};;
traffic_step {light=RedLight; remaining_time=0} = {light=GreenLight; remaining_time=50};;
traffic_step {light=GreenLight; remaining_time=34} = {light=GreenLight; remaining_time=33};;


 (* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢇⣸ *)
 (* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶  ⠸ *)

let rec traffic_next (s: trafficTimer) (n: int) =
    match (s, n) with
    | (traffic, 0) -> traffic
    | (traffic, time) -> traffic_next (traffic_step traffic) (time - 1);;

traffic_next {light=OrangeLight; remaining_time=9} 15 = {light=RedLight; remaining_time=15};;
traffic_next {light=GreenLight; remaining_time=5} 45 = {light=RedLight; remaining_time=12};;





