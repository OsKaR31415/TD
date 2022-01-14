(* project: TD1
   authors: Plaisant, Matuszenski
   date: 12 jan 2022 *)


(* ╺┓          ╺┳┓┏━╸┏━╸┏━┓╻ ╻╻ ╻┏━╸┏━┓╺┳╸┏━╸   ╺┳┓╻┏━┓┏━╸┏━┓┏┳┓╻   *)
(*  ┃    ╺━╸    ┃┃┣╸ ┃  ┃ ┃┃ ┃┃┏┛┣╸ ┣┳┛ ┃ ┣╸     ┃┃ ┃ ┃┃  ┣━┫┃┃┃┃   *)
(* ╺┻╸         ╺┻┛┗━╸┗━╸┗━┛┗━┛┗┛ ┗━╸╹┗╸ ╹ ┗━╸   ╺┻┛ ┗━┛┗━╸╹ ╹╹ ╹┗━╸ *)

(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢺ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠼⠄*)


(* division entière de 29 (int) par 4 (int) *)
29 / 4;;

(* erreur, car /. est une division entre float, alors que 29 et 4 sont des int *)
29 /. 4;;

(* division de 29 (float) par 4 (float) renvoie un float *)
29.0 /. 4.0;;

(* renvoie le reste de la division entière de 29 (int) par 4 (int) *)
29 mod 4;;

(* erreur, car + est l'addition pour les int, alors que 0.5 et 22.2 sont des float *)
0.5 + 20.2;;


(* addition de 0.5 (float) et 20.2 (float) *)
0.5 +. 20.2;;

(* Cette expression est un 2-uplet qui contient un int puis un string (il est
   donc du type int*string) *)
(42, "A");;

(* renvoie la réponse à la grande question sur la vie, l'univers et tout le reste
   prends le premier élément du 2-uplet (42, "A") *)
fst (42, "A");;

(* prends le second élément du 2-uplet (42, "A"), soit "A" *)
snd (42, "A");;

(* teste si 2 est égal à 2 . Renvoie true car c'est vrai *)
2 = 2;;

(* teste si 2 est différent de 2 . Renvoie false car c'est faux *)
2 <> 2;;

(* concatène (joint) la chaîne "a" avec la chaîne "A". renvoie "aA" *)
"a" ^ "A";;

(* opérateur || entre false et true. cet opérateu est aussi appelé "or", ou bien
   "ou", car il représente le "ou" logique. "faux ou vrai" est vrai, donc
   l'expression renvoie vrai *)
false || true;;

(* on sait que (2 = 3) est faux, donc qu'il vaut false. Le && est le ET logique.
   "faux et vrai" vaut "faux". donc l'expression renvoie false *)
(2 = 3) && true;;



(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠮⠤ *)

(* signature de la fonction
   in_between (x: int) (lower: int) (upper: int) : bool *)

(* corps de la fonction
   contient simplement 2 comparaisons *)
let in_between x lower upper =
    if x <= upper && x >= lower then true
    else false;;

(* tests unitaires *)
in_between 6 4 10 = true;;
in_between 2 5 10 = false;;
in_between 11 5 10 = false;;
in_between 5 5 6 = true;;
in_between 6 5 6 = true;;




(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢉⡹ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠤⠜ *)

(* signature de la fonction
   max3 (n1: int) (n2: int) (n3: int) : int *)

let max3 (n1: int) (n2: int) (n3: int) : int =
    max (max n1 n2) n3;;

(* tests unitaires *)
max3 1 2 3 = 3;;
max3 1 3 2 = 3;;
max3 3 1 2 = 3;;
max3 0 0 0 = 0;;

(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢇⣸ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶  ⠸ *)

(* signature de la fonction
   let signe (x: int) : int;; *)

let sign (x: int) : int =
    if x = 0 then 0
    else if x > 0 then 1
    else (-1);;

(* tests unitaires *)
sign 42 = 1;;
sign 0 = 0;;
sign (-73) = -1;;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⣏⡉ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠤⠜ *)

(* signature de la fonction
   mean (a : int) (b : int) : float *)

let mean (a : int) (b : int) : float =
    (float_of_int a +. float_of_int b) /. 2.;;

(* tests unitaires *)
mean 0 10 = 5.;;
mean 0 0 = 0.;;
mean (-1) 1 = 0.;;


(* ┏━┓         ┏━┓┏━┓╻┏┓╻╺┳╸┏━┓ *)
(* ┏━┛   ╺━╸   ┣━┛┃ ┃┃┃┗┫ ┃ ┗━┓ *)
(* ┗━╸         ╹  ┗━┛╹╹ ╹ ╹ ┗━┛ *)

type point = float * float;;

(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠼⠄ *)


(* signature de la fonction
   dist_origin (a: point) : float *)

let dist_origin (a: point) : float =
    sqrt((fst a)**2. +. (fst a)**2.);;

(* tests unitaires *)
dist_origin (2., 2.) = 2.*.sqrt(2.);;
dist_origin (0., 0.) = 0.;;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠮⠤ *)

(* signature de la fonction
   closest_origin (a: point) (b: point) : point *)

let closest_origin (a: point) (b: point) : point =
    if (dist_origin a) < (dist_origin b) then a
    else b;;

(* tests unitaires *)
closest_origin (2., 2.) (4., 4.) = (2., 2.);;
closest_origin (10., 5.) (2., (-1.)) = (2., (-1.));;
closest_origin (10., 1.) (2., 2.) = (2., 2.);;




(* ┏━┓         ╺━┓┏━┓┏┓╻┏━╸┏━┓ *)
(* ╺━┫   ╺━╸   ┏━┛┃ ┃┃┗┫┣╸ ┗━┓ *)
(* ┗━┛         ┗━╸┗━┛╹ ╹┗━╸┗━┛ *)

type zone = {bottom_left : point; top_right : point};;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢉⡹   ⢺ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠤⠜ ⠶ ⠼⠄*)

(* signature de la fonction
   in_zone (a : point) (r : zone) : bool *)

(* la fonction est simplement une série de tests pour vérrifier que le point
   est du bon côté de chacune des droites portées par les côtés du rectangle. *)
let in_zone (a: point) (r: zone): bool =
    (fst a) < (fst r.top_right)
    && (fst a) > (fst r.bottom_left)
    && (snd a) < (snd r.top_right)
    && (snd a) > (snd r.bottom_left);;

(* tests unitaires *)
in_zone (0., 0.) {bottom_left=(0., 0.); top_right=(3., 3.)} = false;;
in_zone (2., 2.) {bottom_left=(0., 0.); top_right=(3., 3.)} = true;;
in_zone (10., 2.) {bottom_left=(0., 0.); top_right=(3., 3.)} = false;;
in_zone (2., 10.) {bottom_left=(0., 0.); top_right=(3., 3.)} = false;;
in_zone (10., 10.) {bottom_left=(0., 0.); top_right=(3., 3.)} = false;;



(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢉⡹   ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠤⠜ ⠶ ⠮⠤ *)


(* signature de la fonction
   is_subzone (z1: zone) (z2: zone) : bool *)

(* Pour que z1 soit dans z2, il faut que z1.bottom_left soit au dessus et à
   droite de z2.bottom_left, et que z1.top_right soit en dessous et à gauche de
   z2.top_right *)
let is_subzone (z1: zone) (z2: zone) : bool =
    (fst z1.bottom_left) >= (fst z2.bottom_left) && (snd z1.bottom_left) >= (snd z2.bottom_left)
    && (fst z1.top_right) <= (fst z2.top_right) && (snd z1.top_right) <= (snd z1.top_right);;



(* tests unitaires *)
let r1 = {bottom_left=(2.,2.); top_right=(3.,3.)};;
let r2 = {bottom_left=(0.,0.); top_right=(1.,1.)};;
let r3 = {bottom_left=(1.,1.); top_right=(5.,5.)};;
let r4 = {bottom_left=(0.,0.); top_right=(3.,3.)};;

is_subzone r4 r4 = true;;
is_subzone r1 r2 = false;;
is_subzone r2 r4 = true;;
is_subzone r3 r4 = false;;
is_subzone r1 r2 = false;;


