(*Discussed the operations of the enigma with grr37 ssh88*)
let testString = "HELLO WORLD"
let id       = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotorI   = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotorII  = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let rotorIII = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let reflB    = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
let rotorList = rotorI::rotorII::rotorIII::[]

let cipher_helper (refl:string) (rotors:string list) (notches:char list) 
  (starts:char list) (s:string) (verbose:bool) : string = 

  (* Takes in a list of characters and returns a list of indices 
   * that the letters correspond to. A=0, B=1, C=3, *)
  let rec get_letter_indices letters = 
    match letters with
    | [] -> []
    | hd::tl -> Char.code (Char.uppercase hd) - 65 :: get_letter_indices tl in

  (* Calculate the start position relative to A, the 0th letter*)
  let start_pos = get_letter_indices starts in

  (* Calculate the notch positions relative to A, the 0th letter*)
  let notch_pos = get_letter_indices notches in

  (* Maps an input postion (in_pos) to an output position with rotor r 
   * and rotor position r_pos from right to left*)
  let get_output_pos r r_pos in_pos= 
    let index = Char.code (String.get r ((r_pos + in_pos) mod 26)) - 65 in
    let out_pos = if index < r_pos then index + 26 - r_pos else index - r_pos in
      (if verbose then Printf.printf "%c -> %c \n" 
      (Char.chr (in_pos + 65)) (Char.chr (out_pos + 65)); out_pos) in

  (* Ciphers a letter through the rotors from right to left
   * Takes in input current rotor position rotor_pos and input position
   * represented by letter (an ASCII uppercase char); This function 
   * maps an input position to an output position through the rotors*)
  let map_rotors rotor_pos letter = 
    let rec rotor_sim r r_pos in_pos= 
      match r,r_pos with
      | hd_r::tl_r, hd_rp::tl_rp -> 
          if tl_r = [] then get_output_pos hd_r hd_rp in_pos
          else rotor_sim tl_r tl_rp (get_output_pos hd_r hd_rp in_pos)
      | _, _ -> -33 in
  Char.chr (rotor_sim (List.rev rotors) (List.rev rotor_pos) 
    (Char.code letter - 65) + 65) in

  (* Maps a letter through the reflector*)
  let map_refl letter = 
    let output = String.get refl (Char.code letter - 65) in
    (if verbose then Printf.printf "%c -> %c \n" letter output; output) in

  (* Maps an input position to an outputposition with rotor r and rotor
   * position r_pos from left to right*)
  let get_output_pos_rev r r_pos in_pos = 
    let index = String.index r (Char.chr ((r_pos + in_pos) mod 26 + 65)) in
    let out_pos =  if index < r_pos then index + 26 - r_pos else index - r_pos in
      (if verbose then Printf.printf "%c -> %c \n" 
      (Char.chr (in_pos + 65)) (Char.chr (out_pos + 65)); out_pos) in

  (* Ciphers a letter through the rotors from left to right. Same 
   * concept as map_rotors*)
  let map_rotors_rev rotor_pos letter = 
    let rec rotor_sim r r_pos in_pos = 
      match r,r_pos with
      | hd_r::tl_r, hd_rp::tl_rp -> 
          if tl_r = [] then get_output_pos_rev hd_r hd_rp in_pos 
          else rotor_sim tl_r tl_rp (get_output_pos_rev hd_r hd_rp in_pos) 
      | _, _ -> -33 in
  Char.chr (rotor_sim rotors rotor_pos (Char.code letter - 65) + 65) in

  (* Runs the enigma encryption process on a single letter using the 
   * current rotor position rotor_pos by chaining all of the mapping
   * helper functions for various stages of the enigma machine*)
  let map_all rotor_pos letter = 
    map_rotors_rev rotor_pos (map_refl (map_rotors rotor_pos letter)) in

  (* Calculates the next set of rotor positions based on the current 
   * rotor positions. The input and output rotor positions are in the 
   * order of left to right *)
  let get_next_rotor_pos cur_pos =          
    (* Just a helper function to get the next index with wrapping*)
    let next_index i = 
      if i = 25 then 0 else i+1 in

    (* The helper function's rotor pos input list must be right to left 
     * The returned output list is left to right*)
    let rec calc_rotor_pos cur_p notch_p turn_rotor next_pos= 
      match cur_p,notch_ with
      | hd_s::tl_s, hd_n::tl_n -> 
          if List.length tl_s + 1 = List.length rotors then
            calc_rotor_pos tl_s tl_n (if hd_s = hd_n then true else false) 
            ((next_index hd_s)::next_pos)
          else 
            calc_rotor_pos tl_s tl_n (if hd_s = hd_n then true else false)
            (if turn_rotor || (if tl_s = [] then false else hd_s = hd_n)
            then ((next_index hd_s)::next_pos)
            else (hd_s::next_pos))
      | _, _ -> next_pos in
    calc_rotor_pos (List.rev cur_pos) (List.rev notch_pos) false [] in

  (* Recurse through the string str and encrypt every letter 
   * s_idx is the current absolute index in the string
   * c_idx is the current letter index in the string for use by
   *    the cipher mapping; it doesn't include puntuation or spaces
   * e_list the list of reverse characters that has been encrypted*)
  let rec encrypt str s_idx rot_pos e_list = 
    match str.[s_idx] with
    | ' ' -> if s_idx + 1 < String.length str then 
      encrypt str (s_idx+1) rot_pos (" "::e_list) else " "::e_list
    | _ -> 
      let next_rotor_pos = get_next_rotor_pos rot_pos in
      (if verbose then 
      (Printf.printf "Current Rotor Position: " ;
      List.iter 
        (fun rIdx -> Printf.printf "%c " (Char.chr (rIdx + 65)))
        (get_next_rotor_pos rot_pos) ;
      Printf.printf "\n") ;
      if s_idx + 1 < String.length str then 
        encrypt str (s_idx+1) (next_rotor_pos) (
        (Char.escaped (map_all (next_rotor_pos) str.[s_idx]))::e_list) 
      else 
        (Char.escaped (map_all (next_rotor_pos) str.[s_idx]))::e_list) in
  String.concat "" (List.rev (encrypt (String.uppercase s) 0 start_pos []))

let cipher (refl:string) (rotors:string list) (notches:char list) 
  (starts:char list) (s:string) : string = 
  cipher_helper refl rotors notches starts s false

let simulate (refl:string) (rotors:string list) (notches:char list) 
  (starts:char list) (s:string) : string = 
  cipher_helper refl rotors notches starts s true
