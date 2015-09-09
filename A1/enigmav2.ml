(*Discussed the operations of the enigma with grr37 ssh88*)
let testString = "HELLO WORLD"
let id       = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotorI   = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotorII  = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let rotorIII = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let reflB    = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
let rotorList = rotorI::rotorII::rotorIII::[]

let cipherHelper (refl:string) (rotors:string list) (notches:char list) 
  (starts:char list) (s:string) (verbose:bool) : string = 

  (* Takes in a list of characters and returns a list of indices 
   * that the letters correspond to. A=0, B=1, C=3, *)
  let rec getLetterIndices letters = 
    match letters with
    | [] -> []
    | hd::tl -> Char.code hd - 65 :: getLetterIndices tl in

  (* Calculate the start position relative to A, the 0th letter*)
  let startPos = getLetterIndices starts in

  (* Calculate the notch positions relative to A, the 0th letter*)
  let notchPos = getLetterIndices notches in

  (* Maps an input postion (inPos) to an output position with rotor r 
   * and rotor position rPos from right to left*)
  let getOutputPos r rPos inPos= 
    let index = Char.code (String.get r ((rPos + inPos) mod 26)) - 65 in
    let outPos = if index < rPos then index + 26 - rPos else index - rPos in
      (if verbose then Printf.printf "%c -> %c \n" 
      (Char.chr (inPos + 65)) (Char.chr (outPos + 65)); outPos) in

  (* Ciphers a letter through the rotors from right to left
   * Takes in input current rotor position rotorPos and input position
   * represented by letter (an ASCII uppercase char); This function 
   * maps an input position to an output position through the rotors*)
  let mapRotors rotorPos letter = 
    let rec rotorSim r rPos inPos= 
      match r,rPos with
      | hd_r::tl_r, hd_rp::tl_rp -> 
          if tl_r = [] then getOutputPos hd_r hd_rp inPos
          else rotorSim tl_r tl_rp (getOutputPos hd_r hd_rp inPos)
      | _, _ -> -33 in
  Char.chr (rotorSim (List.rev rotors) (List.rev rotorPos) 
    (Char.code letter - 65) + 65) in

  (* Maps a letter through the reflector*)
  let mapRefl letter = 
    let output = String.get refl (Char.code letter - 65) in
    (if verbose then Printf.printf "%c -> %c \n" letter output; output) in

  (* Maps an input position to an outputposition with rotor r and rotor
   * position rPos from left to right*)
  let getOutputPosRev r rPos inPos = 
    let index = String.index r (Char.chr ((rPos + inPos) mod 26 + 65)) in
    let outPos =  if index < rPos then index + 26 - rPos else index - rPos in
      (if verbose then Printf.printf "%c -> %c \n" 
      (Char.chr (inPos + 65)) (Char.chr (outPos + 65)); outPos) in

  (* Ciphers a letter through the rotors from left to right. Same 
   * concept as mapRotors*)
  let mapRotorsRev rotorPos letter = 
    let rec rotorSim r rPos inPos = 
      match r,rPos with
      | hd_r::tl_r, hd_rp::tl_rp -> 
          if tl_r = [] then getOutputPosRev hd_r hd_rp inPos 
          else rotorSim tl_r tl_rp (getOutputPosRev hd_r hd_rp inPos) 
      | _, _ -> -33 in
  Char.chr (rotorSim rotors rotorPos (Char.code letter - 65) + 65) in

  (* Runs the enigma encryption process on a single letter using the 
   * current rotor position rotorPos by chaining all of the mapping
   * helper functions for various stages of the enigma machine*)
  let mapAll rotorPos letter = 
    mapRotorsRev rotorPos (mapRefl (mapRotors rotorPos letter)) in

  (* Calculates the next set of rotor positions based on the current 
   * rotor positions. The input and output rotor positions are in the 
   * order of left to right *)
  let getNextRotorPos curPos =          
    (* Just a helper function to get the next index with wrapping*)
    let nextIndex i = 
      if i = 25 then 0 else i+1 in

    (* The helper function's rotor pos input list must be right to left 
     * The returned output list is left to right*)
    let rec calcRotorPos curP notchP turnRotor nextPos= 
      match curP,notchP with
      | hd_s::tl_s, hd_n::tl_n -> 
          if List.length tl_s + 1 = List.length rotors then
            calcRotorPos tl_s tl_n (if hd_s = hd_n then true else false) 
            ((nextIndex hd_s)::nextPos)
          else 
            calcRotorPos tl_s tl_n (if hd_s = hd_n then true else false)
            (if turnRotor || (if tl_s = [] then false else hd_s = hd_n)
            then ((nextIndex hd_s)::nextPos)
            else (hd_s::nextPos))
      | _, _ -> nextPos in
    calcRotorPos (List.rev curPos) (List.rev notchPos) false [] in

  (* Recurse through the string str and encrypt every letter 
   * s_idx is the current absolute index in the string
   * c_idx is the current letter index in the string for use by
   *    the cipher mapping; it doesn't include puntuation or spaces
   * e_list the list of reverse characters that has been encrypted*)
  let rec encrypt str s_idx rotPos e_list = 
    match str.[s_idx] with
    | ' ' -> if s_idx + 1 < String.length str then 
      encrypt str (s_idx+1) rotPos (" "::e_list) else " "::e_list
    | _ -> 
      let nextRotorPos = getNextRotorPos rotPos in
      (if verbose then 
      (Printf.printf "Current Rotor Position: " ;
      List.iter 
        (fun rIdx -> Printf.printf "%c " (Char.chr (rIdx + 65)))
        (getNextRotorPos rotPos) ;
      Printf.printf "\n") ;
      if s_idx + 1 < String.length str then 
        encrypt str (s_idx+1) (nextRotorPos) (
        (Char.escaped (mapAll (nextRotorPos) str.[s_idx]))::e_list) 
      else 
        (Char.escaped (mapAll (nextRotorPos) str.[s_idx]))::e_list) in
  String.concat "" (List.rev (encrypt (String.uppercase s) 0 startPos []))

let cipher (refl:string) (rotors:string list) (notches:char list) 
  (starts:char list) (s:string) : string = 
  cipherHelper refl rotors notches starts s false

let simulate (refl:string) (rotors:string list) (notches:char list) 
  (starts:char list) (s:string) : string = 
  cipherHelper refl rotors notches starts s true
