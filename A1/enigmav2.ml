(*Discussed the operations of the enigma with grr37 ssh88*)
let testString = "HELLO WORLD"
let id       = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotorI   = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotorII  = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let rotorIII = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let reflB    = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
let rotorList = rotorI::rotorII::rotorIII::[]

let printLineLetter idx letter = 
  Printf.printf "%d %c \n" idx letter
let cipher (refl:string) (rotors:string list) (notches:char list) 
  (starts:char list) (s:string) = 
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
  (* Map an input postion to an output position with rotor r and rotor
   * position rPos*)
  let getOutputPos inPos r rPos = 
    let index = 
      Char.code (String.get r ((rPos + inPos) mod 26)) - 65 in
    let output = 
      if index < rPos then 
        index + 26 - rPos else index - rPos in
    let print = Printf.printf "%d %d\n" index output in
    output in
  (* Ciphers a letter through the rotors from right to left
   * Takes in input current rotor position rotorPos and input position
   * represented by letter (a char); This function maps an input position
   * to an output position through the rotors*)
  let mapRotors rotorPos letter = 
    let rec rotorSim r rPos inPos= 
      match r,rPos with
      | [],[] -> -33
      | hd_r::tl_r, hd_rp::tl_rp -> 
          let print2 = Printf.printf 
          "Rotor Pos: %d In Pos: %d \n" hd_rp inPos in 
          if tl_r = [] then getOutputPos inPos hd_r hd_rp 
          else rotorSim tl_r tl_rp (getOutputPos inPos hd_r hd_rp)in
  Char.chr (rotorSim (List.rev rotors) (List.rev rotorPos) 
    (Char.code letter - 65) + 65) in

  let mapRefl letter = 
    if Char.code letter = 32 then ' ' else 
      String.get refl (Char.code letter - 65) in

  let getOutputPosRev r rPos inPos = 
    let index = String.index r (Char.chr ((rPos + inPos) mod 26 + 65)) in
    let output = 
      if index < rPos then 
        index + 26 - rPos else index - rPos in 
    let print = Printf.printf "%d %d \n" index output in
    output in
  let mapRotorsRev rotorPos letter = 
    let rec rotorSim r rPos inPos = 
      match r,rPos with
      | [],[] -> -33
      | hd_r::tl_r, hd_rp::tl_rp -> 
          let print = Printf.printf 
          "Rotor Pos: %d In Pos: %d \n\n" hd_rp inPos in
          if tl_r = [] then getOutputPosRev hd_r hd_rp inPos 
          else rotorSim tl_r tl_rp (getOutputPosRev hd_r hd_rp inPos) in
  Char.chr (rotorSim rotors rotorPos (Char.code letter - 65) + 65) in

  let mapAll rotorPos letter = 
    mapRotorsRev rotorPos (mapRefl (mapRotors rotorPos letter)) in

  (* Calculates the next set of rotor positions based on the current 
   * rotor positions. The input and output rotor positions are in the 
   * order of left to right *)
  let getNextRotorPos curPos =          
    let nextIndex i = 
      if i = 25 then 0 else i+1 in
    (* The helper function rotor pos input list must be right to left 
     * The returned output list is left to right*)
    let rec calcRotorPos curP notchP turnRotor nextPos= 
      match curP,notchP with
      | [],[] -> nextPos
      | hd_s::tl_s, hd_n::tl_n -> 
          if List.length tl_s + 1 = List.length rotors then
            calcRotorPos tl_s tl_n (if hd_s = hd_n then true else false) 
            ((nextIndex hd_s)::nextPos)
          else 
            calcRotorPos tl_s tl_n (if hd_s = hd_n then true else false)
            (if turnRotor || (if tl_s = [] then false else hd_s = hd_n)
             then ((nextIndex hd_s)::nextPos)
             else (hd_s::nextPos)) in
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
        let print = List.iter 
          (fun rIdx -> Printf.printf "%d " (Char.chr (rIdx + 65)))
          (getNextRotorPos rotPos) in
        if s_idx + 1 < String.length str then 
      encrypt str (s_idx+1) (getNextRotorPos rotPos) (
        (Char.escaped (mapAll (getNextRotorPos rotPos) str.[s_idx]))::e_list) 
    else 
        (Char.escaped (mapAll (getNextRotorPos rotPos) str.[s_idx]))::e_list in
  String.concat "" (List.rev (encrypt (String.uppercase s) 0 startPos []))
