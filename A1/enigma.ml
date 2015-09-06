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
  (starts:char list) (s:string) : string = 
  (* Locate where the spaces are and remove them *)
  let spaceLocations= 
    let rec searchSpace str i locList = 
      match str.[i] with
      | ' ' -> if i + 1 < (String.length str) 
        then searchSpace str (i+1) (i::locList) else i::locList
      | _ -> if i + 1 < (String.length str)
        then searchSpace str (i+1) locList else locList in
    searchSpace s 0 [] in
  (* s_in is the input string with spaces removed*)
  let s_in = 
    let rec removeSpace 
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
  (* Map an input postion to an output position with rotor r, starting
   * position s, shift position i *)
  let getOutputPos pos r s i = 
    let index = 
      Char.code (String.get r ((s + i + pos) mod 26)) - 65 in
    let output = 
      if index < (s + i) then index + 26 - (s + i) else index - (s + i) in
    let print = Printf.printf "%d %d\n" index output in
    output in
  (* Ciphers a letter through the rotors right to left*)
  let mapRotors idx letter = 
    let rec rotorSim r n s idx pos = 
      match r,n,s with
      | [],[],[] -> -32
      | hd_r::tl_r, hd_n::tl_n, hd_s::tl_s -> 
          let shift = if List.length tl_r + 1 = List.length rotors then
            idx + 1 else idx in
          let print2 = Printf.printf 
          "Rotor Shift: %d In Pos: %d Starting Pos: %d \n" shift pos hd_s in
          if pos = -33 then -33 else 
            if tl_r = [] then getOutputPos pos hd_r hd_s shift 
            else rotorSim tl_r tl_n tl_s (if hd_n < hd_s then 
              (hd_s + shift - hd_n)/26 else (hd_s + shift + 25 - hd_n)/26)
              (getOutputPos pos hd_r hd_s shift) in
  Char.chr (rotorSim (List.rev rotors) (List.rev notchPos)
  (List.rev startPos) idx (Char.code letter - 65) + 65) in

  let mapRefl idx letter = 
    if Char.code letter = 32 then ' ' else 
      String.get refl (Char.code letter - 65) in

  let getOutputPosRev pos r s i = 
    let index = String.index r (Char.chr ((s + i + pos) mod 26 + 65)) in
    let output = 
      if index < (s + i) then index + 26 - (s + i) else index - (s + i) in 
    let print = Printf.printf "%d %d \n" index output in
    output in
  let mapRotorsRev idx letter = 
    let rec rotorSim r n s idx pos = 
      match r,n,s with
      | [],[],[] -> -33
      | hd_r::tl_r, hd_n::tl_n, hd_s::tl_s -> 
          let shift = if tl_r = [] then (idx + 1) else
            let rec calcNotchShift rightRs rightNs rightSs= 
              match rightRs,rightNs,rightSs with 
              | [],[],[] -> (idx + 1)
              | hr::tr, hn::tn, hs::ts ->
                if hn < hs 
                then (hs + (calcNotchShift tr tn ts) - hn)/26
                else (hs + (calcNotchShift tr tn ts) + 25 - hn)/26 in
            calcNotchShift (tl_r) (tl_n) (tl_s) in
          let print2 = 
            Printf.printf "Rotor Shift: %d In Pos: %d Starting Pos: %d \n\n" 
            shift pos hd_s in
          if pos = -33 then -33 else 
            if tl_r = [] then getOutputPosRev pos hd_r hd_s shift 
            else rotorSim tl_r tl_n tl_s idx
              (getOutputPosRev pos hd_r hd_s shift) in
  Char.chr (rotorSim rotors notchPos startPos 
    idx (Char.code letter - 65) + 65) in
  String.mapi mapRotorsRev (String.mapi mapRefl (String.mapi mapRotors s))
