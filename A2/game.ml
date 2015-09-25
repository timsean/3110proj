(*Record for an exit*)
type exit_rec =
  {
    direction     : string;
    room          : string;
  }
(*Record for a room*)
type room_rec =
  {
    id            : string;
    description   : string;
    items         : string list;
    points        : int;
    exits         : exit_rec list;
    treasures     : string list;
  }
(*Record for an item*)
type item_rec=
  {
    id            : string;
    description   : string;
    points        : int;
  }
(*Helper function for parsing an "exit" json object*)
let parse_exit e =
  let open Yojson.Basic.Util in
  {direction    = e |> (member "direction") |> to_string;
   room         = e |> (member "room") |> to_string;}
(*Helper function for parsing an array of "exit" json object*)
let rec parse_exits jsonExits =
  match jsonExits with
  | [] -> []
  | hd::tl -> parse_exit hd :: parse_exits tl
(*Helper function for parsing an item json object*)
let parse_item i =
  let open Yojson.Basic.Util in
  {id           = to_string (member "id" i);
   description  = to_string (member "description" i);
   points       = to_int (member "points" i);}
(*Helper function for parsing a room json object*)
let parse_room r =
  let open Yojson.Basic.Util in
  {id           = to_string (member "id" r);
   description  = to_string (member "description" r);
   items        = filter_string (flatten (filter_member "items" [r]));
   points       = to_int (member "points" r);
   exits        = parse_exits (flatten (filter_member "exits" [r]));
   treasures    = filter_string (flatten (filter_member "treasure" [r]));}
(*Helper function for parsing an array of "item" json object*)
let rec parse_items jsonItems =
  match jsonItems with
  | [] -> []
  | hd::tl -> parse_item hd :: parse_items tl
(*Helper function for parsing an array "room" json object*)
let rec parse_rooms jsonRooms =
  match jsonRooms with
  | [] -> []
  | hd::tl -> parse_room hd :: parse_rooms tl
(*Functions for extracting various json objects*)
let extract_rooms (t : Yojson.Basic.json) : room_rec list =
  let open Yojson.Basic.Util in
  [t]
  |> filter_member "rooms"
  |> flatten
  |> parse_rooms
let extract_items (t : Yojson.Basic.json) : item_rec list =
  let open Yojson.Basic.Util in
  [t]
  |> filter_member "items"
  |> flatten
  |> parse_items
let extract_start_room (t: Yojson.Basic.json) : string =
  let open Yojson.Basic.Util in
  [t]
  |> filter_member "start_room"
  |> List.hd
  |> to_string
let extract_start_items (t: Yojson.Basic.json) : string list =
  let open Yojson.Basic.Util in
  [t]
  |> filter_member "start_items"
  |> flatten
  |> filter_string

let game_file =
  match Array.to_list Sys.argv with
  | prgm::file::[] -> file
  | _ -> Printf.printf "\nPlease give a game file as an argument\n"; exit 0
let game =
  try
    Yojson.Basic.from_file game_file
  with
    | _ -> Printf.printf "Invalid JSON file!\n"; exit 0
let items = extract_items game
let rooms = extract_rooms game
let start_room = extract_start_room game
let start_items = extract_start_items game
(*The max number of points that can be earned*)
let max_points =
  (List.fold_left (fun pts (room:room_rec) -> room.points + pts) 0 rooms) +
  (List.fold_left (fun pts (item:item_rec) -> item.points + pts) 0 items)
(*The list of item locations at the start of the game*)
let start_item_locations =
  List.flatten (List.fold_left
  (fun i_loc_list (room:room_rec) ->
    (List.map (fun item -> (item, room.id)) room.items)::i_loc_list) [] rooms)
(*Helper function for calculate the number of points an item is worth
 *in the room it's in. An item will only be worth its points when it's
 *in the designated room. This function returns the item's points if
 *it's in the designated room and 0 otherwise. It is used by the functions
 *take_item, drop_item, and for evaluating start_points*)
let calc_item_points item_id room_id =
  let room = List.hd
    (List.filter (fun (room:room_rec) ->
    (String.lowercase room.id) = (String.lowercase room_id)) rooms) in
  if List.filter (fun tre ->
    (String.lowercase tre) = (String.lowercase item_id)) room.treasures <> []
  then (List.hd (List.filter
    (fun i ->
      (String.lowercase i.id) = (String.lowercase item_id)) items)).points
  else 0
(*The starting points*)
let start_points =
  (*The points of the starting room*)
  ((List.hd (List.filter
    (fun (room:room_rec) -> room.id = start_room) rooms)).points) +
  (*Points of items already in their designated rooms*)
  (List.fold_left (fun pts (item_loc:string*string) ->
    (calc_item_points (fst item_loc) (snd item_loc)) + pts)
    0 start_item_locations)

let rec main_game_loop (visited_rooms : string list)
  (item_locations : (string * string) list) (current_room_id : string)
  (inventory : string list) (points : int) (turns : int) (new_game : bool)=
  (*Before everything, check to see if the game is completed (max pts = pts)*)
  if points = max_points then (Printf.printf
    "\nYou have completed this quest in %d turns with %d points!\n"
    turns points; exit 0);
  (*Read the input
   *At the start of the game, we force the input to be look, so that the game
   *will display the description of the starting room. It's a bit hacky*)
  let input = if new_game then "look"
    else (Printf.printf "\nCommand: "; String.lowercase (read_line ())) in
  (*Tokenize the string for pattern matching*)
  let input_split = Str.bounded_split (Str.regexp "[ \t]+") input 2 in
  (*Get the room record for the current room*)
  let current_room = List.hd
    (List.filter (fun (room:room_rec) ->
      (String.lowercase room.id) = (String.lowercase current_room_id)) rooms) in
  (*Check whether the room with id = r_id has been visited*)
  let visited r_id =
    if List.filter (fun x -> x = r_id) visited_rooms <> []
    then true else false in
  (*Helper function to return the exit record for a movement direction
   *Returns a list. The list is empty for an invalid direction*)
  let get_exit move_input =
    List.filter (fun x ->
      (String.lowercase x.direction) = move_input) current_room.exits in
  (*Helper function for printing out the item and item description
   *Takes in the id of the item, item_id*)
  let print_item item_id =
    Printf.printf "%s : %s\n" item_id (List.hd
      (List.filter (fun item -> item.id = item_id) items)).description in
  (*Function for printing out the description of a room
   *Given a room_rec room, this function will print out the room description,
   *the list of items in the room, and the description of each item*)
  let print_room (room:room_rec) =
    Printf.printf "----------------------------------------------\n";
    Printf.printf "You are now in %s\n\n" room.id;
    Printf.printf "%s\n\n" room.description;
    (*Get the list of items that are in this room*)
    let items_in_room =
      List.map (fun i_loc -> fst i_loc)
      (List.filter (fun i_loc -> (snd i_loc) = room.id) item_locations) in
    (*Print the item list if there are items in this room*)
    (if items_in_room != []
    then (Printf.printf "The room contains: \n";
      List.iter (fun item -> print_item item) items_in_room)
    else Printf.printf "The room does not contain any items\n");
    Printf.printf "----------------------------------------------\n" in
  (*Function for moving moving to the next room according to the move command
   *Function calls main_game_loop with new states to go to next turn*)
  let goto_room move =
    let exit = get_exit move in
    if exit <> [] then
    (*Create a temporary variable for storing the next room room_rec*)
    let next_room = List.hd (List.filter
      (fun (room:room_rec) -> room.id = (List.hd exit).room) rooms) in
    (*Print out the room description for the next room*)
    print_room next_room;
    (*Call main loop again with the new room*)
    main_game_loop
    (if visited next_room.id
      then visited_rooms else (List.hd exit).room::visited_rooms)
    item_locations
    (List.hd exit).room inventory
    (points +
      (if visited next_room.id then 0 else next_room.points))
    (turns + 1)
    false
    else Printf.printf "%s is not a valid move! \n" move;
      main_game_loop visited_rooms item_locations
        current_room_id inventory points turns false in
  (*Function for picking up an item
   *Function calls main_game_loop with new states to go to next turn*)
  let take_item item_id =
    (*Temp variable for storing the actual name (case accurate) of the item*)
    let item = List.filter (fun id -> (String.lowercase id) = item_id)
      (List.map (fun i_tuple -> fst i_tuple) (List.filter
        (fun i_loc -> snd i_loc = current_room_id) item_locations))in
    if item <> []
    then let item_points = calc_item_points item_id current_room_id in
      Printf.printf "\nYou have taken %s and lost %d points\n"
        (List.hd item) item_points;
      main_game_loop
      visited_rooms
      (*Generate the new list of item locations with the taken item removed*)
      (List.filter (fun it_loc ->
        (String.lowercase (fst it_loc)) <> item_id) item_locations)
      current_room_id
      ((List.hd item)::inventory)
      (points - item_points)
      (turns + 1)
      false
    else Printf.printf "\n%s does not exist in this room.\n" item_id;
      main_game_loop visited_rooms item_locations
        current_room_id inventory points turns false in
  (*Function for dropping an item
   *Function calls main_game_loop with new states to go to next turn*)
  let drop_item item_id =
    let item = List.filter (fun id ->
      (String.lowercase id) = item_id) inventory in
    if item <> []
    then let item_points = calc_item_points item_id current_room_id in
      Printf.printf "\nYou have dropped %s and gained %d points\n"
        (List.hd item) item_points;
      main_game_loop
      visited_rooms
      (*Generate the new list of item locations with the dropped item added*)
      (((List.hd item), current_room_id)::item_locations)
      current_room_id
      (List.filter (fun id -> (String.lowercase id) <> item_id) inventory)
      (points + item_points)
      (turns + 1)
      false
    else Printf.printf "\n%s is not in your inventory.\n" item_id;
      main_game_loop visited_rooms item_locations
        current_room_id inventory points turns false in
  (*Pattern match the input to figure out what the user wants*)
  let decode_input command =
    match command with
    (*quit command*)
    | "quit"::tl -> Printf.printf "\nSuccess is not final, failure is not fatal:
     it is the courage to continue that counts.\n"; exit 0
    (*look command*)
    | "look"::tl -> print_room current_room;
      main_game_loop visited_rooms item_locations
      current_room_id inventory points turns false
    (*score command*)
    | "score"::tl -> Printf.printf "\nYour score: %d\n" points;
      main_game_loop visited_rooms item_locations
      current_room_id inventory points turns false
    (*turns command*)
    | "turns"::tl -> Printf.printf "\nNumber of turns taken: %d\n" turns;
      main_game_loop visited_rooms item_locations
      current_room_id inventory points turns false
    (*inventory command*)
    | hd::tl when hd = "inventory" || hd = "inv" ->
      Printf.printf "\nYour inventory contains:\n";
      List.iter (fun x -> Printf.printf "%s\n" x) inventory;
      main_game_loop visited_rooms item_locations
      current_room_id inventory points turns false
    (*item take command*)
    | hd::tl when hd = "take" && tl <> [] -> take_item (List.hd tl)
    (*item drop command*)
    | hd::tl when hd = "drop" && tl <> [] -> drop_item (List.hd tl)
    (*movement commands (using go)*)
    | hd::tl when hd = "go" && tl <> [] -> goto_room (List.hd tl)
    (*single word movement commands*)
    | hd::[] when get_exit hd <> []-> goto_room hd
    (*unrecognized command*)
    | _ -> Printf.printf "\nSorry I don't understand that\n";
      main_game_loop visited_rooms item_locations
      current_room_id inventory points turns false in
  decode_input input_split in

main_game_loop
  [start_room]
  start_item_locations
  start_room start_items
  start_points
  0
  true