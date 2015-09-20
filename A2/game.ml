#require "yojson"
open Yojson
(*Record for an exit*)
type exit =
  {
    direction     : string;
    room          : string;
  }
(*Record for a room*)
type room =
  {
    id            : string;
    description   : string;
    items         : string list;
    points        : int;
    exits         : exit list;
    treasures     : string list;
  }
(*Record for an item*)
type item =
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
   items        = filter_string (filter_member "items" [r]);
   points       = to_int (member "points" r);
   exits        = parse_exits (flatten (filter_member "exits" [r]));
   treasures    = filter_string (filter_member "treasure" [r]);}
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
(*Function extracts an array of all "room" json objects*)
let extract_rooms (t : Yojson.Basic.json) : room list =
  let open Yojson.Basic.Util in
  [t]
  |> filter_member "rooms"
  |> flatten
  |> parse_rooms

let extract_items (t : Yojson.Basic.json) : item list =
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

let t = Yojson.Basic.from_file "minimal.json"
let g = Yojson.Basic.from_file "gates.json"
let items = extract_items t
let rooms = extract_rooms t
let startRoom = extract_start_room t
let startItems = extract_start_items t

let rec main_game_loop (visited_rooms : string list)
  (item_locations : (string * string) list) (current_room : string)
  (inventory : string list) (points : int) (turns : int)
  (*Read the input*)
  let input = read_line ()