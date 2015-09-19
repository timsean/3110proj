#require "yojson"
open Yojson

type exit =
  {
    direction     : string;
    room          : string;
  }

type room =
  {
    id            : string;
    description   : string;
    items         : string list;
    points        : int;
    exits         : exit list;
    treasures     : string list;
  }

type item =
  {
    id            : string;
    description   : string;
    points        : int;
  }

let parse_exit e =
  let open Yojson.Basic.Util in
  {direction    = e |> (member "direction") |> to_string;
   room         = e |> (member "room") |> to_string;}

let rec parse_exits jsonExits =
  match jsonExits with
  | [] -> []
  | hd::tl -> parse_exit hd :: parse_exits tl

let parse_item i =
  let open Yojson.Basic.Util in
  {id           = to_string (member "id" i);
   description  = to_string (member "description" i);
   points       = to_int (member "points" i);}

let parse_room r =
  let open Yojson.Basic.Util in
  {id           = to_string (member "id" r);
   description  = to_string (member "description" r);
   items        = filter_string (filter_member "items" [r]);
   points       = to_int (member "points" r);
   exits        = parse_exits (flatten (filter_member "exits" [r]));
   treasures    = filter_string (filter_member "treasure" [r]);}

let rec parse_items jsonItems =
  match jsonItems with
  | [] -> []
  | hd::tl -> parse_item hd :: parse_items tl

let rec parse_rooms jsonRooms =
  match jsonRooms with
  | [] -> []
  | hd::tl -> parse_room hd :: parse_rooms tl

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