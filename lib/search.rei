/**[get_rests num cuisine loc_x Loc_y range price] returns a list of [num]
   restaurants as a string of a json. The data in the jsons can be seen in
   [to_rest json]*/

let get_rests:
  (~cuisine: list(string)=?, float, float, int, int, list(string)) =>
  Lwt.t(string);

/**[get_winner json id] is a string JSON representing the restaurant with rank
   [id] in the list of restaurants in [json]*/

let get_winner: (int, string) => string;
