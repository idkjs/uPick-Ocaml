/* open Cohttp.Header */
/* open Cohttp */
open Lwt.Infix;
open Yojson.Basic;
open Yojson.Basic.Util;

type result = {
  name: string,
  address: string,
  cuisines: string,
  price: int,
  highlights: list(string),
  rating: float,
  photo: string,
  timing: string,
  phone: string,
  reservation: bool,
  takeout: bool,
} /*Zomato API Key*/;

let user_key = List.assoc("USER_KEY", Dotenv.parse());

let string_of_rest = r =>
  Printf.sprintf(
    {|{
     "name": "%s",
     "address": "%s",
     "cuisines": "%s",
     "price": %d,
     "highlights": ["%s"],
     "rating": %f,
     "photo": "%s",
     "timing": "%s",
     "phone": "%s",
     "reservation": %b,
     "takeout": %b
   }|},
    r.name,
    r.address,
    r.cuisines,
    r.price,
    String.concat("\", \"", r.highlights),
    r.rating,
    r.photo,
    r.timing,
    r.phone,
    r.reservation,
    r.takeout,
  );

/**Convert a list of restaurant JSONs into a string JSON list*/

let string_of_t = t =>
  List.map(string_of_rest, t)
  |> String.concat(",\n")
  |> (l => "{\"restaurants\": [\n" ++ l ++ "\n]}");

/** [let split_str_lst s] is the first string in the comma-separated string
    [s] */

let split_str_lst = s =>
  switch (String.split_on_char(',', s)) {
  | [] => ""
  | [h, ..._] => String.trim(h)
  };

/**[splice n lst] returns the first [n] elements of [lst]
   Requires: [lst] has at least [n] elements.*/

let splice = (n, lst) => {
  let rec aux = (lst, i, acc) =>
    if (i == n) {
      acc;
    } else {
      switch (lst) {
      | [] => failwith("invariant violated")
      | [h, ...t] => aux(t, i + 1, [h, ...acc])
      };
    };
  List.rev(aux(lst, 0, []));
};

let to_result = json => {
  let json = member("restaurant", json);
  {
    name: json |> member("name") |> to_string,
    address: json |> member("location") |> member("address") |> to_string,
    cuisines: json |> member("cuisines") |> to_string,
    price: json |> member("average_cost_for_two") |> to_int |> (p => p / 2),
    highlights:
      json |> member("highlights") |> to_list |> List.map(to_string),
    rating:
      json
      |> member("user_rating")
      |> member("aggregate_rating")
      |> to_string
      |> float_of_string,
    photo: json |> member("photos_url") |> to_string,
    timing: json |> member("timings") |> to_string,
    phone: json |> member("phone_numbers") |> to_string |> split_str_lst,
    reservation:
      json |> member("is_table_reservation_supported") |> to_int |> (!=)(0),
    takeout: json |> member("has_online_delivery") |> to_int |> (==)(1),
  };
};

let from_body = json =>
  try(
    json
    |> member("restaurants")
    |> to_list /*restaurant list*/
    |> List.map(to_result)
  ) {
  | [@implicit_arity] Type_error(s, _) => failwith("Parsing error: " ++ s)
  };

/**Sets a semi-arbitrary upper bound for [price].*/

let set_bound = price =>
  (
    switch (float_of_int(price)) {
    | p when p <= 70. => p *. (1.4 -. (p -. 10.) *. 0.005)
    | p => p
    }
  )
  |> int_of_float;

let filter_results = (price, l) => {
  let filtered = List.filter(x => x.price <= price, l);
  if (List.length(filtered) <= 5) {
    l;
  } else {
    filtered;
  };
};

let compare_op = (x, y) =>
  if (snd(x) > snd(y)) {
    (-1);
  } else if (snd(x) < snd(y)) {
    1;
  } else {
    0;
  };

let rank_highlights = preferences => {
  let rec helper = acc =>
    fun
    | [] => acc
    | [h, ...t] =>
      if (List.mem_assoc(h, acc)) {
        let count = List.assoc(h, acc);
        let new_lst =
          acc |> List.remove_assoc(h) |> List.cons((h, count + 1));
        helper(new_lst, t);
      } else {
        helper(List.cons((h, 1), acc), t);
      };
  let pref = helper([], preferences);
  List.sort(compare_op, pref);
};

let rec highlights_recurser = (acc, sorted_highlights) =>
  fun
  | [] => acc
  | [h, ...t] =>
    if (List.mem_assoc(h, sorted_highlights)) {
      highlights_recurser(
        acc + List.assoc(h, sorted_highlights),
        sorted_highlights,
        t,
      );
    } else {
      highlights_recurser(acc, sorted_highlights, t);
    };

let rec results_recurser = (acc, sorted_highlights) =>
  fun
  | [] => acc
  | [h, ...t] =>
    results_recurser(
      [
        (h, highlights_recurser(0, sorted_highlights, h.highlights)),
        ...acc,
      ],
      sorted_highlights,
      t,
    );

let filter_highlights = (highlights, results) => {
  let sorted_highlights = rank_highlights(highlights);
  if (List.length(results) <= 5) {
    results;
  } else {
    let rest_scores =
      List.rev(results_recurser([], sorted_highlights, results));
    let sorted_rests = List.sort(compare_op, rest_scores);
    sorted_rests |> List.split |> fst |> splice(5);
  };
};

let process_results = (price, pref, inbound) =>
  inbound
  |> from_string
  |> from_body
  |> filter_results(price)
  |> filter_highlights(pref)
  |> string_of_t
  |> (
    x => {
      print_endline(x);
      x;
    }
  );

/**[bind_request header url] is a string of a json that resulting from a get
   request to the Zomato API
   at [url] with [header], a list of key: value pairs that contain metadata
   as well as the API key */

let bind_request = (header, url, price, pref) =>
  Cohttp_lwt_unix.Client.get(~headers=header, Uri.of_string(url))
  >>= (
    a =>
      snd(a)
      |> Cohttp_lwt__.Body.to_string
      >>= (b => b |> process_results(price, pref) |> Lwt.return)
  );

/**[get_rests num cuisine loc_x Loc_y range price] returns a list of [num]
   restaurants as a string of a json. The data in the jsons can be seen in
   [to_result json]

   Requires: [cuisine] must be a list of strings that represent Zomato
   cuisine IDs*/

let get_rests = (~cuisine as c=[], loc_x, loc_y, range, price, pref) => {
  let price = set_bound(price);
  let hdr =
    Cohttp.Header.add_list(
      Cohttp.Header.init(),
      [("Accept", "application/json"), ("user-key", user_key)],
    );
  let url =
    "https://developers.zomato.com/api/v2.1/search?count=20"
    ++ "&lat="
    ++ string_of_float(loc_x)
    ++ "&lon="
    ++ string_of_float(loc_y)
    ++ "&radius="
    ++ string_of_float(float_of_int(range))
    ++ "&cuisines="
    ++ String.concat("%2c", c)
    ++ "&sort=rating&order=desc";
  bind_request(hdr, url, price, pref);
} /*Calculate the winner of a vote given by an id (position in a list)*/;

let to_winner = json => {
  name: json |> member("name") |> to_string,
  address: json |> member("address") |> to_string,
  cuisines: json |> member("cuisines") |> to_string,
  price: json |> member("price") |> to_int,
  highlights: json |> member("highlights") |> to_list |> List.map(to_string),
  rating: json |> member("rating") |> to_float,
  photo: json |> member("photo") |> to_string,
  timing: json |> member("timing") |> to_string,
  phone: json |> member("phone") |> to_string,
  reservation: json |> member("reservation") |> to_bool,
  takeout: json |> member("takeout") |> to_bool,
};

let get_winner = (rank, json_str) =>
  json_str
  |> from_string
  |> member("restaurants")
  |> to_list
  |> (l => List.nth(l, rank) |> to_winner |> string_of_rest);
