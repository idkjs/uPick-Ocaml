open Lwt.Infix;
open Sqlite3;
open Yojson.Basic;
open Yojson.Basic.Util;

let db = db_open("upick.db");

type user = {
  id: int,
  username: string,
  password: string,
  name: string,
  friends: list(int),
  restrictions: list(int),
  groups: list(int),
  visited: list(string),
};

type group = {
  id: int,
  name: string,
  host_id: int,
  members: list(int),
  voting_allowed: bool,
  top_5: option(string),
  top_pick: option(string),
};

type restriction = {
  id: int,
  name: string,
};

exception Not_found;

/**Escapes all single quotes in a json string with an additional single quote
   for SQL compliance*/

let sanitize = sql => Str.global_replace(Str.regexp("'"), "''", sql);

let make_stmt = sql => prepare(db, sql);

/**[single_row_query sql_col sql_tbl sql_where] is an array of strings
   representing the columns [sql_select] in a single row matching [sql_where]*/

let single_row_query =
    (sql_select: string, sql_tbl: string, sql_where: string) => {
  let sql =
    Printf.sprintf(
      {|
  SELECT %s
  FROM %s
  WHERE %s;
  |},
      sql_select,
      sql_tbl,
      sql_where,
    ); /* print_endline sql; */
  print_endline(sql);
  let stmnt = make_stmt(sql);
  ignore(step(stmnt));
  Array.map(Data.to_string_coerce, row_data(stmnt));
};

/**[  sql_col sql_tbl sql_where f] is a list of lists containing
   the values of [sql_col] in [sql_tbl] satisfying [sql_where], converted into
   their primitive types from a string representation with [f]
   Returns: a list of lists of values for a query
   Requires: [sql_col] contains only one column
   [sql_col], [sql_tbl], abd [sql_where] are defined in the schema.*/

let lst_from_col =
    (
      ~unique as u=true,
      ~voting as v=false,
      sql_col: string,
      sql_tbl: string,
      sql_where: string,
      f: string => 'a,
    ) => {
  let arr = ref([||]);
  let sql =
    Printf.sprintf(
      {|
  SELECT %s
  FROM %s
  WHERE %s;
  |},
      sql_col,
      sql_tbl,
      sql_where,
    );
  let stmnt = make_stmt(sql);
  while (step(stmnt) == ROW) {
    let value = row_data(stmnt)[0] |> Data.to_string_coerce |> f;
    arr := Array.append(arr^, [|value|]);
  };
  if (v) {
    Array.to_list(arr^);
  } else if (u) {
    List.sort_uniq(compare, Array.to_list(arr^));
  } else {
    List.sort(compare, Array.to_list(arr^));
  };
};

/**Returns the number of occurrences of rows satisfying [sql_where] in
   [sql_tbl*/

let count = (sql_tbl, sql_where) =>
  single_row_query("COUNT (*)", sql_tbl, sql_where)
  |> (arr => int_of_string(arr[0]));

/**[make_response] returns [Some last_id] if an insertion operation succeeded
   and [None] otherwise.*/

let make_response =
  fun
  | Rc.OK => {
      let id = Sqlite3.last_insert_rowid(db);
      Printf.printf("Row inserted with id %Ld\n", id);
      Some(id);
    }
  | r => {
      prerr_endline(Rc.to_string(r));
      prerr_endline(errmsg(db));
      None;
    };

let delete_sql = (sql_tbl: string, sql_where: string) =>
  Printf.sprintf({|DELETE FROM %s WHERE %s; |}, sql_tbl, sql_where);

let is_host = (str_hid, str_gid) =>
  count("group_info", "host_id = " ++ str_hid ++ " AND rowid = " ++ str_gid)
  > 0;

let is_admin = user_id =>
  count("users", "is_admin = 1 AND rowid = " ++ string_of_int(user_id)) > 0 /*insertion functions */;

let add_user = (username, password, name) =>
  if (name == "" || username == "") {
    None;
  } else {
    let sql =
      Printf.sprintf(
        "INSERT INTO users (username, password, name) VALUES('%s','%s','%s'); ",
        username,
        password,
        name,
      );
    print_endline(username);
    make_response(exec(db, sql));
  };

let update_username = (user_id, username) => {
  let str_uid = string_of_int(user_id);
  if (username != "") {
    let sql =
      "UPDATE users \r\nSET username = '"
      ++ username
      ++ "' \r\nWHERE rowid = "
      ++ str_uid
      ++ ";";
    make_response(exec(db, sql));
  } else {
    None;
  };
};

let update_password = (user_id, password) => {
  let str_uid = string_of_int(user_id);
  let sql =
    "UPDATE users \r\nSET password = '"
    ++ password
    ++ "' \r\nWHERE rowid = "
    ++ str_uid
    ++ ";";
  make_response(exec(db, sql));
};

/**[add_friends friend1 friend2 inserts a pairing of two friends]
   Requires: friend1 is not friend2
   Raises: Invalid_arg*/

let add_friends = (friend1, friend2) =>
  try(
    {
      assert(friend1 != friend2);
      if (count("users", "rowid = " ++ string_of_int(friend2)) > 0) {
        if (friend1 < friend2) {
          let sql =
            Printf.sprintf(
              "INSERT INTO friends VALUES(%d, %d); ",
              friend1,
              friend2,
            );
          make_response(exec(db, sql));
        } else {
          let sql =
            Printf.sprintf(
              "INSERT INTO friends VALUES(%d, %d); ",
              friend2,
              friend1,
            );
          make_response(exec(db, sql));
        };
      } else {
        None;
      };
    }
  ) {
  | e =>
    print_endline(Printexc.to_string(e));
    print_endline("Cannot friend yourself");
    None;
  };

let add_restrictions = (user_id, restriction) => {
  let sql =
    Printf.sprintf(
      "INSERT INTO restrictions VALUES(%d, %d); ",
      user_id,
      restriction,
    );
  make_response(exec(db, sql));
};

let add_rest_pref_helper = (user_id, sql_text) =>
  if (is_admin(user_id)) {
    let sql = sql_text;
    make_response(exec(db, sql));
  } else {
    None;
  };

let add_restrictions_index = (user_id, restriction) => {
  let sql =
    Printf.sprintf(
      "INSERT INTO restriction_index VALUES('%s'); ",
      restriction,
    );
  add_rest_pref_helper(user_id, sql);
};

let add_preferences_index = (user_id, preference) => {
  let sql =
    Printf.sprintf("INSERT INTO preferences VALUES('%s'); ", preference);
  add_rest_pref_helper(user_id, sql);
};

let rm_rest_pref_helper = (user_id, sql_text) =>
  if (is_admin(user_id)) {
    let sql = sql_text;
    make_response(exec(db, sql));
  } else {
    None;
  };

let remove_restrictions_index = (user_id, restriction_id) =>
  if (count("restriction_index", "rowid = " ++ string_of_int(restriction_id))
      > 0) {
    let sql_index =
      delete_sql(
        "restriction_index",
        "rowid = " ++ string_of_int(restriction_id),
      );
    let sql_restr =
      delete_sql(
        "restrictions",
        "restriction = " ++ string_of_int(restriction_id),
      );
    rm_rest_pref_helper(user_id, sql_restr ++ sql_index);
  } else {
    None;
  };

let remove_preferences_index = (user_id, preference_id) =>
  if (count("preference_index", "rowid = " ++ string_of_int(preference_id))
      > 0) {
    let sql =
      delete_sql("preferences", "rowid = " ++ string_of_int(preference_id));
    rm_rest_pref_helper(user_id, sql);
  } else {
    None;
  };

let add_cuisine = (user_id, cuisine_id, cuisine) =>
  if (is_admin(user_id)) {
    let sql =
      Printf.sprintf(
        "INSERT INTO cuisines VALUES(%d, '%s'); ",
        cuisine_id,
        cuisine,
      );
    make_response(exec(db, sql));
  } else {
    None;
  };

let remove_cuisine = (user_id, cuisine_id) =>
  if (is_admin(user_id)
      && count("cuisines", "cuisine_id = " ++ string_of_int(cuisine_id)) > 0) {
    let sql =
      delete_sql("cuisines", "cuisine_id = " ++ string_of_int(cuisine_id));
    make_response(exec(db, sql));
  } else {
    None;
  };

let join_group = (group_id, member_id) =>
  if (count(
        "group_invites",
        "group_id = "
        ++ string_of_int(group_id)
        ++ " AND user_id = "
        ++ string_of_int(member_id),
      )
      > 0) {
    let sql =
      Printf.sprintf(
        {|
  INSERT INTO groups (group_id, member_id) VALUES(%d, %d); |},
        group_id,
        member_id,
      );
    let resp = make_response(exec(db, sql));
    if (resp == None) {
      None;
    } else {
      let update_sql =
        Printf.sprintf(
          {|UPDATE group_info
  SET num_members = num_members + 1
  WHERE rowid = %d; |},
          group_id,
        );
      make_response(exec(db, update_sql));
    };
  } else {
    None;
  };

let add_group_invites = (group_id, user_id, host_id) =>
  if (is_host(string_of_int(host_id), string_of_int(group_id))
      && count("group_info", "rowid = " ++ string_of_int(group_id)) > 0
      && count("users", "rowid = " ++ string_of_int(user_id)) > 0) {
    let sql =
      "INSERT INTO group_invites VALUES ("
      ++ string_of_int(group_id)
      ++ ", "
      ++ string_of_int(user_id)
      ++ "); ";
    make_response(exec(db, sql));
  } else {
    None;
  };

let add_group_info = (group_name, host_id) => {
  let sql =
    Printf.sprintf(
      "INSERT INTO group_info (group_name, host_id) VALUES('%s', %d); ",
      group_name,
      host_id,
    );
  switch (exec(db, sql)) {
  | Rc.OK =>
    let id = Sqlite3.last_insert_rowid(db);
    Printf.printf("Row inserted with id %Ld\n", id);
    ignore(add_group_invites(Int64.to_int(id), host_id, host_id));
    ignore(join_group(Int64.to_int(id), host_id));
    Some(id);
  | r =>
    prerr_endline(Rc.to_string(r));
    prerr_endline(errmsg(db));
    None;
  };
};

let remove_a_user = str_did => {
  let lst =
    lst_from_col("group_id", "groups", "member_id = " ++ str_did, x => x);
  let sql_lst =
    List.map(
      x =>
        "UPDATE group_info \r\n        SET num_members = num_members - 1 \r\n        WHERE rowid = "
        ++ x
        ++ "; ",
      lst,
    );
  List.fold_left((x, y) => x ++ y, "", sql_lst);
};

let delete_user = (user_id, delete_id) => {
  let str_did = string_of_int(delete_id);
  let are_users =
    count("users", "rowid = " ++ string_of_int(user_id)) > 0
    && count("users", "rowid = " ++ str_did) > 0;
  let is_a_host = count("group_info", "host_id = " ++ str_did) > 0;
  if (are_users
      && (user_id == delete_id || is_admin(user_id))
      && is_admin(delete_id) == false
      && is_a_host == false) {
    let user = delete_sql("users", "rowid = " ++ str_did);
    let groups = delete_sql("groups", "member_id = " ++ str_did);
    let group_invites = delete_sql("group_invites", "user_id = " ++ str_did);
    let votes = delete_sql("votes", "user_id = " ++ str_did);
    let restrictions = delete_sql("restrictions", "user_id = " ++ str_did);
    let group_members_update = remove_a_user(str_did);
    let friends =
      delete_sql(
        "friends",
        "friend_1 = " ++ str_did ++ " OR friend_2 = " ++ str_did,
      );
    make_response(
      exec(
        db,
        user
        ++ groups
        ++ group_invites
        ++ votes
        ++ restrictions
        ++ friends
        ++ group_members_update,
      ),
    );
  } else {
    None;
  };
};

let delete_group = (user_id, group_id) => {
  let str_uid = string_of_int(user_id);
  let str_gid = string_of_int(group_id);
  let is_user = count("users", "rowid = " ++ str_uid) > 0;
  let is_group = count("group_info", "rowid = " ++ str_gid) > 0;
  let is_host_or_admin = is_host(str_uid, str_gid) || is_admin(user_id);
  if (is_user && is_group && is_host_or_admin) {
    let group_info = delete_sql("group_info", "rowid = " ++ str_gid);
    let groups = delete_sql("groups", "group_id = " ++ str_gid);
    let group_invites = delete_sql("group_invites", "group_id = " ++ str_gid);
    let votes = delete_sql("votes", "group_id = " ++ str_gid);
    make_response(exec(db, votes ++ group_invites ++ groups ++ group_info));
  } else {
    None;
  };
} /* if searches appear off maybe we need hostid? */;

let num_rests = str_gid => {
  let json_str =
    List.hd(
      lst_from_col("top_5", "group_info", "rowid = " ++ str_gid, x => x),
    );
  let json = from_string(json_str);
  json |> member("restaurants") |> to_list |> List.length;
};

let rec add_user_votes = (group_id, user_id, count, acc, lst) =>
  switch (lst) {
  | [] => acc
  | [hd, ...tl] =>
    let sql =
      Printf.sprintf(
        "INSERT INTO votes VALUES(%d, %d, %d, %d); ",
        group_id,
        user_id,
        count,
        hd,
      );
    add_user_votes(group_id, user_id, count + 1, acc ++ sql, tl);
  };

/**Inserts a [user_id]'s votes in [group_id] with [ballot] representing
   the positions of restaurants in the list ranked in order*/

let add_votes = (group_id, user_id, ballot) => {
  let str_gid = string_of_int(group_id);
  let str_uid = string_of_int(user_id);
  if (count("group_info", "voting_allowed = 1 AND rowid = " ++ str_gid) == 1) {
    let rest_size = num_rests(str_gid);
    let rec valid_ballot = (counter, lst) =>
      if (counter == 0) {
        true;
      } else if (List.mem(counter - 1, lst)) {
        valid_ballot(counter - 1, lst);
      } else {
        false;
      };
    if (List.length(ballot) == rest_size && valid_ballot(rest_size, ballot)) {
      let new_votes = add_user_votes(group_id, user_id, 1, "", ballot);
      if (count(
            "votes",
            "group_id = " ++ str_gid ++ " AND user_id = " ++ str_uid,
          )
          > 0) {
        let drop_sql =
          delete_sql(
            "votes",
            "group_id = " ++ str_gid ++ " AND user_id = " ++ str_uid,
          );
        make_response(exec(db, drop_sql ++ new_votes));
      } else {
        make_response(exec(db, new_votes));
      };
    } else {
      None;
    };
  } else {
    None;
  };
};

let login = username =>
  try(
    Some(
      single_row_query("password", "users", "username = '" ++ username ++ "'")[0],
    )
  ) {
  | e =>
    ignore(e);
    None;
  };

/**[id_by_usr usr] is the id of the user with unique username [usr]*/

let id_by_usr = usr =>
  single_row_query("rowid", "users", "username = '" ++ usr ++ "'")[0]
  |> int_of_string;

let get_friends = u_id => {
  let friends1 =
    lst_from_col(
      "friend_2",
      "friends",
      "friend_1 = " ++ string_of_int(u_id),
      int_of_string,
    );
  let friends2 =
    lst_from_col(
      "friend_1",
      "friends",
      "friend_2 = " ++ string_of_int(u_id),
      int_of_string,
    );
  List.sort_uniq(compare, friends1 @ friends2);
};

/** [get_user userid] returns a representation of a single user from the
    database in type user.
    Requires: A valid userid is inputted, valid [username], [password]
    inputted, valid [name] inputted, valid [friends] inputted, [restricitons]
    inputted, [groups] inputted definined in the same user */

let get_user = user_id =>
  try({
    let arr1 =
      single_row_query(
        "username, password, name",
        "users",
        "rowid = " ++ string_of_int(user_id),
      );
    let friends = get_friends(user_id);
    let restrictions =
      lst_from_col(
        "restriction",
        "restrictions",
        "user_id = " ++ string_of_int(user_id),
        int_of_string,
      );
    let groups =
      lst_from_col(
        "group_id",
        "groups",
        "member_id = " ++ string_of_int(user_id),
        int_of_string,
      );
    let visited =
      lst_from_col(
        "restaurant",
        "visited_restaurants",
        "user_id = " ++ string_of_int(user_id),
        x =>
        x
      );
    {
      id: user_id,
      username: arr1[0],
      password: arr1[1],
      name: arr1[2],
      friends,
      restrictions,
      groups,
      visited,
    };
  }) {
  | _ => raise(Not_found)
  };

let get_group = group_id =>
  try({
    let arr1 =
      single_row_query(
        "group_name, host_id, voting_allowed, top_5, top_pick",
        "group_info",
        "rowid = " ++ string_of_int(group_id),
      );
    let mem_lst =
      lst_from_col(
        "member_id",
        "groups",
        "group_id = " ++ string_of_int(group_id),
        int_of_string,
      );
    {
      id: group_id,
      name: arr1[0],
      host_id: arr1[1] |> int_of_string,
      voting_allowed: arr1[2] == "1",
      members: mem_lst,
      top_5:
        if (arr1[3] == "") {
          None;
        } else {
          Some(arr1[3]);
        },
      top_pick:
        if (arr1[4] == "") {
          None;
        } else {
          Some(arr1[4]);
        },
    };
  }) {
  | _ => raise(Not_found)
  };

let get_restrictions = () =>
  lst_from_col(~voting=true, "restriction", "restriction_index", "1 = 1", x =>
    x
  );

let get_restriction_by_id = rest_id =>
  try({
    let rest =
      single_row_query(
        "restriction",
        "restriction_index",
        "rowid = " ++ string_of_int(rest_id),
      );
    rest[0];
  }) {
  | _ => raise(Not_found)
  };

let get_preferences = () =>
  lst_from_col(~voting=true, "preference", "preferences", "1 = 1", x => x);

let get_preference_by_id = pref_id =>
  try({
    let pref =
      single_row_query(
        "preference",
        "preferences",
        "rowid = " ++ string_of_int(pref_id),
      );
    pref[0];
  }) {
  | _ => raise(Not_found)
  };

let get_cuisines = () => {
  let cuisine_id_lst =
    lst_from_col(~voting=true, "cuisine_id", "cuisines", "1 = 1", x =>
      int_of_string(x)
    );
  let cuisine_lst =
    lst_from_col(~voting=true, "cuisine", "cuisines", "1 = 1", x => x);
  (cuisine_id_lst, cuisine_lst);
};

let get_cuisine_by_id = cuisine_id =>
  try({
    let cuisine =
      single_row_query(
        "cuisine",
        "cuisines",
        "cuisine_id = " ++ string_of_int(cuisine_id),
      );
    cuisine[0];
  }) {
  | _ => raise(Not_found)
  } /* let get_visited_restaurants user_id request_user_id =
   let str_uid = string_of_int user_id in
   let str_ruid = string_of_int request_user_id in
   let count_int = if user_id < request_user_id then count "friends"
   ("friend_1 = " ^ str_uid ^ " AND friend_2 = " ^ str_ruid) else
   count "friends" ("friend_1 = " ^ str_ruid ^ " AND friend_2 = " ^ str_uid) in
   if count_int > 0 || user_id = request_user_id then
   lst_from_col "restaurant" "visited_restaurants" ("user_id = " ^ str_ruid)
   (fun x -> x)
   else [] */;

let deletion_updates = (str_gid, str_uid) => {
  let sql_grouprm =
    delete_sql(
      "groups",
      "group_id = " ++ str_gid ++ " AND member_id = " ++ str_uid,
    );
  let update_sql =
    Printf.sprintf(
      {|
    UPDATE group_info
    SET num_members = num_members - 1
    WHERE rowid = %d; |},
      int_of_string(str_gid),
    );
  let voting_updated =
    delete_sql(
      "votes",
      "group_id = " ++ str_gid ++ " AND user_id = " ++ str_uid,
    );
  voting_updated ++ sql_grouprm ++ update_sql;
};

let delete_from_group = (group_id, member_id, host_id) => {
  let str_gid = string_of_int(group_id);
  let str_hid = string_of_int(host_id);
  let str_uid = string_of_int(member_id);
  let num_members = count("groups", "group_id = " ++ str_gid);
  let is_host_bool = is_host(str_hid, str_gid);
  let mem_not_host = member_id != host_id;
  let is_group_mem =
    count(
      "groups",
      "member_id = " ++ str_uid ++ " AND group_id = " ++ str_gid,
    )
    > 0;
  if (is_group_mem
      && (
        is_host_bool
        && (mem_not_host || num_members == 1)
        || is_host_bool == false
        && mem_not_host == false
      )) {
    let response = deletion_updates(str_gid, str_uid);
    if (is_host_bool) {
      let sql_invite =
        delete_sql(
          "group_invites",
          "group_id = " ++ str_gid ++ " AND user_id = " ++ str_uid,
        );

      print_endline(response ++ sql_invite);
      make_response(exec(db, response ++ sql_invite));
    } else {
      make_response(exec(db, response));
    };
  } else {
    None;
  };
};

let reassign_host = (group_id, user_id, host_id) => {
  let str_gid = string_of_int(group_id);
  let str_hid = string_of_int(host_id);
  let str_uid = string_of_int(host_id);
  let is_group_mem =
    count(
      "groups",
      "member_id = " ++ str_uid ++ " AND group_id = " ++ str_gid,
    )
    > 0;
  if (is_host(str_hid, str_gid) && is_group_mem) {
    let update_sql =
      Printf.sprintf(
        {|
    UPDATE group_info
    SET host_id = %d
    WHERE rowid = %d; |},
        user_id,
        group_id,
      );
    make_response(exec(db, update_sql));
  } else {
    None;
  };
};

let ans_survey =
    (user_id, group_id, loc_x, loc_y, cuisine, price, range, preferences) => {
  let sql =
    Printf.sprintf(
      {|
  UPDATE groups
  SET loc_x = %f, loc_y = %f, target_price = %d,
  cuisines = '%s', range = %d, preferences = '%s', surveyed = 1
  WHERE member_id = %d AND group_id = %d; |},
      loc_x,
      loc_y,
      price,
      cuisine,
      range,
      preferences,
      user_id,
      group_id,
    );
  make_response(exec(db, sql));
};

let avg_flt = (col, n, g_id) => {
  let n = float_of_int(n);
  let g = string_of_int(g_id);
  lst_from_col(
    ~unique=false,
    col,
    "groups",
    "group_id = " ++ g,
    float_of_string,
  )
  |> List.fold_left((+.), 0.)
  |> (
    x =>
      x
      /. n
      |> string_of_float
      |> (
        x =>
          if (String.length(x) > 7) {
            float_of_string(String.sub(x, 0, 6));
          } else {
            float_of_string(x);
          }
      )
  );
};

let avg_int = (col, n, g_id) => {
  let g = string_of_int(g_id);
  lst_from_col(
    ~unique=false,
    col,
    "groups",
    "group_id = " ++ g,
    int_of_string,
  )
  |> List.fold_left((+), 0)
  |> (x => x / n);
};

let ranks = str_gid => {
  let rank_lst =
    lst_from_col(
      ~voting=true,
      "ranking",
      "votes",
      "group_id = " ++ str_gid,
      int_of_string,
    );
  let rest_lst =
    lst_from_col(
      ~voting=true,
      "restaurant_id",
      "votes",
      "group_id = " ++ str_gid,
      int_of_string,
    );
  let matched_ranks = List.combine(rest_lst, rank_lst);
  let rec ranked_lst = acc =>
    fun
    | [] => acc
    | [(rest, rank), ...t] =>
      if (List.mem_assoc(rest, acc)) {
        let current_vote = rank + List.assoc(rest, acc);
        let new_acc =
          acc |> List.remove_assoc(rest) |> List.cons((rest, current_vote));
        ranked_lst(new_acc, t);
      } else {
        let new_acc = [(rest, rank), ...acc];
        ranked_lst(new_acc, t);
      };
  ranked_lst([], matched_ranks);
};

let empty_survey_votes = str_gid => {
  let votes = delete_sql("votes", "group_id = " ++ str_gid);
  let updated_surveys =
    "UPDATE groups SET loc_x = NULL, loc_y = NULL, \r\n  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, \r\n  surveyed = 0 WHERE group_id = "
    ++ str_gid
    ++ "; ";
  votes ++ updated_surveys;
};

let visited_entries = (str_gid, top) => {
  let member_lst =
    lst_from_col("member_id", "groups", "group_id = " ++ str_gid, x => x);
  let sql_lst =
    List.map(
      x =>
        "INSERT INTO visited_restaurants (user_id, restaurant) \r\nVALUES("
        ++ x
        ++ ", '"
        ++ top
        ++ "'); ",
      member_lst,
    );
  List.fold_right((x, y) => x ++ y, sql_lst, "");
};

let calculate_votes = (g_id, h_id) => {
  let str_gid = string_of_int(g_id);
  let str_hid = string_of_int(h_id);
  if (is_host(str_hid, str_gid)
      && count("groups", "surveyed = 1 AND member_id = " ++ str_hid) > 0) {
    let ranks_final = ranks(str_gid);
    let compare_op = (x, y) =>
      if (snd(x) > snd(y)) {
        1;
      } else if (snd(x) < snd(y)) {
        (-1);
      } else {
        0;
      };
    let ordered_ranks = List.sort(compare_op, ranks_final);
    let top_pick = fst(List.hd(ordered_ranks));
    let top =
      single_row_query("top_5", "group_info", "rowid = " ++ str_gid)
      |> (row => row[0] |> sanitize |> Search.get_winner(top_pick));
    let sql =
      Printf.sprintf(
        {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |},
        sanitize(top),
        g_id,
      );
    let sql_visited = visited_entries(str_gid, sanitize(top));
    let empty_tables = empty_survey_votes(str_gid);
    make_response(exec(db, sql ++ sql_visited ++ empty_tables));
  } else {
    None;
  };
};

let format_cuisines = group_id =>
  lst_from_col(
    "cuisines", "groups", "group_id = " ++ string_of_int(group_id), x =>
    x
  )
  |> (
    l =>
      List.fold_right((x, y) => x ++ "," ++ y, l, "")
      |> String.split_on_char(',')
      |> List.filter(s => s != "")
  );

let gather_restrictions = g_id => {
  let mems =
    lst_from_col(
      "member_id", "groups", "group_id = " ++ string_of_int(g_id), x =>
      x
    );
  let rec restr_lst = acc =>
    fun
    | [] => acc
    | [h, ...t] =>
      restr_lst(
        [
          lst_from_col("restriction", "restrictions", "user_id = " ++ h, x =>
            x
          ),
          ...acc,
        ],
        t,
      );
  let restr = List.flatten(restr_lst([], mems));
  let f = x =>
    lst_from_col("restriction", "restriction_index", "rowid = " ++ x, x => x);
  List.flatten(List.map(f, restr));
};

let calculate_survey = (cuisines, x, y, range, price, g_id) => {
  let pref =
    lst_from_col(
      ~unique=false,
      "preferences",
      "groups",
      "group_id = " ++ string_of_int(g_id),
      x =>
      x
    );
  let pref_str = List.fold_left((x, y) => x ++ "," ++ y, "", pref);
  let pref_list = String.split_on_char(',', pref_str);
  let pref_restr = pref_list @ gather_restrictions(g_id);
  Search.get_rests(~cuisine=cuisines, x, y, range, price, pref_restr)
  >>= (
    res => {
      let sql =
        Printf.sprintf(
          {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1
                 WHERE rowid = %d;|},
          sanitize(res),
          g_id,
        );
      Lwt.return(make_response(exec(db, sql)));
    }
  );
};

let adjust_group = str_gid => {
  let count_drop = count("groups", "surveyed = 0 AND group_id = " ++ str_gid);
  let to_drop =
    delete_sql("groups", "surveyed = 0 AND group_id = " ++ str_gid);
  ignore(make_response(exec(db, to_drop)));
  let update_num_members =
    Printf.sprintf(
      {|UPDATE group_info SET num_members = num_members - %d
          WHERE rowid = %d; |},
      count_drop,
      int_of_string(str_gid),
    );
  ignore(make_response(exec(db, update_num_members)));
};

let process_survey = (g_id, h_id) => {
  let str_gid = string_of_int(g_id);
  let str_hid = string_of_int(h_id);
  if (is_host(str_hid, str_gid)
      && count(
           "groups",
           "surveyed = 1 AND member_id = "
           ++ str_hid
           ++ " AND group_id = "
           ++ str_gid,
         )
      > 0) /*Ensure that the user making the request is the host of the group*/ {
    adjust_group(str_gid);
    let num_votes = count("groups", "group_id = " ++ str_gid);
    let x = avg_flt("loc_x", num_votes, g_id);
    let y = avg_flt("loc_y", num_votes, g_id);
    let price = avg_int("target_price", num_votes, g_id);
    let range = avg_int("range", num_votes, g_id);
    let cuisines = format_cuisines(g_id);
    ignore(calculate_survey(cuisines, x, y, range, price, g_id));
    Some(Int64.zero);
  } else {
    None;
  };
};

let add_feedback = (rating, comments) => {
  let str_rating = string_of_float(rating);
  if (comments == "") {
    let sql = "INSERT INTO feedback (rating) VALUES(" ++ str_rating ++ "); ";
    make_response(exec(db, sql));
  } else {
    let sql =
      "INSERT INTO feedback VALUES("
      ++ str_rating
      ++ ", '"
      ++ comments
      ++ "'); ";
    make_response(exec(db, sql));
  };
};

let top_visited = () => {
  let arr = ref([||]);
  let sql = "SELECT restaurant, COUNT(*) AS total \r\n  FROM visited_restaurants \r\n  GROUP BY restaurant \r\n  ORDER BY total DESC LIMIT 5; ";
  let stmnt = make_stmt(sql);
  while (step(stmnt) == ROW) {
    let value = row_data(stmnt)[0] |> Data.to_string_coerce;
    arr := Array.append(arr^, [|value|]);
  };
  Array.to_list(arr^);
};

let create_tables = () => Db.create_tables();
