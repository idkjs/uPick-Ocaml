open Dbquery;
open Lwt.Infix;
open Opium;
open Yojson.Basic;
open Yojson.Basic.Util;

/**Creates a server module with database query module M1 and database module
   M2 with names "upick.db"*/ /**************************JSON builders and parsers***************************/ /* module type ServerMaker = functor (M1 : Dbquery) -> functor (M2 : Db) ->  */;

exception Login_failure(string);
exception Password_failure(string);

let login = json => {
  let pw = member("password", json) |> to_string;
  let stor_pw = member("username", json) |> to_string |> login;
  switch (stor_pw) {
  | None => raise(Login_failure(", Invalid username or password"))
  | Some(p) =>
    if (Bcrypt.verify(pw, Bcrypt.hash_of_string(p))) {
      ();
    } else {
      raise(Login_failure(", Invalid username or password"));
    }
  };
};

let make_response =
  fun
  | Some(id) =>
    Lwt.return(
      Ezjsonm.from_string(
        {|{"success": true, "id": |} ++ Int64.to_string(id) ++ "}",
      )
      |> Json_repr.to_yojson
      |> Response.of_json,
    )
  | None =>
    Lwt.return(
      Ezjsonm.from_string({|{"success": false }|})
      |> Json_repr.to_yojson
      |> Response.of_json,
    );

let load_json = (req, ins_func, inserter) =>
  req.Request.body
  |> Body.to_string
  >>= (
    a =>
      try(Lwt.return(inserter(from_string(a), ins_func))) {
      | Password_failure(_) => Lwt.return(None)
      }
  );

/**[load_json req ins_func inserter] inserts the contents of load_json
   into the database*/

let load_json_login = (req, ins_func, inserter) =>
  req.Request.body
  |> Body.to_string
  >>= (
    a =>
      try({
        let json = from_string(a);
        login(json);
        Lwt.return(inserter(from_string(a), ins_func))
        >>= (a => make_response(a));
      }) {
      | Login_failure(_) =>
        (
          x => {
            print_endline("login credentials did not match");
            x;
          }
        )(
          Lwt.return,
          None,
        )
        >>= (a => make_response(a))
      }
  );

let json_of_user =
    ({id, username, password, name, friends, restrictions, groups, visited}) => {
  ignore(password) /*Do not return the user's password on a get request*/;
  Ezjsonm.(
    dict([
      ("id", int(id)),
      ("username", string(username)),
      ("name", string(name)),
      ("friends", list(int, friends)),
      ("restrictions", list(int, restrictions)),
      ("groups", list(int, groups)),
      ("visited", list(from_string, visited)),
    ])
  );
};

let json_of_group =
    ({id, name, host_id, members, voting_allowed, top_5, top_pick}) =>
  Ezjsonm.(
    dict([
      ("id", int(id)),
      ("name", string(name)),
      ("host_id", int(host_id)),
      ("members", list(int, members)),
      ("voting_allowed", bool(voting_allowed)),
      ("top_5", option(string, top_5)),
      ("top_pick", option(string, top_pick)),
    ])
  );

/**Checks to see if [u_id] is in [g_id*/

let is_member = (g_id, u_id) =>
  List.mem(g_id, get_user(u_id).groups) /*****************************database inserters*******************************/;

/**[user_inserter json ins_func] inserts the data representing a user into
   the database*/

let user_inserter = (json, ins_func) => {
  let raw_pw = member("password", json) |> to_string;
  try(
    {
      assert(Passwords.is_valid(raw_pw));
      ins_func(
        member("username", json) |> to_string,
        raw_pw |> Bcrypt.hash |> Bcrypt.string_of_hash,
        member("name", json) |> to_string,
      );
    }
  ) {
  | _ => raise(Password_failure("invalid password"))
  };
};

let username_update_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  ins_func(u_id, member("new_username", json) |> to_string);
};

let password_update_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  let raw_pw = member("new_password", json) |> to_string;
  try(
    {
      assert(Passwords.is_valid(raw_pw));
      ins_func(u_id, raw_pw |> Bcrypt.hash |> Bcrypt.string_of_hash);
    }
  ) {
  | _ => raise(Password_failure("invalid password"))
  };
};

let user_delete_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  ins_func(u_id, member("user_id", json) |> to_int);
};

let friend_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  ins_func(u_id, member("friend", json) |> to_int);
};

let rest_inserter = (json, ins_func) =>
  ins_func(
    member("user_id", json) |> to_int,
    member("restriction_id", json) |> to_int,
  );

let rest_indx_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  ins_func(u_id, member("restriction", json) |> to_string);
};

let rm_rest_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  ins_func(u_id, member("restriction_id", json) |> to_int);
};

let pref_indx_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  ins_func(u_id, member("preference", json) |> to_string);
};

let rm_pref_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  ins_func(u_id, member("preference_id", json) |> to_int);
};

let add_cuisine_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  ins_func(
    u_id,
    member("cuisine_id", json) |> to_int,
    member("cuisine", json) |> to_string,
  );
};

let rm_cuisine_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  ins_func(u_id, member("cuisine_id", json) |> to_int);
};

let group_info_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  ins_func(member("group_name", json) |> to_string, u_id);
};

let group_inserter = (json, ins_func) =>
  ins_func(
    member("group_id", json) |> to_int,
    member("username", json) |> to_string |> id_by_usr,
  );

let delete_group_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  ins_func(u_id, member("group_id", json) |> to_int);
};

let group_host_user_inserter = (json, ins_func) => {
  let h_id = id_by_usr(member("username", json) |> to_string);
  ins_func(
    member("group_id", json) |> to_int,
    member("user_id", json) |> to_int,
    h_id,
  );
};

let feedback_inserter = (json, ins_func) =>
  ins_func(
    member("rating", json) |> to_float,
    member("comments", json) |> to_string,
  );

let survey_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  if (is_member(member("group_id", json) |> to_int, u_id)) {
    ins_func(
      u_id,
      member("group_id", json) |> to_int,
      member("loc_x", json) |> to_float,
      member("loc_y", json) |> to_float,
      member("cuisines", json)
      |> to_list
      |> List.map(to_string)
      |> String.concat(","),
      member("price", json) |> to_int,
      member("range", json) |> to_int,
      member("preferences", json)
      |> to_list
      |> List.map(to_string)
      |> String.concat(","),
    );
  } else {
    (
      x => {
        print_endline("not a member");
        x;
      }
    )(None);
  };
};

let vote_status_inserter = (json, ins_func) => {
  let h_id = id_by_usr(member("username", json) |> to_string);
  ins_func(member("group_id", json) |> to_int, h_id);
};

let vote_inserter = (json, ins_func) => {
  let u_id = id_by_usr(member("username", json) |> to_string);
  if (is_member(member("group_id", json) |> to_int, u_id)) {
    ins_func(
      member("group_id", json) |> to_int,
      member("username", json) |> to_string |> id_by_usr,
      member("votes", json) |> to_list |> List.map(to_int),
    );
  } else {
    (
      x => {
        print_endline("was not in group");
        x;
      }
    )(None);
  };
} /* Route not found */ /*******************************route list*************************************/;

// let default_ex =
//   App.not_found(_req =>{
//    let body = Body.of_string("No route found\n");
//         Lwt.return(Response.make(~status=`Not_found, ~body, ()));
//   });
// let default =
//   App.not_found(_req =>
//     Ezjsonm.(dict([("message", string("Route not found"))]))|>toYojson|>toResponse    |> Lwt.return
//   );
// let success_false_ezjsonm = Ezjsonm.from_string({|{"success": false}|});
// let success_false_yojson = Json_repr.to_yojson(success_false_ezjsonm);
let toYojson = x => Json_repr.to_yojson(x);
let toResponse = x => Response.of_json(x);

let ezjsonm = Ezjsonm.from_string({|{"success": false}|});
let status = Json_repr.to_yojson(ezjsonm);
let get_list = [
  /* user */
  App.get("/users/:id", req =>
    try({
      let user = Dbquery.get_user(int_of_string(Router.param(req, "id")));
      let ezjsonm = user |> json_of_user;
      // let ezjsonm = `Json(user |> json_of_user);
      let yojson = Json_repr.to_yojson(ezjsonm);

      Response.of_json(~status=`Not_found, yojson) |> Lwt.return;
    }) {
    | e =>
      ignore(e);

      Lwt.return(Response.of_json(status));
    }
  ) /* groups */,
  App.get("/groups/:id", req =>
    try({
      let group = Dbquery.get_group(int_of_string(Router.param(req, "id")));

      let ezjsonm = group |> json_of_group;
      // let ezjsonm = `Json(user |> json_of_user);
      let yojson = Json_repr.to_yojson(ezjsonm);
      // `Json(group |> json_of_group) |> Lwt.return;
      Response.of_json(~status=`Code(200), yojson) |> Lwt.return;
    }) {
    | e =>
      ignore(e);

      Lwt.return(Response.of_json(status));
    }
  ) /* get restriction by id */,
  App.get("/restrictions/:id", req =>
    try({
      let restriction =
        Dbquery.get_restriction_by_id(
          int_of_string(Router.param(req, "id")),
        );
      let ezjsonm =
        Ezjsonm.from_string(
          Printf.sprintf({|{"success": true, "data": "%s"}|}, restriction),
        );
      let yojson = Json_repr.to_yojson(ezjsonm);

      Response.of_json(yojson) |> Lwt.return;
    }) {
    | e =>
      ignore(e);

      Lwt.return(Response.of_json(status));
    }
  ) /* get all restrictions */,
  App.get("/restrictions", _ => {
    let restriction = Dbquery.get_restrictions();

    let ezjsonm = Ezjsonm.list(Ezjsonm.string, restriction);
    let yojson = Json_repr.to_yojson(ezjsonm);

    Response.of_json(yojson) |> Lwt.return;
    // `Json(Ezjsonm.list(Ezjsonm.string, restriction)) |> Lwt.return;
  }),
  App.get("/preferences/:id", req =>
    try({
      let preference =
        Dbquery.get_preference_by_id(
          int_of_string(Router.param(req, "id")),
        );
      // `Json(
      //   Ezjsonm.from_string(
      //     Printf.sprintf({|{"success": true, "data": "%s"}|}, preference),
      //   ),
      // )
      // |> Lwt.return;
      let ezjsonm =
        Ezjsonm.from_string(
          Printf.sprintf({|{"success": true, "data": "%s"}|}, preference),
        );
      let yojson = Json_repr.to_yojson(ezjsonm);

      Response.of_json(yojson) |> Lwt.return;
    }) {
    | e =>
      ignore(e);

      Lwt.return(Response.of_json(status));
    }
  ),
  App.get("/preferences", _ => {
    let preference = Dbquery.get_preferences();
    let ezjsonm = Ezjsonm.list(Ezjsonm.string, preference);
    let yojson = Json_repr.to_yojson(ezjsonm);

    Response.of_json(yojson) |> Lwt.return;
  }),
  App.get("/cuisines/:id", req =>
    try({
      let cuisine =
        Dbquery.get_cuisine_by_id(int_of_string(Router.param(req, "id")));
      // `Json(
      //   Ezjsonm.from_string(
      //     Printf.sprintf({|{"success": true, "data": "%s"}|}, cuisine),
      //   ),
      // )
      // |> Lwt.return;
      let ezjsonm =
        Ezjsonm.from_string(
          Printf.sprintf({|{"success": true, "data": "%s"}|}, cuisine),
        );
      let yojson = Json_repr.to_yojson(ezjsonm);

      Response.of_json(yojson) |> Lwt.return;
    }) {
    | e =>
      ignore(e);

      Lwt.return(Response.of_json(status));
    }
  ),
  App.get("/cuisines", _ => {
    let (cuisine_id, cuisine_str) = Dbquery.get_cuisines();
    let cuisine_id_lst = List.map(x => string_of_int(x), cuisine_id);
    let cuisine_str_lst = List.map(x => Ezjsonm.string(x), cuisine_str);
    let ezjsonm =
      Ezjsonm.dict(List.combine(cuisine_id_lst, cuisine_str_lst));
    let yojson = Json_repr.to_yojson(ezjsonm);

    Response.of_json(yojson) |> Lwt.return;
    // `Json(Ezjsonm.dict(List.combine(cuisine_id_lst, cuisine_str_lst)))
    // |> Lwt.return;
  }),
  App.get("/topvisits", _ => {
    let top_rests = Dbquery.top_visited();
    // `Json(Ezjsonm.list(Ezjsonm.from_string, top_rests)) |> Lwt.return;
    let ezjsonm = Ezjsonm.list(Ezjsonm.from_string, top_rests);
    let yojson = Json_repr.to_yojson(ezjsonm);
    Response.of_json(yojson) |> Lwt.return;
  }),
];
let default = _req => {
  let headers = Httpaf.Headers.of_list([("Content-Type", "text/plain")]);
  let body = Body.of_string("No route found\n");
  // Lwt.return(Response.make(~headers, ~body, ()));
  // Lwt.return @@ Response.make(~headers, ~body, ());
  Response.make(~headers, ~body, ());
};
let post_list = [
  App.post("/feedback", req =>
    load_json_login(req, add_feedback, feedback_inserter)
  ) /* let insert_user =  */, // |> Json_repr.to_yojson
  // |> Response.of_json
  // |> Lwt.return
  App.post("/users", req =>
    load_json(req, add_user, user_inserter) >>= (a => make_response(a))
  ),
  App.post("/users/newusername", req =>
    load_json_login(req, update_username, username_update_inserter)
  ),
  App.post("/users/newpassword", req =>
    load_json_login(req, update_password, password_update_inserter)
  ),
  App.post("/users/delete", req =>
    load_json_login(req, delete_user, user_delete_inserter)
  ) /* let insert_friends =  */,
  App.post("/friends", req => load_json_login(req, add_friends, friend_inserter)) /* let insert_restriction =  */,
  App.post("/restrictions", req =>
    load_json(req, add_restrictions, rest_inserter)
    >>= (a => make_response(a))
  ) /* let insert_restrictions_index =  */,
  App.post("/restrictions/add", req =>
    load_json(req, add_restrictions_index, rest_indx_inserter)
    >>= (a => make_response(a))
  ),
  App.post("/preferences/add", req =>
    load_json(req, add_preferences_index, pref_indx_inserter)
    >>= (a => make_response(a))
  ),
  App.post("/cuisine/add", req =>
    load_json(req, add_cuisine, add_cuisine_inserter)
    >>= (a => make_response(a))
  ),
  App.post("/restrictions/remove", req =>
    load_json(req, remove_restrictions_index, rm_rest_inserter)
    >>= (a => make_response(a))
  ),
  App.post("/preferences/remove", req =>
    load_json(req, remove_preferences_index, rm_pref_inserter)
    >>= (a => make_response(a))
  ),
  App.post("/cuisine/remove", req =>
    load_json(req, remove_cuisine, rm_cuisine_inserter)
    >>= (a => make_response(a))
  ),
  App.post("/groups/join", req =>
    load_json_login(req, join_group, group_inserter)
  ),
  App.post("/groups/add", req =>
    load_json_login(req, add_group_info, group_info_inserter)
  ),
  App.post("/groups/delete", req =>
    load_json_login(req, delete_group, delete_group_inserter)
  ),
  App.post("/groups/newhost", req =>
    load_json_login(req, reassign_host, group_host_user_inserter)
  ),
  App.post("/groups/rmuser", req =>
    load_json_login(req, delete_from_group, group_host_user_inserter)
  ),
  App.post("/groups/invite", req =>
    load_json_login(req, add_group_invites, group_host_user_inserter)
  ),
  App.post("/login", req =>
    req.Request.body
    |> Body.to_string
    >>= (
      a => {
        let usrname = a |> from_string |> member("username") |> to_string;
        let pw = a |> from_string |> member("password") |> to_string;
        switch (Dbquery.login(usrname)) {
        | None =>
          Lwt.return(
            Ezjsonm.from_string({|{"success": false}|})
            |> toYojson
            |> toResponse,
          )
        | Some(password) =>
          if (Bcrypt.verify(pw, Bcrypt.hash_of_string(password))) {
            Lwt.return(
              Ezjsonm.from_string({|{"success": true}|})
              |> Json_repr.to_yojson
              |> Response.of_json,
            );
          } else {
            let ezjsonm = Ezjsonm.from_string({|{"success": false}|});
            let status = Json_repr.to_yojson(ezjsonm);

            Lwt.return(Response.of_json(status));
          }
        };
      }
    )
  ) /*Voting Routes*/,
  App.post("/survey", req =>
    load_json_login(req, ans_survey, survey_inserter)
  ),
  App.post("/ready", req =>
    load_json_login(req, process_survey, vote_status_inserter)
  ),
  App.post("/vote", req => load_json_login(req, add_votes, vote_inserter)),
  App.post("/done", req =>
    load_json_login(req, calculate_votes, vote_status_inserter)
  ),
];

let rec app_builder = (lst, app) =>
  switch (lst) {
  | [] => app
  | [h, ...t] => app_builder(t, app |> h)
  };

let port = 3000;

let start = () => {
  create_tables();
  Dotenv.export();
  print_endline(
    "Server running on port http://localhost:" ++ string_of_int(port),
  );

  App.empty
  |> App.port(port)
  // |> default
  |> App.middleware(Middleware.logger)
  |> app_builder(get_list)
  |> app_builder(post_list)
  |> App.run_command;
};
// let run = () => {
//   let listen_address =
//     Unix.([@implicit_arity] ADDR_INET(inet_addr_loopback, 8080));
//   let connection_handler = (addr, fd) => {
//     let f = (~request_handler, ~error_handler) =>
//       Httpaf_lwt_unix.Server.create_connection_handler(
//         ~request_handler=_ => request_handler,
//         ~error_handler=_ => error_handler,
//         addr,
//         fd,
//       );

//     Rock.Server_connection.run(f, app);
//   };
//   open Lwt.Syntax;
//   Lwt.async(() => {
//     let* _ =
//       Lwt_io.establish_server_with_client_socket(
//         listen_address,
//         connection_handler,
//       );
//     Lwt.return_unit;
//   });
//   let (forever, _) = Lwt.wait();
//   Lwt_main.run(forever);
// };

// let () = run();
