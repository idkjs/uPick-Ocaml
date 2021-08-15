type node = {
  mutable token: option(string),
  target: string,
  mutable wait_token: Lwt.t(string),
};

type config = {
  nodes: list(node),
  delay_head: float,
  delay_peers: float,
};

let save_config = ref(() => failwith("NullPointerException"));

let destruct = (encoding, data) => {
  let yojson = Yojson.Safe.from_string(data);
  let ezjsonm = Json_repr.from_yojson(yojson);
  Json_encoding.destruct(encoding, ezjsonm);
};

let register_for_token = ({token, target, wait_token: _} as node) =>
  switch (token) {
  | Some(token) => Lwt.return(token)
  | None =>
    let (ret, resolver) = Lwt.task();
    let () =
      EzCohttp.post(
        ~content_type="application/json",
        ~content=Main_to_db.(ask(register_encoding, target)),
        "register " ++ target,
        EzAPI.TYPES.URL("http://127.0.0.1:9000/register"),
        s => {
          let (data, error) = destruct(Main_to_db.token_encoding, s);
          switch (data) {
          | Some(token) =>
            let () =
              switch (error) {
              | None => ()
              | Some(error) =>
                let () = Printf.eprintf("Errors: %s", error);
                flush(stderr);
              };

            let () = node.token = Some(token);
            let () = save_config^();
            Lwt.wakeup(resolver, token);
          | None =>
            switch (error) {
            | None =>
              Lwt.wakeup_later_result(
                resolver,
                Error(Failure("No token for target" ++ target)),
              )
            | Some(error) =>
              Lwt.wakeup_later_result(resolver, Error(Failure(error)))
            }
          };
        },
      );
    ret;
  };

let dummy_string_lwt = Lwt.return("");

let config_encoding = {
  open Json_encoding;
  let node_encoding =
    conv(
      ({token, target, wait_token: _}) => (token, target),
      ((token, target)) => {
        let ret = {token, target, wait_token: dummy_string_lwt};
        let () = ret.wait_token = register_for_token(ret);
        ret;
      },
      obj2(opt("token", string), req("target", string)),
    );
  let encoding =
    obj3(
      req("nodes", list(node_encoding)),
      dft("delay_head", float, 10.),
      dft("delay_peers", float, 60.),
    );

  conv(
    ({nodes, delay_head, delay_peers}) => (nodes, delay_head, delay_peers),
    ((nodes, delay_head, delay_peers)) => {nodes, delay_head, delay_peers},
    encoding,
  );
};

let read_config = {
  let already_read = ref(false);
  config_file =>
    if (! already_read^) {
      let yojson = Yojson.Safe.from_file(~fname="config", config_file);
      let ezjsonm = Json_repr.from_yojson(yojson);
      let ret = Json_encoding.destruct(config_encoding, ezjsonm);
      let () = already_read := true;
      ret;
    } else {
      failwith(
        "The config is already loaded: You shouldn't reload it because"
        ++ " it create Lwt value",
      );
    };
};

let (config, config_path) =
  try({
    let path = "config.json";
    (read_config(path), path);
  }) {
  /* hack during development */
  | _ =>
    let path = "src/sonde/config.json";
    (read_config(path), path);
  };

let write_config = () => {
  let ezjsonm = Json_encoding.construct(config_encoding, config);
  let yojson = Json_repr.to_yojson(ezjsonm);
  Yojson.Safe.to_file(config_path, yojson);
};

let () = save_config := write_config;

let () = write_config();

let fetch = (msg, delay) =>
  Printf.printf(
    "We will fetch %s every %f second%s",
    msg,
    delay,
    if (delay > 2.) {
      "s\n";
    } else {
      "\n";
    },
  );

let () = print_endline("config loaded");
let () =
  List.iter(
    config => print_endline("We will monitor: " ++ config.target),
    config.nodes,
  );
let () = fetch("head", config.delay_head);
let () = fetch("peers", config.delay_peers);

let generate_crawler_URL =
    (
      root,
      url_name,
      url,
      inital_value,
      encoding,
      error,
      delay,
      action,
      wait_token,
    ) => {
  let last_value = ref(inital_value)
  and url = EzAPI.TYPES.URL(root ++ url)
  and error =
    switch (error) {
    | Some(error) => error
    | None => (
        _i =>
          fun
          | Some(s) =>
            Printf.eprintf("A request has failed with error n: %s", s)
          | None => Printf.eprintf("A request has failed with error n")
      )
    }
  and (ret, _resolver) = Lwt.wait();
  let rec peek_URL =
    fun
    | () => {
        let () =
          EzCohttp.get(
            url_name,
            url,
            ~error,
            s => {
              let value = destruct(encoding, s);
              let wait_token =
                Lwt.map(
                  tok => last_value := action(tok, last_value^, value),
                  wait_token,
                );
              let sleep = Lwt_unix.sleep(delay);
              let sleep = Lwt.(<&>)(sleep, wait_token);
              let _ = Lwt.bind(sleep, peek_URL);
              ();
            },
          );

        ret;
      };
  peek_URL();
};

let peek_head = node_config => {
  let print_head = head =>
    print_endline(node_config.target ++ " is at " ++ head);
  generate_crawler_URL(
    node_config.target,
    "peek_head",
    "/chains/main/blocks/head/hash",
    "",
    Json_encoding.string,
    None,
    config.delay_head,
    fun
    | _token => (
        fun
        | last_head => (
            fun
            | head => {
                let () =
                  if (last_head != head) {
                    print_head(head);
                  };
                head;
              }
          )
      ),
    node_config.wait_token,
  );
};

let peek_peers = node_config => {
  let print_peers = num => {
    let token =
      switch (node_config.token) {
      | Some(token) => token
      | None => assert(false)
      };
    let () =
      EzCohttp.post(
        ~content_type="application/json",
        ~content=Main_to_db.(ask(peers_encoding, num)),
        "peers " ++ string_of_int(num),
        EzAPI.TYPES.URL("http://127.0.0.1:9000/tezos/peers"),
        ~headers=[("db_token", token)],
        _ =>
        ()
      );
    Printf.printf(
      "%s as %d active or running peer%s",
      node_config.target,
      num,
      if (num > 2) {
        "s\n";
      } else {
        "\n";
      },
    );
  };
  generate_crawler_URL(
    node_config.target,
    "peek_peers",
    "/network/peers",
    -1,
    Ocplib_tezos.Tezos_encoding.Encoding.Network.encoding,
    None,
    config.delay_peers,
    fun
    | _token => (
        fun
        | last_number_peers => (
            fun
            | peers => {
                let peers =
                  List.filter(
                    Ocplib_tezos.Tezos_types.(e => e.state != Disconnected),
                    peers,
                  );
                let num = List.length(peers);
                let () =
                  if (last_number_peers != num) {
                    print_peers(num);
                  };
                num;
              }
          )
      ),
    node_config.wait_token,
  );
};

let peek_node = node_config =>
  Lwt.join([peek_head(node_config), peek_peers(node_config)]);

let () = List.map(peek_node, config.nodes) |> Lwt.join |> Lwt_main.run;
