open Rock;
open Lwt.Syntax;
//
let index_handler = _req => {
  let headers = Httpaf.Headers.of_list([("Content-Type", "text/plain")]);
  let body = Body.of_string("Hello World!\n");
  Lwt.return @@ Response.make(~headers, ~body, ());
};

let sum_handler = (~a, ~b, _req) => {
  let headers = Httpaf.Headers.of_list([("Content-Type", "text/plain")]);
  let body =
    Body.of_string(Printf.sprintf("Sum of %d and %d = %d\n", a, b, a + b));
  Lwt.return @@ Response.make(~headers, ~body, ());
};

module Router = {
  let m = {
    let filter = (handler, req) => {
      let parts =
        req.Request.target
        |> String.split_on_char('/')
        |> List.filter(x => !String.equal(x, ""));

      switch (parts) {
      | [] => index_handler(req)
      | ["sum", a, b] =>
        sum_handler(~a=int_of_string(a), ~b=int_of_string(b), req)
      | _ => handler(req)
      };
    };

    Middleware.create(~filter, ~name="");
  };
};

let app =
  Rock.App.create(
    ~middlewares=[Router.m],
    ~handler=
      _ => {
        let body = Body.of_string("No route found\n");
        Lwt.return(Response.make(~status=`Not_found, ~body, ()));
      },
    (),
  );

let run = () => {
  let listen_address =
    Unix.([@implicit_arity] ADDR_INET(inet_addr_loopback, 8080));
  let connection_handler = (addr, fd) => {
    let f = (~request_handler, ~error_handler) =>
      Httpaf_lwt_unix.Server.create_connection_handler(
        ~request_handler=_ => request_handler,
        ~error_handler=_ => error_handler,
        addr,
        fd,
      );

    Rock.Server_connection.run(f, app);
  };

  Lwt.async(() => {
    let* _ =
      Lwt_io.establish_server_with_client_socket(
        listen_address,
        connection_handler,
      );
    Lwt.return_unit;
  });
  let (forever, _) = Lwt.wait();
  Lwt_main.run(forever);
};

let () = run();
