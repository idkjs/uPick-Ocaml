let () = {
  let env_variables = Dotenv.parse();
  List.iter(
    ((name, value)) => print_endline(Printf.sprintf("%s=%s", name, value)),
    env_variables,
  );
};
