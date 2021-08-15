/*Test Plan: We used a mix of manual and automated testing to ensure the
    robustness of our backend.

    *****************Automated (Dbquery, Search, Passwords)***********************
    We used OUnit2 specifically to test interaction with the Database in terms of
    insertions, updates, and deletions. By performing operations using the in
    3  imterfacing get and set functions in the Dbquery module, we thoroughly
    checked that data were formatted correctly if acceptable and rejected if
    formatted incorrectly. We directly assessed Dbquery and Search, as the
    purpose of Db was to handle database table creation, which was not changed
    throughout the development process; testing of it was handled manually.
    The Server module was tested manually as well, because testing routes us
    ing OUnit2 presented challenges with request creation and running the test
    suite and server concurrently.

    Due to the dynamicity and size of the search operation, only property-based
    tests were employed; the true output was thoroughly assessed with different
    manual inputs to Postman (detailed below).

    We used dummy inputs generated by a script in order to validate our data
    insertion. However, in testing search, the program made live requests to the
    Zomato API.

    Types of testing: We used a mix of black box and glass box testing for the
    automatic portion. We created tests by using "make docs" and using the
    generated documentation to ensure that specifications were met. We then
    reviewed the actual implementation to ensure coverage of all branches.
    We did not utilize randomized testing.

    ************************Manual (Db, Server, Search)**************************
    We used Postman, an application for sending and receiving HTTP requests for
    testing the server module. We confirmed that the insertion of new users,
    voting, and performing surveys was reliable and that unique and primary key
    constraints in the database led to the rejection of invalid requests.
    We also employed DB Browser for sqlite--a graphical application for viewing
    and interacting with sqlite ".db" files, which allowed us to visualize the
    database structure and test the raw SQL that was in our source code. The DB
    module was tested with a thorough, manual review of each table.
    Types of testing: We used a combination of both black-box and glass-box
    testing for the manual portion. For black-box testing, we used the HTTP
    interface to make sure that we got responses for all well-formed bodies and
    routes (as documented). With glass-box testing, we were able to go
    line-by-line and ensure that for every table definition that we executed,
    the proper constraints, types, and structures were applied. Since the
    interface of Db does not expose any of its structure, we had to rely on
    glass-box testing exclusively for it.
    Testing Approach: This manual testing approach demonstrates the correctness
    of the system since we were able to ensure that our HTTP requests are
    successful and that the tables we created in sqlite3. Through the
    implementation of both automated and manual testing alongside we ensure
    the correctness of our system by implementing a wide
    variety of testing that provide a significant number of normal and edge-case
    inputs to our routes. After ensuring that our functions and routes are
    successful individually, our testing plan ensures that they correctly come
    together when testing our modules.
  */
open Lib;
open Lib.Dbquery;
open OUnit2;

/**[open_db] is evaluated when the module is first processed*/

let open_db = Lib.Dbquery.create_tables();

/*Testing user functions*/
let make_user =
    (~friends as f=[], ~restrictions as r=[], ~groups as g=[], id, un, pw, n) => {
  id,
  username: un,
  password: pw,
  name: n,
  friends: f,
  restrictions: r,
  groups: g,
  visited: [],
};

let make_group =
    (
      ~members as m=[],
      ~voting as v=false,
      ~top_5 as t5=None,
      ~top_pick as t=None,
      id,
      name,
      host,
    ) => {
  id,
  name,
  host_id: host,
  members: m,
  voting_allowed: v,
  top_5: t5,
  top_pick: t,
};

let make_restriction = (id, name) => {id, name};

/*OUnit2 test creators*/
let test_equal = (~compare as cmp=(==), name, exptd, expr) =>
  name >:: (_ => assert_equal(~cmp, exptd, expr));

let test_passes = (name, ~succ as s=true, ins_func, data) => {
  let try_test = (ins_func, data) =>
    try(
      switch (ins_func(data)) {
      | Some(_) => true
      | None => false
      }
    ) {
    | _ => false
    };
  name >:: (_ => assert_equal(s, try_test(ins_func, data)));
};

/**[try_get get_func id] is false if an attempt to retrieve data with [id]
   using get_func raises and exception. The resulting data are ignored.*/

let try_get = (~succ as b=true, name, get_func, id) => {
  let success =
    try(
      switch (get_func(id)) {
      | _ => true
      }
    ) {
    | Not_found => true
    | _ => false
    };
  test_equal(name, b, success);
};

/**ensures that the user matching [n] in the database matches the user
   generated with [make_user]*/

let test_user = (name, id, (u, p, n)) => {
  /*add a user to the database*/
  let user = make_user(id, u, Bcrypt.hash(p) |> Bcrypt.string_of_hash, n);
  test_equal(name, {...get_user(id), password: p}, {...user, password: p});
};

/**[ins_user] curries the user insertion function to throw an error within
   the test instead of the definition */

let ins_user = ((u, n, p)) => add_user(u, n, p);

/*definitions perform operations on the database at open; 8, 9 in suite*/
let users_1_thru_4 = (); /*Admins*/
let user_5 = add_user("reetu", "Reetu123", "Reetu");
let user_6 = add_user("bigdroo", "Andrew1", "Andrew");
let user_7 = add_user("andrewo", "Andrew2", "Andrew");
let user_8 = add_user("zach", "Zach123", "Zachary");
let user_9 = add_user("johndoe", "Johnny1", "John");
let user_10 = add_user("janedoe", "Jane123", "Jane");
let user_11 = add_user("peterparker", "Peter1234", "Peter");
let user_12 = ("andrew1235", "Baa5j1", "Andrew"); /*("andrew1235", "Baa5j1", "Andrew")*/
let user_13 = ("nikkinikki", "Andrew1 ", "Michael"); /*("nikkinikki", "Andrew1 ", "Michael")*/

let add_user_test = [
  /*validate existing insertions*/
  test_user(
    "ensure that the first user added is correct, 1-indexing",
    5,
    ("reetu", "Reetu123", "Reetu"),
  ),
  test_user(
    "ensure that the 5th user is correct",
    9,
    ("johndoe", "Johnny", "John"),
  ),
  test_user(
    "ensure that all users have been added",
    11,
    ("peterparker", "Peter1234", "Peter"),
  ),
  test_passes(
    "usernames must be unique",
    ~succ=false,
    ins_user,
    ("andrewosorio", "Andrew3", "John"),
  ),
  test_passes(
    "username is incorrect",
    ~succ=false,
    ins_user,
    ("", "Jane123", "Jane"),
  ),
];

let get_user_test = [
  try_get("valid user", get_user, 1),
  try_get("ids that are out of bounds are handled", get_user, 13),
  try_get("negative ids are not permitted", get_user, - 10),
  try_get("0th id is", get_user, 0),
  try_get("invalid users can not be gotten", get_user, 1000),
  try_get("min int", get_user, min_int),
  try_get("max int", get_user, max_int),
];

/*ensure validity of password requirements*/
let test_password = (name, bool, pw) =>
  test_equal(name, bool, Passwords.is_valid(pw));

let password_test = [
  /*ensure passwords meet criteria*/
  test_password("password cannot be empty", false, ""),
  test_password("contains no capital letter", false, "abcd123"),
  test_password("numeric password", false, "123135424354"),
  test_password("password contains no numbers", false, "Abcdefghi"),
  test_password("missing lowercase password", false, "ASD123"),
  test_password("password less than seven characters", false, "ab12"),
  test_password("valid password", true, "Abcde123"),
];

let ins_friends = ((f1, f2)) => add_friends(f1, f2);

let test_friends = (~are_friends as ff=true, name, f1, f2) => {
  let friends_1 = get_user(f1).friends;
  let expr =
    if (ff) {
      List.mem(f2, friends_1);
    } else {
      !List.mem(f2, friends_1);
    };
  name >:: (_ => assert_bool("not friends", expr));
};

/*definitions perform operations on the database at open*/
let friends_1 = add_friends(5, 6);
let friends_2 = add_friends(6, 7);
let friends_3 = add_friends(7, 8);
let friends_4 = (); /*(5, 8)*/
let friends_5 = (); /*(5, 13)*/

let friends_6 =
  try(add_friends(1, 2)) {
  | _ => None
  };
let friends_7 =
  try(add_friends(2, 3)) {
  | _ => None
  };
let friends_8 =
  try(add_friends(5, 13)) {
  | _ => None
  };

let add_friends_test = [
  /*attempt new insertions*/
  test_friends("insertion of a valid friendship", 1, 2),
  test_friends("insertion of the last friendship is valid", 7, 8),
  test_friends("insertion of first friend and last user are valid", 6, 7),
  try_get("insert two valid friends", ~succ=true, ins_friends, (1, 2)),
  try_get("friend first and last users", ~succ=true, ins_friends, (2, 3)),
  test_friends(
    ~are_friends=true,
    "users are friends following insertion",
    6,
    7,
  ),
  test_passes(
    "the same friendship, reversed",
    ~succ=false,
    ins_friends,
    (8, 5),
  ),
  test_passes("friendships are unique", ~succ=false, ins_friends, (5, 8)),
  test_friends(
    ~are_friends=false,
    "test that previous insert was blocked",
    1,
    1,
  ),
];

let is_friend_test = [
  test_friends("test that users 6 and 7 are friends", 6, 7),
  test_friends("test friend reciprocity", 7, 6),
  test_friends(~are_friends=false, "users that are not friends", 1, 3),
  test_friends(~are_friends=false, "one nonexistent user", 1, 55),
];

let ins_group = ((g_id, u_id)) => join_group(g_id, u_id);

/**ensures that the group matching [n] in the database matches the user
   generated with [make_group]*/

let test_group =
    (
      ~is_eq as eq=(==),
      ~members as m=[],
      ~voting as v=false,
      ~top_5 as t5=None,
      ~top_pick as t=None,
      name,
      group_id,
      n,
      h,
    ) => {
  let group =
    make_group(~members=m, ~voting=v, ~top_5=t5, ~top_pick=t, group_id, n, h);
  test_equal(~compare=eq, name, get_group(group_id), group);
};

let no_group_test = [
  try_get("join group before creation", ins_group, (1, 2)),
  try_get("invalid user joins a group", ins_group, (1, 14)),
  try_get("user tries to join invalid group", ins_group, (0, 3)),
];

let ins_group_info = ((g_name, h_id)) => add_group_info(g_name, h_id);

let group_1 = add_group_info("birthday party", 7);
let group_2 = add_group_info("anniversary dinner", 5);
let group_3 = add_group_info("lunch", 6);
let group_4 = add_group_info("taco tuesday", 5);
let group_5 = (); /*("birthday party", 2)*/
let group_6 = (); /*("garden party", 2)*/

/*the total number of valid groups should be 6*/
let add_group_info_test = [
  /* validate existing insertions */
  test_group(
    ~is_eq=(!=),
    "group and host id not reversed",
    2,
    "anniversary dinner",
    1,
  ),
  test_group(~is_eq=(!=), "mismatched group id and details", 1, "lunch", 3),
  test_group(~is_eq=(!=), "non existing group", 1, "dinner", 1),
  test_group(~is_eq=(!=), "non existing host", 1, "birthday party", 0),
  /* attempt new insertions*/
  test_passes(
    ~succ=false,
    "one host cannot create two groups with the same name",
    ins_group_info,
    ("birthday party", 3),
  ),
  test_passes(
    ~succ=false,
    "incorrect host_id cannot create group",
    ins_group_info,
    ("birthday party", 0),
  ),
];

/**[ins_group_invite] inserts [u_id] into [g_id] by invitation of [h_id]*/

let ins_group_invite = ((g_id, u_id, h_id)) =>
  add_group_invites(g_id, u_id, h_id);

let invite_1 = add_group_invites(3, 1, 6);
let invite_2 = add_group_invites(2, 3, 1);
let invite_3 = add_group_invites(1, 2, 3);
let invite_4 = (); /*5, 1, 2*/
let invite_5 = (); /*3, 1, 2*/

let group_invites_test = [
  test_passes(
    ~succ=false,
    "ensures that nonexisting admin cannot send invites",
    ins_group_invite,
    (1, 2, 0),
  ),
  test_passes(
    ~succ=false,
    "ensure invite cannot be sent twice to same memeber",
    ins_group_invite,
    (1, 2, 3),
  ),
  test_passes(
    ~succ=false,
    "ensure cannot send invalid group",
    ins_group_invite,
    (0, 1, 2),
  ),
  test_passes(
    ~succ=false,
    "ensure cannot send to invalid user",
    ins_group_invite,
    (1, 0, 3),
  ),
];
/*group membership ids, hosts are inserted by default*/
let group_mem1 = (); /*(1, 3)*/
let group_mem2 = (); /*(2, 1)*/
let group_mem3 = (); /*(3, 6)*/
let group_mem4 = (); /*(4, 5)*/
let group_mem5 = (); /*(5, 2)*/
let group_mem6 = (); /*(6, 2)*/
let group_mem7 = join_group(3, 1);
let group_mem8 = join_group(2, 3);
let group_mem9 = join_group(1, 3);

let add_group_test = [
  test_passes(
    ~succ=false,
    "ensures invalid member cannot be added to group",
    ins_group,
    (3, 0),
  ),
  test_passes(
    ~succ=false,
    "test that user cannot be added to nonexisting group",
    ins_group,
    (0, 3),
  ),
  test_passes(~succ=false, "cannot add host to group", ins_group, (5, 2)),
  test_passes(
    ~succ=false,
    "member cannot join same group",
    ins_group,
    (3, 4),
  ),
];

let get_group_test = [
  try_get("correctly returns a valid group", get_group, 1),
  try_get("ensures that nonexisting group cannot be returned", get_group, 0),
  try_get("negative groups are not permitted", get_group, - 10),
  try_get("min group", get_group, min_int),
  try_get("max group", get_group, max_int),
];

let is_member = (~succ as b=true, name, g_id, h_id) => {
  let res =
    try(
      switch (List.mem(g_id, get_user(h_id).groups)) {
      | bool => bool
      }
    ) {
    | _ => false
    };
  test_equal(name, b, res);
};

let member_test = [
  is_member(
    "check that the host is a member by default (no invitation)",
    1,
    7,
  ),
  is_member("check that a non-host's invitation was acepted", 3, 1),
  is_member(~succ=false, "member that is not in a group", 1, 5),
  is_member(
    ~succ=false,
    "check if nonexistent group contains an existing user",
    0,
    3,
  ),
];

let test_restriction = (~succ as b=true, name, rest_id, rest_name) => {
  let comp = if (b) {(==)} else {(!=)};
  test_equal(~compare=comp, name, get_restriction_by_id(rest_id), rest_name);
};

/**Inserts a restriction using [ad_id] */

let ins_restriction = ((ad_id, rest_name)) =>
  add_restrictions_index(ad_id, rest_name);

let del_restriction = ((ad_id, rest_id)) =>
  remove_restrictions_index(ad_id, rest_id);

let restriction_1 = add_restrictions_index(1, "Vegan");
let restriction_2 = add_restrictions_index(2, "dairy");
let restriction_3 = add_restrictions_index(3, "Gluten");
let restriction_4 = add_restrictions_index(4, "peanuts");
/* let restriction_5 = ignore (add_restrictions_index 4 "") */

let add_restriction_test = [
  test_restriction(
    "insertion of a valid restriction by an admin",
    4,
    "peanuts",
  ),
  try_get("invalid restriction name", add_restrictions, 2),
  test_passes(
    "duplicate restriction name",
    ~succ=false,
    ins_restriction,
    (2, "Vegan"),
  ),
  test_passes(
    "case-insensitive restrictions are unique",
    ~succ=false,
    ins_restriction,
    (3, "gluten"),
  ),
  test_passes(
    "insertion of restriction not by admin",
    ~succ=false,
    ins_restriction,
    (8, "shellfish"),
  ),
];

let delete_restriction_test = [
  /*test deletions*/
  test_passes(
    "delete nonexistent restriction",
    ~succ=false,
    del_restriction,
    (2, 0),
  ),
  test_passes(
    ~succ=false,
    "invalid restriction cannot be deleted",
    del_restriction,
    (3, 0),
  ),
  test_passes(
    ~succ=false,
    "cannot delete restriction not by admin",
    del_restriction,
    (0, 0),
  ),
];

let get_restrictions_test = [
  test_restriction(
    ~succ=true,
    "correctly returns a valid restriction",
    1,
    "Vegan",
  ),
  test_restriction("case insensitive restriction", 2, "dairy"),
  try_get("nonexistent restriction", get_restriction_by_id, 15),
  try_get("min restriction", get_restriction_by_id, min_int),
  try_get("max restriction", get_restriction_by_id, max_int),
];

let test_preference = (~succ as b=true, name, pref_id, pref_name) => {
  let comp = if (b) {(==)} else {(!=)};
  test_equal(~compare=comp, name, get_preference_by_id(pref_id), pref_name);
};

let ins_pref = ((ad_id, pref_name)) =>
  add_preferences_index(ad_id, pref_name);

let del_pref = ((ad_id, pref_name)) =>
  remove_preferences_index(ad_id, pref_name);

let preferences_1 = add_preferences_index(1, "Takeout");
let preferences_2 = add_preferences_index(2, "Dog Friendly");
let preferences_3 = add_preferences_index(4, "Indoors");
let preferences_4 = (); /*("peanuts")*/

let change_preferences_test = [
  test_preference(
    ~succ=true,
    "check first preferences added to empty preferences",
    1,
    "Takeout",
  ),
  test_preference(
    ~succ=true,
    "more than one person can add preference",
    2,
    "Dog Friendly",
  ),
  try_get(
    ~succ=true,
    "test that user can add preference",
    ins_pref,
    (1, "Takeout"),
  ),
  test_passes(
    ~succ=false,
    "ensures invalid member cannot add to preferences",
    ins_pref,
    (0, "Outdoors"),
  ),
  try_get(
    ~succ=false,
    "test that admin can delete preference",
    del_pref,
    (1, 1),
  ),
  try_get(
    ~succ=false,
    "test that invalid user cannot delete a preference",
    del_pref,
    (0, 3),
  ),
  try_get(
    ~succ=false,
    "test that admins cannot delete nonexisting preferences",
    del_pref,
    (3, 0),
  ),
];

let get_preferences_test = [
  try_get(
    "correctly returns the first preference id",
    get_preference_by_id,
    1,
  ),
  try_get(
    "ensures that nonexisting admin cannot get preference",
    get_preference_by_id,
    5,
  ),
  try_get("returns the minimum preference id", get_preference_by_id, min_int),
  try_get("returns the maximum preference id", get_preference_by_id, max_int),
];

let test_cuisine = (~succ as b=true, name, c_id, c_name) => {
  let comp = if (b) {(==)} else {(!=)};
  test_equal(~compare=comp, name, get_cuisine_by_id(c_id), c_name);
};

let ins_cuisine = ((ad_id, cuisine_id, cuisine_name)) =>
  add_cuisine(ad_id, cuisine_id, cuisine_name);

/*[del_cuisine] does not require the cuisine name*/
let del_cuisine = ((ad_id, cuisine_id)) =>
  remove_cuisine(ad_id, cuisine_id);

let cuisine_1 = add_cuisine(1, 55, "Italian");
let cuisine_2 = add_cuisine(4, 73, "Mexican");
let cuisine_3 = add_cuisine(2, 25, "Chinese");
let cuisine_4 = add_cuisine(3, 1018, "Modern Indian");

let cuisines_test = [
  test_cuisine(
    ~succ=true,
    "check first cuisine is added to empty cuisines",
    55,
    "Italian",
  ),
  test_cuisine(
    ~succ=true,
    "test that more than one cuisines can be added to cuisines",
    73,
    "Mexican",
  ),
  try_get(
    ~succ=true,
    "ensures that the first cuisine is inserted correctly",
    ins_cuisine,
    (1, 55, "Italian"),
  ),
  test_passes(
    ~succ=false,
    "ensures that invalid cuisine cannot be added",
    ins_cuisine,
    (1, 25, ""),
  ),
  try_get(
    ~succ=true,
    "test that admin can delete cuisine",
    del_cuisine,
    (1, 55),
  ),
  test_passes(
    ~succ=false,
    "test that invalid cuisine cannot be deleted",
    del_cuisine,
    (1, (-1)),
  ),
];

let get_cuisines_test = [
  try_get("correctly returns a valid cuisine", get_cuisine_by_id, 1),
  try_get(
    "ensures that nonexisting cuisine cannot be returned",
    get_cuisine_by_id,
    - 1,
  ),
  try_get("0th cuisine is", get_cuisine_by_id, 0),
  try_get("min cuisine", get_cuisine_by_id, min_int),
  try_get("max cuisine", get_cuisine_by_id, max_int),
];

let del_group = ((u_id, g_id)) => delete_group(u_id, g_id);

/*both admins and hosts can delete groups*/
let delete_group_test = [
  test_passes(
    "ensure that first group is deleted",
    ~succ=false,
    del_group,
    (1, 2),
  ),
  test_passes(
    "delete already deleted group",
    ~succ=false,
    del_group,
    (2, 2),
  ),
  test_passes(
    "nonexisting admin delete group",
    ~succ=false,
    del_group,
    (0, 3),
  ),
  test_passes("delete maximum group", ~succ=false, del_group, (4, max_int)),
  test_passes("delete minimum group", ~succ=false, del_group, (2, min_int)),
];

let kick_member = ((g_id, u_id, h_id)) =>
  delete_from_group(g_id, u_id, h_id);

let has_member = ((g_id, h_id)) => List.mem(h_id, get_group(g_id).members);

/*use [is_member] to assess whether a user has a group*/
let delete_from_group_test = [
  try_get(~succ=true, "kick first member of group", kick_member, (1, 3, 7)),
  test_passes(
    ~succ=false,
    "kick somone not in group",
    kick_member,
    (1, 2, 7),
  ),
  test_passes(
    ~succ=false,
    "cannot kick host from group",
    kick_member,
    (1, 7, 7),
  ),
  test_passes(
    ~succ=false,
    "kick the maximum user_id from the group",
    kick_member,
    (2, max_int, 7),
  ),
  test_passes(
    ~succ=false,
    "kick the minimum user_id from the group",
    kick_member,
    (2, min_int, 7),
  ),
  try_get(
    ~succ=true,
    "get the host of the group, since cannot delete host",
    has_member,
    (1, 7),
  ),
  try_get(
    "cannot get a nonexisting member of the group",
    has_member,
    (1, 3),
  ),
  try_get(
    "cannot get from a member from a nonexisting group",
    has_member,
    (0, 3),
  ),
];

let is_hosting = ((g_id, u_id)) => get_group(g_id).host_id == u_id;

let change_host = ((g_id, u_id, h_id)) => reassign_host(g_id, u_id, h_id);

let reassign_host_test = [
  try_get(
    ~succ=true,
    "ensures that original host is correct",
    is_hosting,
    (1, 7),
  ),
  try_get("cannot return invalid host", is_hosting, (2, 0)),
  try_get("cannot get host of invalid group", is_hosting, (0, 2)),
  try_get(~succ=true, "successfully change host", change_host, (1, 2, 7)),
  test_passes(
    ~succ=false,
    "ensures cannot change host of invalid group",
    change_host,
    (0, 2, 5),
  ),
  test_passes(
    ~succ=false,
    "cannot change host if user does not exist",
    change_host,
    (3, 10, 6),
  ),
  test_passes(
    ~succ=false,
    "cannot change host if host_id is invalid",
    change_host,
    (4, 2, 5),
  ),
  test_passes(
    ~succ=false,
    "cannot assign host if user_id is already host",
    change_host,
    (4, 5, 5),
  ),
];

let rests_exist = (~survey as s=true, ~succ as b=false, name, grp_id) => {
  let restaurants =
    if (s) {
      get_group(grp_id).top_5;
    } else {
      get_group(grp_id).top_pick;
    };
  let result =
    switch (restaurants) {
    | Some(_) => true
    | None => false
    };
  name >:: (_ => assert_bool("result was not determined", result == b));
};

/*submit surveys for group 3*/
let survey_1 = ans_survey(1, 3, 76.423, 70.5005, "25", 3);
let survey_2 = ans_survey(6, 3, -. 76.423, 64.5005, "25", 14);

/*submit survey for group 2*/
let survey_3 = ans_survey(5, 2, 76.423, 70.5005, "25,55", 3, 5);

let submit_group_3 = process_survey(3, 6);
let submit_group_2 = process_survey(2, 5);

let survey_test = [
  rests_exist("results for two-person group exist", 3),
  rests_exist("results for a group with incomplete votes", 2),
  rests_exist(~succ=false, "no results for a group without a survey", 1),
];

let vote_1 = add_votes(3, 1, [4, 1, 2, 0, 3]);
let vote_2 = add_votes(3, 6, [1, 0, 2, 3, 4]);
let vote_3 = add_votes(2, 5, [4, 1, 2, 0, 3]);

let voting_test = [
  rests_exist("results for two-person group exist", 3),
  rests_exist("vote with only the host succeeds", 2),
  rests_exist(~succ=false, "votes for a group that did not have a vote", 1),
];

let tests =
  "test suite for uPick"
  >::: List.flatten([
         add_user_test,
         get_user_test,
         password_test,
         add_friends_test,
         is_friend_test,
         no_group_test,
         add_group_info_test,
         add_group_test,
         get_group_test,
         member_test,
         add_restriction_test,
         delete_restriction_test,
         get_restrictions_test,
         change_preferences_test,
         get_preferences_test,
         cuisines_test,
         get_cuisines_test,
         group_invites_test,
         delete_group_test,
         delete_from_group_test,
         reassign_host_test,
         survey_test,
         voting_test,
       ]);

let _ = run_test_tt_main(tests);
