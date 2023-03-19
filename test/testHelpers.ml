let assert_bool name expected actual =
  let test_function () = Alcotest.(check bool) name expected actual in
  Alcotest.test_case name `Quick test_function

let assert_raises name exn test_func =
  let assertion () = Alcotest.check_raises name exn test_func in
  Alcotest.test_case name `Quick assertion

let assert_no_raises name test_func =
  let assertion () = Alcotest.(check bool) name true
  (try
    let () = test_func () in
    true
  with raised_exn -> raise raised_exn) in
  Alcotest.test_case name `Quick assertion

let assert_true name value = assert_bool name true value
let assert_false name value = assert_bool name false value