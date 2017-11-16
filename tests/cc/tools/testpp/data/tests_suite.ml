let non_test () =
	()
let test_fun1 () =
	assert_equal 0 1

let test_fun2 () =
	assert_equal 0 0
let suite = "Suite" >::: [
	"test_fun2" >:: test_fun2;
	"test_fun1" >:: test_fun1;
]
let _ = run_test_tt ~verbose:false suite