open Eval

module Arithmetic = struct

  let operations () =
    begin
      Alcotest.(check string) "Addition" "3" (interpret "(+ 1 2)");
      Alcotest.(check string) "Subtraction" "2" (interpret "(- 3 1)");
      Alcotest.(check string) "Division with no remainder" "2" (interpret "(/ 4 2)");
      Alcotest.(check string) "Division with remainder" "2" (interpret "(/ 5 2)");
      Alcotest.(check string) "Multiplication" "10" (interpret "(* 5 2)");
      Alcotest.(check string) "Modulo" "1" (interpret "(% 5 2)");
      Alcotest.(check string) "Composed operations" "1" (interpret "(- (+ (* 2 (/ 4 2)) (- 2 3)) (% 5 3))")
    end

end

module LogicalConnectives = struct

  let conjunction () = 
    begin
      Alcotest.(check string) "Conjunction TT" "#t" (interpret "(and #t #t)");
      Alcotest.(check string) "Conjunction TF" "#f" (interpret "(and #t #f)");
      Alcotest.(check string) "Conjunction FT" "#f" (interpret "(and #f #t)");
      Alcotest.(check string) "Conjunction FF" "#f" (interpret "(and #f #f)")
    end

  let disjunction () = 
    begin
      Alcotest.(check string) "Disjunction TT" "#t" (interpret "(or #t #t)");
      Alcotest.(check string) "Disjunction TF" "#t" (interpret "(or #t #f)");
      Alcotest.(check string) "Disjunction FT" "#t" (interpret "(or #f #t)");
      Alcotest.(check string) "Disjunction FF" "#f" (interpret "(or #f #f)");
    end

end

module Conditional = struct

  let operations () = 
    begin
      Alcotest.(check string) "if <" "4" (interpret "(if (< 1 2) 4 5)");
      Alcotest.(check string) "if >" "2" (interpret "(if (> 1 2) (* 2 2) (/ 5 2))");
      Alcotest.(check string) "if =" "2" (interpret "(if (= 1 2) (* 2 2) (/ 5 2))");
      Alcotest.(check string) "if \\=" "4" (interpret "(if (\\= 1 2) (* 2 2) (/ 5 2))");
      Alcotest.(check string) "if >=" "2" (interpret "(if (>= 1 2) (* 2 2) (/ 5 2))");
      Alcotest.(check string) "if <=" "4" (interpret "(if (<= 1 2) (* 2 2) (/ 5 2))");
    end

end

module List = struct

  let operations () = 
    begin
      Alcotest.(check string) "car" "1" (interpret "(car '(1 2 3))");
      Alcotest.(check string) "car" "'a" (interpret "(car '('a 'b 'c))");
      Alcotest.(check string) "car" "'c" (interpret "(car '('c 3 'a 'b))");
      Alcotest.(check string) "car" "1" (interpret "(car '(1))");
      Alcotest.(check string) "cdr" "(2 3)" (interpret "(cdr '(1 2 3))");
    end
end

(* Run it *)
let () =
  Alcotest.run "All tests" [
    "Tests", [
      "Arithmetic operations", `Quick, Arithmetic.operations;
      "Logical conjunctions", `Quick, LogicalConnectives.conjunction;
      "Logical disjunctions", `Quick, LogicalConnectives.disjunction;
      "Conditionals", `Quick, Conditional.operations;
      "List", `Quick, List.operations;
    ]
  ]
