module ErrorTests = struct

  module Parsing = struct
    open Interpreter
    open Parser

    let operations () =
      begin
        Alcotest.check_raises "mismatched parentheses" Parentheses_mismatch (fun () -> interpret "("; ());
        Alcotest.check_raises "mismatched parentheses" Parentheses_mismatch (fun () -> interpret ")"; ());
        Alcotest.check_raises "mismatched parentheses" Parentheses_mismatch (fun () -> interpret "(()"; ());
        Alcotest.check_raises "mismatched parentheses" Parentheses_mismatch (fun () -> interpret "())"; ());
      end
  end

end

module EvalTests = struct

  open Interpreter

  let check = Alcotest.(check string)

  module Basic = struct
    let operations () =
      begin
        check "positive integer" "3" (interpret "3");
        check "negative integer" "-3" (interpret "-3");
        check "empty list" "()" (interpret "'()");
        check "true" "#t" (interpret "#t");
        check "false" "#f" (interpret "#f");
        check "list with one element" "(1)" (interpret "'(1)");
        check "list with two elements" "(1 2)" (interpret "'(1 2)");
        check "list with nested elements" "((1 2) 3)" (interpret "'('(1 2) 3)");
        check "list with elements of different types" "((\"abc\" 2) 3)" (interpret "'('(\"abc\" 2) 3)");
        check "extra spaces are okay" "(1 2 3)" (interpret "'(1  2  3)");
        check "some russian (unicode)" "\"Съешь же ещё этих мягких французских булок да выпей чаю\"" (interpret "\"Съешь же ещё этих мягких французских булок да выпей чаю\"");
        check "ascii strings" "\"The quick brown fox jumps over the lazy dog 0123456789 !@#$%^&*();':[]{}<>,./\|`~\"" (interpret "\"The quick brown fox jumps over the lazy dog 0123456789 !@#$%^&*();':[]{}<>,./\|`~\"");
        check "empty strings" "\"\"" (interpret "\"\"");
        check "character escaping" "\"\\\" abc \\\"\"" (interpret "\"\\\" abc \\\"\"");
      end
  end

  module Arithmetic = struct
    let operations () =
      begin
        check "addition of two numbers" "3" (interpret "(+ 1 2)");
        check "subtraction of a number from another number" "2" (interpret "(- 3 1)");
        check "division with no remainder" "2" (interpret "(/ 4 2)");
        check "division with remainder" "2" (interpret "(/ 5 2)");
        check "multiplication" "10" (interpret "(* 5 2)");
        check "modulo" "1" (interpret "(% 5 2)");
        check "nested operations" "1" (interpret "(- (+ (* 2 (/ 4 2)) (- 2 3)) (% 5 3))");
        check "nested operations" "980" (interpret "(+ 14 (* 23 42))");
        check "nested operations" "57" (interpret "(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))");
      end

    let comparisons () =
      begin
        check "less than" "#t" (interpret "(< 1 2)");
        check "less than" "#f" (interpret "(< 2 2)");
        check "more than" "#t" (interpret "(> 5 2)");
        check "more than" "#f" (interpret "(> 2 2)");
        check "less than or equals" "#t" (interpret "(<= 2 2)");
        check "less than or equals" "#f" (interpret "(<= 3 2)");
        check "more than or equals" "#t" (interpret "(>= 2 2)");
        check "more than or equals" "#f" (interpret "(>= 1 2)");
        check "equals" "#t" (interpret "(= 1 1)");
        check "equals" "#f" (interpret "(= 1 2)");
        check "not equals" "#t" (interpret "(\\= 1 2)");
        check "not equals" "#f" (interpret "(\\= 1 1)");
      end
  end

  module LogicalConnectives = struct
    let conjunction () =
      begin
        check "Conjunction TT" "#t" (interpret "(and #t #t)");
        check "Conjunction TF" "#f" (interpret "(and #t #f)");
        check "Conjunction FT" "#f" (interpret "(and #f #t)");
        check "Conjunction FF" "#f" (interpret "(and #f #f)")
      end

    let disjunction () =
      begin
        check "Disjunction TT" "#t" (interpret "(or #t #t)");
        check "Disjunction TF" "#t" (interpret "(or #t #f)");
        check "Disjunction FT" "#t" (interpret "(or #f #t)");
        check "Disjunction FF" "#f" (interpret "(or #f #f)");
      end
  end

  module Conditional = struct
    let operations () =
      begin
        check "if clause with a true predicate should return the first expression" "1" (interpret "(if #t 1 2)");
        check "if clause with a tru-ish predicate should return the first expression" "1" (interpret "(if 42 1 2)");
        check "if clause with a false predicate should return the second expression" "2" (interpret "(if #f 1 2)");
        check "if <" "4" (interpret "(if (< 1 2) 4 5)");
        check "if >" "2" (interpret "(if (> 1 2) (* 2 2) (/ 5 2))");
        check "if =" "2" (interpret "(if (= 1 2) (* 2 2) (/ 5 2))");
        check "if \\=" "4" (interpret "(if (\\= 1 2) (* 2 2) (/ 5 2))");
        check "if >=" "2" (interpret "(if (>= 1 2) (* 2 2) (/ 5 2))");
        check "if <=" "4" (interpret "(if (<= 1 2) (* 2 2) (/ 5 2))");
      end
  end

  module List = struct
    let operations () =
      begin
        check "car of quoted list" "1" (interpret "(car '(1 2 3))");
        check "car of quoted list" "'a" (interpret "(car '('a 'b 'c))");
        check "car of quoted list" "'c" (interpret "(car '('c 3 'a 'b))");
        check "car of quoted list with quoted list as head" "(1 2)" (interpret "(car '('(1 2) 3 'a 'b))");
        check "car should return head of list with one element" "1" (interpret "(car '(1))");
        check "cdr should return tail of list with 3 elements" "(2 3)" (interpret "(cdr '(1 2 3))");
        check "cdr should return tail of list with 2 elements" "(3)" (interpret "(cdr '(2 3))");
        check "cdr should return empty list as tail of list with 1 element" "()" (interpret "(cdr '(3))");
        check "cons of one number with empty list" "(1)" (interpret "(cons 1 '())");
        check "cons of result of expression with empty list" "(3)" (interpret "(cons (+ 1 2) '())");
        check "cons of quoted list with empty list" "((1 2))" (interpret "(cons '(1 2) '())");
        check "cons of number with non-empty list" "(1 2 3)" (interpret "(cons 1 '(2 3))");
        check "cons of number with nested non-empty list" "(1 (1 1) 2)" (interpret "(cons 1 '('(1 1) 2))");
        check "nested list operations" "(2 3)" (interpret "(cdr (cons 1 '((* 1 2) (if (< 1 2) 3 4))))");
        check "Quoted list prevents evaluation" "(+ 1 2)" (interpret "'(+ 1 2)");
        check "Declare a quoted datum with the quote operation" "1" (interpret "(quote 1)");
        check "Declare a quoted datum with the quote operation" "'a" (interpret "(quote 'a)");
        check "Declare a quoted list with the quote operation" "(1)" (interpret "(quote (1))");
        check "Declare a quoted list with the quote operation" "(1 2 3)" (interpret "(quote (1 2 3))");
        (* Alcotest.(check string) "Declare a quoted nested list with the quote operation" "(1 (2 3) 4)" (interpret "(quote (1 (2 3) 4))"); *)
      end

  end
end

let () =
  Alcotest.run "all tests" [
    "Eval tests", [
      "Language basic syntax", `Quick, EvalTests.Basic.operations;
      "Arithmetic operations", `Quick, EvalTests.Arithmetic.operations;
      "Arithmetic comparisons", `Quick, EvalTests.Arithmetic.comparisons;
      "Logical conjunctions", `Quick, EvalTests.LogicalConnectives.conjunction;
      "Logical disjunctions", `Quick, EvalTests.LogicalConnectives.disjunction;
      "Conditionals", `Quick, EvalTests.Conditional.operations;
      "List", `Quick, EvalTests.List.operations;
    ];
    "Error tests", [
      "Parser errors", `Quick, ErrorTests.Parsing.operations;
    ]
  ]
