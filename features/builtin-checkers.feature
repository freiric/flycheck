Feature: Builtin checkers

  Scenario: Bash syntax error
    Given a buffer "error.bash" in sh-mode:
      """
      #!/bin/bash

      if true then
        echo "Hello World"
      fi
      """
    And I check syntax
    Then I should see Flycheck errors:
      | filename   | checker | level | line | message                                 |
      | error.bash | bash    | error |    5 | syntax error near unexpected token `fi' |
      | error.bash | bash    | error |    5 | `fi'                                    |

  Scenario: C/C++ Clang warning
    Given a buffer "warning.c" in c-mode:
      """
      int f(int x)
      {
           int unused;
           unsigned int y = 10;
           return x < y ? ++x : x;
      }
      """
    When I disable checker c/c++-cppcheck
    And I check syntax
    Then I should see Flycheck errors:
      | filename  | checker     | level   | line | column | message                                                             |
      | warning.c | c/c++-clang | warning |    3 |     10 | unused variable 'unused'                                            |
      | warning.c | c/c++-clang | warning |    5 |     15 | comparison of integers of different signs: 'int' and 'unsigned int' |

  Scenario: C/C++ Clang customized warnings
    Given a buffer "warning.c" in c-mode:
     """
     int f(int x)
     {
          int unused;
          unsigned int y = 10;
          return x < y ? ++x : x;
     }
     """
    When I set flycheck-clang-warnings to ("all" "missing-prototypes")
    And I disable checker c/c++-cppcheck
    And I check syntax
    Then I should see Flycheck errors:
      | filename  | checker     | level   | line | column | message                                |
      | warning.c | c/c++-clang | warning |    1 |      5 | no previous prototype for function 'f' |
      | warning.c | c/c++-clang | warning |    3 |     10 | unused variable 'unused'               |

  Scenario: C/C++ Clang error
    Given a buffer "error.c" in c-mode:
      """
      int main(void)
      {
          return foo;
      }
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename | checker     | level | line | column | message                                                 |
      | error.c  | c/c++-clang | error |    3 |     12 | use of undeclared identifier 'foo'; did you mean 'for'? |

  Scenario: C/C++ Clang fatal error
    Given a buffer "fatal-error.c" in c-mode:
      """
      #include <no-such-header.h>
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename      | checker     | level | line | column | message                           |
      | fatal-error.c | c/c++-clang | error |    1 |     10 | 'no-such-header.h' file not found |

  Scenario: C/C++ Clang include path
    Given a file "include/my-header.h":
      """
      void f(int);
      """
    And a buffer "test.c" in c-mode:
      """
      #include <my-header.h>
      """
    When I set flycheck-clang-include-path to ("./include")
    And I check syntax
    Then I should see no Flycheck errors

  Scenario: C/C++ Clang custom includes
    Given a file "include/my-header.h":
      """
      #define BAR
      """
    And a buffer "includes.c++" in c++-mode:
      """
      int main() {
      #if !defined(BAR)
      return 0;
      #else
      int *foo
      #endif
      }
      """
    When I set flycheck-clang-includes to ("include/my-header.h")
    When I check syntax
    Then I should see Flycheck errors:
      | filename     | checker     | level | line | column | message                            |
      | includes.c++ | c/c++-clang | error |    5 |      9 | expected ';' at end of declaration |

  Scenario: C/C++ Clang custom definitions
    Given a buffer "definitions.c++" in c++-mode:
      """
      int main() {
      #if !defined(BAR) && !defined(FOO)
      return 0;
      #else
      int *foo
      #endif
      }
      """
    When I set flycheck-clang-definitions to ("BAR" "FOO")
    When I check syntax
    Then I should see Flycheck errors:
      | filename        | checker     | level | line | column | message                            |
      | definitions.c++ | c/c++-clang | error |    5 |      9 | expected ';' at end of declaration |

  Scenario: C/C++ Clang language standard
    Given a buffer "standard.c++" in c++-mode:
      """
      int main()
      {
          int *foo = nullptr;
      }
      """
    When I set flycheck-clang-language-standard to "c++11"
    And I disable checker c/c++-cppcheck
    And I check syntax
    Then I should see Flycheck errors:
      | filename     | checker     | level   | line | column | message               |
      | standard.c++ | c/c++-clang | warning |    3 |     10 | unused variable 'foo' |

  Scenario: C/C++ Cppcheck warnings and errors
    Given a buffer "cppcheck.c" in c-mode:
      """
      bool f(bool x)
      {
          int* nil = NULL;
          int y = *nil;
          int z;
          return x == y;
      }
      """
    When I disable checker c/c++-clang
    And I check syntax
    Then I should see Flycheck errors:
      | filename   | checker        | level   | line | message                                                                                                  |
      | cppcheck.c | c/c++-cppcheck | error   |    4 | Null pointer dereference                                                                                 |
      | cppcheck.c | c/c++-cppcheck | warning |    5 | Unused variable: z                                                                                       |
      | cppcheck.c | c/c++-cppcheck | warning |    6 | The expression "x" is of type 'bool' and it is compared against a integer value that is neither 1 nor 0. |

  Scenario: C/C++ Cppcheck style suppressed
    Given a buffer "no-style.c" in c++-mode:
      """
      int f(void)
      {
          int z;
          return 1;
      }
      """
    When I disable checker c/c++-clang
    And I set flycheck-cppcheck-checks to nil
    And I check syntax
    Then I should see no Flycheck errors

  Scenario: C/C++ Cppcheck multiple checks
    Given a buffer "multiple.c++" in c++-mode:
      """
      class A {
          A::~A();
      };

      typedef std::vector<int> IntVec;
      void f(const IntVec& v)
      {
          int unused;
          for (IntVec::const_iterator it = v.begin(); it != v.end(); it++) { }
      }
      """
    When I disable checker c/c++-clang
    And I set flycheck-cppcheck-checks to ("performance" "portability")
    And I check syntax
    Then I should see Flycheck errors:
      | filename     | checker        | level   | line | message                                                                                                                                                                                                                                                              |
      | multiple.c++ | c/c++-cppcheck | warning |    2 | Extra qualification 'A::' unnecessary and considered an error by many compilers.                                                                                                                                                                                     |
      | multiple.c++ | c/c++-cppcheck | warning |    9 | Prefix ++/-- operators should be preferred for non-primitive types. Pre-increment/decrement can be more efficient than post-increment/decrement. Post-increment/decrement usually involves keeping a copy of the previous value around and adds a little extra code. |

  Scenario: Coffeescript syntax error
    Given a buffer "syntax-error.coffee" in coffee-mode:
      """
      foo = () ->
        bar "Hello world
        1 + 1
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename            | checker | level | line | column | message             |
      | syntax-error.coffee | coffee  | error |    2 |      7 | missing ", starting |

  Scenario: Coffeelint error
    Given a buffer "error.coffee" in coffee-mode:
      """
      foo = () ->
        throw "Hello world"
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename     | checker           | level | line | message                       |
      | error.coffee | coffee-coffeelint | error |    2 | Throwing strings is forbidden |

  Scenario: Coffeelint error
    Given a file "coffeelint.json":
      """
      {
        "no_throwing_strings" : {
          "level" : "warn"
        }
      }
      """
    And a buffer "warning.coffee" in coffee-mode:
      """
      foo = () ->
        throw "Hello world"
      """
    When I set flycheck-coffeelintrc to "coffeelint.json"
    And I check syntax
    Then I should see Flycheck errors:
      | filename       | checker           | level   | line | message                       |
      | warning.coffee | coffee-coffeelint | warning |    2 | Throwing strings is forbidden |

  Scenario: CSS syntax error
    Given a buffer "syntax-error.css" in css-mode:
      """
      h1
          font-size: 10px;
      }
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename         | checker     | level | line | column | message                                    |
      | syntax-error.css | css-csslint | error |    2 |     16 | Unexpected token '10px' at line 2, col 16.  |
      | syntax-error.css | css-csslint | error |    2 |     16 | Expected LBRACE at line 2, col 16.          |
      | syntax-error.css | css-csslint | error |    2 |     20 | Unexpected token ';' at line 2, col 20.     |
      | syntax-error.css | css-csslint | error |    3 |      1 | Unexpected token '}' at line 3, col 1.      |

  Scenario: CSS CSSlint warning
    Given a buffer "warning.css" in css-mode:
      """
      .foo h1 {
          font-size: 10px;
      }
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename    | checker     | level   | line | column | message                               |
      | warning.css | css-csslint | warning |    1 |      6 | Heading (h1) should not be qualified. |

  Scenario: DMD syntax error
    Given a buffer "syntax-error.d" in d-mode:
      """
      module d_dmd_syntax_error;
      import std.studio;

      void main()
      {
          writeln("Hello, world!");
      }
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename       | checker | level | line | message                                                      |
      | syntax-error.d | d-dmd   | error |    2 | module studio is in file 'std/studio.d' which cannot be read |

  Scenario: DMD syntax error without module
    Given a buffer "no_module.d" in d-mode:
      """
      import std.stdio;

      void main()
      {
          writel("Hello, world!");
      }
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename    | checker | level | line | message                                                                                       |
      | no_module.d | d-dmd   | error |    5 | undefined identifier writel, did you mean template write(T...)(T args) if (!is(T[0] : File))? |

  Scenario: DMD warning
    Given a buffer "warning.d" in d-mode:
      """
      module warning;

      auto foo(int a)
      {
          return a;
          return a;
      }
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename  | checker | level   | line | message                    |
      | warning.d | d-dmd   | warning |    6 | statement is not reachable |

  Scenario: DMD deprecation
    Given a buffer "deprecated_function.d" in d-mode:
      """
      module deprecated_function;

      deprecated
      auto foo(int a)
      {
          return a;
      }

      void main()
      {
          auto bar = foo(1);
      }
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename              | checker | level   | line | message                                        |
      | deprecated_function.d | d-dmd   | warning |   11 | function deprecated_function.foo is deprecated |

  Scenario: Elixir error
    Given a buffer "error.ex" in elixir-mode:
      """
      defmodule AnError do
        def error_func do
          puts "Flycheck is great!"
        end
      end
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename | checker | level | line | message                   |
      | error.ex | elixir  | error |    3 | function puts/1 undefined |

  Scenario: Elixir warnings
    Given a buffer "warnings.ex" in elixir-mode:
      """
      defmodule Shadowed do
        def func() do
          a = 1
          fn(^a) -> a end
        end
      end
      
      defmodule AlwaysMatch do
        def func(_) do
          IO.puts "Flycheck is great!"
        end
        def func(:atom) do
          IO.puts "Cannot get here."
        end
      end
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename    | checker | level   | line | message                                                                      |
      | warnings.ex | elixir  | warning |    3 | variable a is unused                                                         |
      | warnings.ex | elixir  | warning |    4 | variable a shadowed in 'fun'                                                 |
      | warnings.ex | elixir  | warning |   12 | this clause cannot match because a previous clause at line 9 always matches   |

  Scenario: Erlang error
    Given a buffer "error.erl" in erlang-mode:
      """
      -module('erlang-error').
      -compile(export_all).
      
      great_func() ->
          io:format("Flycheck is great!");
      error_func() ->
          'head-mismatch'.
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename  | checker | level | line | message       |
      | error.erl | erlang  | error |    6 | head mismatch |

  Scenario: Erlang warning
    Given a buffer "warning.erl" in erlang-mode:
      """
      -module('erlang-warning').
      -compile(export_all).
      
      simple_warning() ->
          io:format("Flycheck is great!", ["unused argument"]).
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename    | checker | level   | line | message                                  |
      | warning.erl | erlang  | warning |    5 | wrong number of arguments in format call |

  Scenario: Go syntax error
    Given a buffer "syntax-error.go" in go-mode:
      """
      package main
      
      func ta ta() {
      }      
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename        | checker  | level | line | column | message                        |
      | syntax-error.go | go-gofmt | error |    3 |      9 | expected '(', found 'IDENT' ta |
      | syntax-error.go | go-gofmt | error |    4 |      1 | expected ')', found '}'        |

  Scenario: Go build error
    Given a buffer "go-testpackage/build-error.go" in go-mode:
      """
      package testpackage
      
      func Foo() {
          fmt.Println("foo")
      }
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename                      | checker  | level | line | message        |
      | go-testpackage/build-error.go | go-build | error |    4 | undefined: fmt |

  Scenario: Go test error
    Given a buffer "go-testpackage/test-error_test.go" in go-mode:
      """
      package testpackage
      
      import "testing"
      
      func TestFoo(t *testing.T) {
          fmt.Println("foo")
      }
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename                          | checker | level | line | message        |
      | go-testpackage/test-error_test.go | go-test | error |    6 | undefined: fmt |

  Scenario: HAML
    Given a buffer "error.haml" in haml-mode:
      """
      %%html
        %%head
         %%title Bad indent!      
      """
    When I check syntax
    Then I should see Flycheck errors:
      | checker | level | line | message                                                                                                            |
      | haml    | error |    3 | Inconsistent indentation: 3 spaces used for indentation, but the rest of the document was indented using 2 spaces. |

  Scenario: Haskell GHC error
    Given a buffer "error.hs" in haskell-mode:
      """
      bogus

      module Foo where
      """
    When I disable checker haskell-hdevtools
    And I check syntax
    Then I should see Flycheck errors:
      | filename | checker     | level | line | column | message                       |
      | error.hs | haskell-ghc | error |    3 |      1 | parse error on input `module' |

  Scenario: Haskell GHC warning
    Given a buffer "warning.hs" in haskell-mode:
      """
      module Foo where
      
      foo = 10 :: Integer
      """
    When I disable checker haskell-hdevtools
    And I check syntax
    Then I should see Flycheck errors:
      | filename   | checker     | level   | line | column | message                                                  |
      | warning.hs | haskell-ghc | warning |    3 |      1 | Top-level binding with no type signature: foo :: Integer |
    
  Scenario: Haskell HLint
    Given a buffer "hlint.hs" in haskell-mode:
      """
      module Foo
      where

      warnMe :: [String] -> [[String]]
      warnMe xs = map lines xs
      
      main :: IO ()
      main = (putStrLn "Foobar")
      """
    When I check syntax
    Then I should see Flycheck errors:
      | filename | checker       | level   | line | column | message                                                                         |
      | hlint.hs | haskell-hlint | error   |    5 |      1 | Eta reduce\nFound:\n  warnMe xs = map lines xs\nWhy not:\n  warnMe = map lines  |
      | hlint.hs | haskell-hlint | warning |    8 |      8 | Redundant bracket\nFound:\n  (putStrLn "Foobar")\nWhy not:\n  putStrLn "Foobar" |
