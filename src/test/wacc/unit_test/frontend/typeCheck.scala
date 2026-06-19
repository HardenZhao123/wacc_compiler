package wacc.unit_test.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}

import wacc.frontend.parser
import wacc.frontend.typeCheck.TypeChecker
import wacc.common.ExitCode

class TypeCheckerTest extends AnyFlatSpec {

  private val p = parser

  private def shouldExit(src: String, expected: Int): Unit =
    p.parseProgram(src) match {
      case Success(ast) =>
        TypeChecker.checkProgram(ast)
        val exitCode = if (TypeChecker.getErrors().nonEmpty) 200 else 0
        withClue("\n" + TypeChecker.getErrors().map(_.toString).mkString("\n")) {
          exitCode shouldBe expected
        }
      case Failure(err) =>
        fail(s"parse failed: $err")
    }

  "type checker" should "accept begin skip end" in {
    shouldExit("begin skip end", ExitCode.Success)
  }

  it should "reject the program when it has wrong array dimensions" in {
    val src =
      """
      begin
        int[] a = [1, 2];
        int oops = a[1][2]
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  //unary operators
  it should "reject ! on int" in {
    val src =
      """
      begin
        int x = 1;
        bool b = !x
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "reject ord on int" in {
    val src =
      """
      begin
        int x = ord 1
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "reject chr on char" in {
    val src =
      """
      begin
        char c = chr 'a'
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "reject len on non-array" in {
    val src =
      """
      begin
        int x = len 123
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept len on array" in {
    val src =
      """
      begin
        int[] a = [1,2,3];
        int n = len a
      end
      """
    shouldExit(src, ExitCode.Success)
  }


  //binary operators
  it should "reject arithmetic on bool" in {
    val src =
      """
      begin
        bool b = true;
        int x = b + 1
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept char comparisons" in {
    val src =
      """
      begin
        bool b = 'a' < 'b'
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "reject comparison between int and bool" in {
    val src =
      """
      begin
        bool b = 1 < true
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  // equality: both sides must weaken to same type (string <-> char[] is OK)
  it should "accept equality between string and char[]" in {
    val src =
      """
      begin
        char[] cs = ['h','i'];
        string s = "hi";
        bool b = cs == s
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "reject equality between int[] and char[]" in {
    val src =
      """
      begin
        int[] a = [1,2];
        char[] b = ['a','b'];
        bool x = a == b
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  //if/while condition typing
  it should "reject if condition not bool" in {
    val src =
      """
      begin
        if 1 then skip else skip fi
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "reject while condition not bool" in {
    val src =
      """
      begin
        while 123 do skip done
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  //read/exit/free typing
  it should "reject read into bool" in {
    val src =
      """
      begin
        bool b = true;
        read b
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept read into int and char" in {
    val src =
      """
      begin
        int x = 0;
        char c = 'a';
        read x;
        read c
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "reject exit with non-int" in {
    val src =
      """
      begin
        exit true
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "reject free on string (free only arrays/pairs)" in {
    val src =
      """
      begin
        string s = "hi";
        free s
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept free on arrays and pairs" in {
    val src =
      """
      begin
        int[] a = [1,2];
        pair(int, bool) p = newpair(1, true);
        free a;
        free p
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  //string weakening + array invariance + array literal LCA
  it should "accept assigning char[] to string (weakening)" in {
    val src =
      """
      begin
        char[] cs = ['a','b'];
        string s = cs
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "reject assigning string to char[] (no strengthening)" in {
    val src =
      """
      begin
        string s = "ab";
        char[] cs = s
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "reject char[][] as string[] (array invariance)" in {
    val src =
      """
    begin
      char[] a = ['a'];
      char[] b = ['b'];
      char[][] ccs = [a, b];
      string[] ss = ccs
    end
    """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept array literal LCA (char[] and string => string[])" in {
    val src =
      """
      begin
        char[] cs = ['h','i'];
        string s = "ok";
        string[] arr = [cs, s]
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  //return rules + function calls
  it should "reject return in main body" in {
    val src =
      """
      begin
        return 0
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept correct function return and call" in {
    val src =
      """
      begin
        int f(int x) is
          return x + 1
        end

        int y = call f(41)
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "reject calling function with wrong arg type" in {
    val src =
      """
      begin
        int f(int x) is
          return x
        end

        int y = call f(true)
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "reject calling function with wrong arity (not fully saturated)" in {
    val src =
      """
      begin
        int f(int x, int y) is
          return x + y
        end

        int z = call f(1)
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "reject returning wrong type in function" in {
    val src =
      """
      begin
        int f() is
          return true
        end

        int x = call f()
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept overloaded function calls resolved by argument type" in {
    val src =
      """
      begin
        int f(int x) is
          return x + 1
        end

        bool f(bool x) is
          return !x
        end

        int a = call f(1);
        bool b = call f(true)
      end
      """
    shouldExit(src, ExitCode.Success)
  }


  it should "prefer exact overload over weakened overload" in {
    val src =
      """
      begin
        int kind(string s) is
          return 1
        end

        int kind(char[] s) is
          return 2
        end

        char[] a = ['h', 'i'];
        int x = call kind(a)
      end
      """
    shouldExit(src, ExitCode.Success)
  }
  it should "reject ambiguous overloaded call" in {
    val src =
      """
      begin
        int f(pair(int, int) p) is
          return 1
        end

        bool f(pair(bool, bool) p) is
          return true
        end

        bool b = call f(null)
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }
  //pairs: null + erased pair known-type rule
  it should "accept null as any pair type" in {
    val src =
      """
      begin
        pair(int, bool) p = null
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "reject fst/snd when neither side gives a known type (erased pair ambiguity)" in {
    val src =
      """
      begin
        pair(pair, pair) p = null;
        pair(pair, pair) q = null;
        fst fst p = fst fst q
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept nested fst without parentheses" in {
    val src =
      """
      begin
        pair(pair, int) p = null;
        fst fst p = 1
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "accept char[] stored in string[]" in {
    val src =
      """
      begin
        char[] cs = ['a'];
        string[] ss = [cs]
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "reject string stored in char[][]" in {
    val src =
      """
      begin
        string s = "abc";
        char[][] cs = [s]
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept string and char[] stored in string[]" in {
    val src =
      """
      begin
        string s = "abc";
        char[] cs = ['a'];
        string[] ss = [s, cs]
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "reject string stored into char[][]" in {
    val src =
      """
      begin
        string s = "abc";
        char[][] css = [s]
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept char[] assigned to the string in a pair" in {
    val src =
      """
      begin
        char[] cs = ['a'];
        pair(string, char[]) p = newpair(cs, cs)
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "reject string assigned to the char[] in a pair" in {
    val src =
      """
      begin
        string s = "abc";
        pair(string, char[]) p = newpair(s, s)
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept char[] stored in string in pair, and that pair is in an array" in {
    val src =
      """
      begin
        char[] cs = ['a'];
        pair(string, int) p = newpair(cs, 1);
        pair(string, int)[] ps = [p]
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "accept basic for loop" in {
    val src =
      """
      begin
        int x = 1;
        for (int x = 2, x < 10, x = x + 1)
          println x
        done
      end
      """
    shouldExit(src, ExitCode.Success)
  }
  
  it should "accept a basic do-while loop" in {
    val src =
      """
      begin
        int x = 1;
        do
          int y = x * 2;
          x = x + 1
        while x <= 10
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "accept bitwise for two integers" in {
    val src =
      """
      begin
        int x = 1;
        int y = 2;
        int z = x & y;
        int w = y | z;
        int a = ~w
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "reject bitwise with either two sides not integers" in {
    val src =
      """
      begin
        int x = 1;
        bool y = x & 1 == 2 && true;
        print x & y
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "reject bitwise with wrong variable type" in {
    val src =
      """
      begin
        bool b = 1 & 10
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept break and continue inside loops" in {
    val src =
      """
      begin
        int i = 0;
        while i < 10 do
          i = i + 1;
          if i == 3 then Continue else skip fi;
          if i == 5 then Break else skip fi
        done
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "reject break outside loops" in {
    val src =
      """
      begin
        Break
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept throw with exception of string and reject throw with exception of int" in {
    shouldExit(
      """
      begin
        string s = "array out of bound";
        throw ArrayOutOfBoundsException(s)
      end
      """,
      ExitCode.Success
    )

    shouldExit(
      """
      begin
        int n = 1;
        throw ArrayOutOfBoundsException(n)
      end
      """,
      ExitCode.SemanticError
    )
  }

  it should "accept general exception throw and catch" in {
    val src =
      """
      begin
        try
          throw Exception("general failure")
        catch Exception err do
          print err
        done
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "accept catching implicit arithmetic exception in try-catch" in {
    val src =
      """
      begin
        int n = 0;
        try
          int x = 1 / n;
          println x
        catch ArithmeticException err do
          println err
        done
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "accept catching an exception from a callee" in {
    val src =
      """
      begin
        int boom() is
          throw ArrayOutOfBoundsException("array out of bound")
        end

        try
          int x = call boom()
        catch ArrayOutOfBoundsException err do
          print err
        done
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "reject continue outside loops" in {
    val src =
      """
      begin
        Continue
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }
}


