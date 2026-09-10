import Test.Hspec

import qualified LexerSpec
import qualified ParserSpec
import qualified ExamplesSpec
import qualified InferSpec
import qualified ClassesSpec
import qualified EvalSpec
import qualified NegativeSpec
import qualified GoldenSpec
import qualified KindSpec


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Testing Lexer" LexerSpec.spec
  describe "Testing Parser" ParserSpec.spec
  describe "Test Language Examples" ExamplesSpec.spec
  describe "Testing Inference" InferSpec.spec
  describe "Testing Classes" ClassesSpec.spec
  describe "Testing Evaluator" EvalSpec.spec
  describe "Testing Negative Cases" NegativeSpec.spec
  describe "Testing Golden Files" GoldenSpec.spec
  describe "Testing Kinds" KindSpec.spec
