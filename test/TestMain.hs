module Main where
import CHPath.Test.Tests
import System.Exit(exitFailure, exitSuccess)
import Test.HUnit(Counts(..))

main = do
  Counts{errors=errors,failures=failures} <- runAll
  if failures > 0 || errors > 0 then exitFailure else exitSuccess
