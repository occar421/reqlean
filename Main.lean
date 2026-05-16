import Lean
import RemarkBridge
import Reqlean
import Log.Log

open Lean

def main : IO Unit := do
  let logger : Logger := { minLevel := .debug }

  let result <- process "./README.md"
  match result with
  | .ok result => logger.info s!"{result.toFormat}"
  | .error e => logger.error e
