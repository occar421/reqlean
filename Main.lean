import Lean
import RemarkBridge
import Reqlean
import Log.Logger

open Lean

def main : IO Unit := do
  let logger : Logger := { minLevel := .debug }

  let path <- IO.FS.realPath "./README.md"
  logger.info s!"Read: {path}"

  let result <- process path
  
  match result with
  | .error e => logger.error e
  | .ok result =>
    logger.debug s!"{result.toFormat}"
    -- TODO implement logic
