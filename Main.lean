import RemarkBridge
import Reqlean

def main : IO Unit := do
  let result <- process
  match result with
  | .ok result => dbg_trace result.toFormat
  | .error e => dbg_trace e
