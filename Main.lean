import RemarkBridge
import Reqlean

def main : IO Unit := do
  let result <- process
  dbg_trace result