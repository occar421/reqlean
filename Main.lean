import RemarkBridge
import Reqlean

def main : IO Unit := do
  let result <- process
  match result with
    | .text a => IO.println s!"Result: {a}!"
    | _ => IO.println s!"Unhandled result. Hello, {hello}!"
