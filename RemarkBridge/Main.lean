import RemarkBridge.Mdast

open Mdast

def process: IO Mdast.MdastNode := do
  let spawnArgs: IO.Process.SpawnArgs := {
    cmd := "bun.exe",
    args := #["--version"],
  }
  let output <- IO.Process.output spawnArgs
  dbg_trace output.stdout
  pure $ MdastNode.text "This is from mdast"