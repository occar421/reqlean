import Lean
import RemarkBridge.Mdast
import RemarkBridge.MdastFromJson

open Lean Mdast

def process (path : System.FilePath) : ExceptT String IO Mdast.MdastNode := do
  let path <- IO.FS.realPath path
   
  let spawnArgs: IO.Process.SpawnArgs := {
    cmd := "bun.exe",
    args := #["run", "main", path.toString],
    cwd := pure $ "." / "remark-bridge"
  }
  let output <- IO.Process.output spawnArgs
  
  if output.exitCode != 0 then
    throw output.stderr
  
  let json <- liftExcept <| Json.parse output.stdout
  let ast <- liftExcept <| Mdast.fromJson json
  
  -- TODO: implement logic
  
  pure $ ast -- temporary code to debug 