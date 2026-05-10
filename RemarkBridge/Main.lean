import RemarkBridge.Mdast

open Mdast

def process: IO Mdast.MdastNode := do
  pure $ MdastNode.text "This is from mdast"