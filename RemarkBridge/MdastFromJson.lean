import RemarkBridge.Mdast
import Lean.Data.Json

open Mdast Lean

namespace Mdast

private def getStr (j : Json) (key : String) : Option String :=
  match j.getObjValAs? String key with
  | .ok s => some s
  | .error _ => none

private def getNat (j : Json) (key : String) : Option Nat :=
  match j.getObjValAs? Nat key with
  | .ok n => some n
  | .error _ => none

private def getBool (j : Json) (key : String) : Option Bool :=
  match j.getObjValAs? Bool key with
  | .ok b => some b
  | .error _ => none

private def parsePoint (j : Json) : Except String Point := do
  let line ← j.getObjValAs? Nat "line"
  let column ← j.getObjValAs? Nat "column"
  let offset := getNat j "offset"
  return { line, column, offset }

private def parsePosition (j : Json) : Except String Position := do
  let startJson ← j.getObjVal? "start"
  let endJson ← j.getObjVal? "end"
  let start ← parsePoint startJson
  let end_ ← parsePoint endJson
  return { start, end_ }

private def getPosition (j : Json) : Option Position :=
  match j.getObjVal? "position" with
  | .ok posJson =>
    match parsePosition posJson with
    | .ok pos => some pos
    | .error _ => none
  | .error _ => none

private def parseAlignType (s : String) : Option AlignType :=
  match s with
  | "left" => some .left
  | "center" => some .center
  | "right" => some .right
  | _ => none

private def parseReferenceType (s : String) : Except String ReferenceType :=
  match s with
  | "shortcut" => .ok .shortcut
  | "collapsed" => .ok .collapsed
  | "full" => .ok .full
  | other => .error s!"unknown referenceType: {other}"

private partial def fromJsonAux (j : Json) : Except String MdastNode := do
  let type ← j.getObjValAs? String "type"
  let pos := getPosition j
  let parseChildren : Except String (Array MdastNode) := do
    match j.getObjVal? "children" with
    | .ok (Json.arr arr) => arr.mapM fromJsonAux
    | .ok _ => .error "children is not an array"
    | .error _ => return #[]
  match type with
  | "root" =>
    let children ← parseChildren
    return .root children pos
  | "heading" =>
    let depth ← j.getObjValAs? Nat "depth"
    let children ← parseChildren
    return .heading depth children pos
  | "paragraph" =>
    let children ← parseChildren
    return .paragraph children pos
  | "blockquote" =>
    let children ← parseChildren
    return .blockquote children pos
  | "list" =>
    let children ← parseChildren
    return .list (getBool j "ordered") (getNat j "start") (getBool j "spread") children pos
  | "listItem" =>
    let children ← parseChildren
    return .listItem (getBool j "checked") (getBool j "spread") children pos
  | "table" =>
    let align : Option (Array (Option AlignType)) :=
      match j.getObjVal? "align" with
      | .ok (Json.arr arr) => some (arr.map fun v =>
          match v with
          | Json.str s => parseAlignType s
          | _ => none)
      | _ => none
    let children ← parseChildren
    return .table align children pos
  | "tableRow" =>
    let children ← parseChildren
    return .tableRow children pos
  | "tableCell" =>
    let children ← parseChildren
    return .tableCell children pos
  | "strong" =>
    let children ← parseChildren
    return .strong children pos
  | "emphasis" =>
    let children ← parseChildren
    return .emphasis children pos
  | "delete" =>
    let children ← parseChildren
    return .delete children pos
  | "link" =>
    let url ← j.getObjValAs? String "url"
    let title := getStr j "title"
    let children ← parseChildren
    return .link url title children pos
  | "linkReference" =>
    let identifier ← j.getObjValAs? String "identifier"
    let label := getStr j "label"
    let refTypeStr ← j.getObjValAs? String "referenceType"
    let refType ← parseReferenceType refTypeStr
    let children ← parseChildren
    return .linkReference identifier label refType children pos
  | "image" =>
    let url ← j.getObjValAs? String "url"
    let title := getStr j "title"
    let alt := getStr j "alt"
    return .image url title alt pos
  | "imageReference" =>
    let identifier ← j.getObjValAs? String "identifier"
    let label := getStr j "label"
    let refTypeStr ← j.getObjValAs? String "referenceType"
    let refType ← parseReferenceType refTypeStr
    let alt := getStr j "alt"
    return .imageReference identifier label refType alt pos
  | "footnoteDefinition" =>
    let identifier ← j.getObjValAs? String "identifier"
    let label := getStr j "label"
    let children ← parseChildren
    return .footnoteDefinition identifier label children pos
  | "footnoteReference" =>
    let identifier ← j.getObjValAs? String "identifier"
    let label := getStr j "label"
    return .footnoteReference identifier label pos
  | "text" =>
    let value ← j.getObjValAs? String "value"
    return .text value pos
  | "code" =>
    let value ← j.getObjValAs? String "value"
    let lang := getStr j "lang"
    let meta_ := getStr j "meta"
    return .code value lang meta_ pos
  | "inlineCode" =>
    let value ← j.getObjValAs? String "value"
    return .inlineCode value pos
  | "html" =>
    let value ← j.getObjValAs? String "value"
    return .html value pos
  | "yaml" =>
    let value ← j.getObjValAs? String "value"
    return .yaml value pos
  | "definition" =>
    let identifier ← j.getObjValAs? String "identifier"
    let label := getStr j "label"
    let url ← j.getObjValAs? String "url"
    let title := getStr j "title"
    return .definition identifier label url title pos
  | "thematicBreak" =>
    return .thematicBreak pos
  | "break" =>
    return .break_ pos
  | other => .error s!"unknown node type: {other}"

/-- Parse a JSON string into an MdastNode. -/
def fromJson (json : Json) : Except String MdastNode := fromJsonAux json

end Mdast
