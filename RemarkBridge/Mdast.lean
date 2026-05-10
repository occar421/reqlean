/-
  Lean representation of mdast (Markdown Abstract Syntax Tree).
  Based on https://github.com/DefinitelyTyped/DefinitelyTyped/blob/master/types/mdast/index.d.ts
-/

import Lean

namespace Mdast

open Lean

-- ## Enumerations

inductive AlignType where
  | left
  | center
  | right
  deriving Repr, BEq, Inhabited

instance : ToString AlignType where
  toString
    | .left => "left"
    | .center => "center"
    | .right => "right"

instance : ToFormat AlignType where
  format := fun a => f!"{toString a}"

inductive ReferenceType where
  | shortcut
  | collapsed
  | full
  deriving Repr, BEq, Inhabited

instance : ToString ReferenceType where
  toString
    | .shortcut => "shortcut"
    | .collapsed => "collapsed"
    | .full => "full"

instance : ToFormat ReferenceType where
  format := fun r => f!"{toString r}"

-- ## Unist base types

structure Point where
  line : Nat
  column : Nat
  offset : Option Nat := none
  deriving Repr, BEq, Inhabited

instance : ToString Point where
  toString p :=
    let off := match p.offset with | some o => s!", offset: {o}" | none => ""
    "{line: " ++ toString p.line ++ ", column: " ++ toString p.column ++ off ++ "}"

structure Position where
  start : Point
  end_ : Point
  deriving Repr, BEq, Inhabited

instance : ToString Position where
  toString p := "{start: " ++ toString p.start ++ ", end: " ++ toString p.end_ ++ "}"

-- ## Mdast Node (sum type)

inductive MdastNode where
  | root (children : Array MdastNode) (position : Option Position := none)
  | heading (depth : Nat) (children : Array MdastNode) (position : Option Position := none)
  | paragraph (children : Array MdastNode) (position : Option Position := none)
  | blockquote (children : Array MdastNode) (position : Option Position := none)
  | list (ordered : Option Bool) (start : Option Nat) (spread : Option Bool)
         (children : Array MdastNode) (position : Option Position := none)
  | listItem (checked : Option Bool) (spread : Option Bool)
             (children : Array MdastNode) (position : Option Position := none)
  | table (align : Option (Array (Option AlignType)))
          (children : Array MdastNode) (position : Option Position := none)
  | tableRow (children : Array MdastNode) (position : Option Position := none)
  | tableCell (children : Array MdastNode) (position : Option Position := none)
  | strong (children : Array MdastNode) (position : Option Position := none)
  | emphasis (children : Array MdastNode) (position : Option Position := none)
  | delete (children : Array MdastNode) (position : Option Position := none)
  | link (url : String) (title : Option String)
         (children : Array MdastNode) (position : Option Position := none)
  | linkReference (identifier : String) (label : Option String)
                  (referenceType : ReferenceType)
                  (children : Array MdastNode) (position : Option Position := none)
  | image (url : String) (title : Option String) (alt : Option String)
          (position : Option Position := none)
  | imageReference (identifier : String) (label : Option String)
                   (referenceType : ReferenceType) (alt : Option String)
                   (position : Option Position := none)
  | footnoteDefinition (identifier : String) (label : Option String)
                       (children : Array MdastNode) (position : Option Position := none)
  | footnoteReference (identifier : String) (label : Option String)
                      (position : Option Position := none)
  | text (value : String) (position : Option Position := none)
  | code (value : String) (lang : Option String) (meta_ : Option String)
         (position : Option Position := none)
  | inlineCode (value : String) (position : Option Position := none)
  | html (value : String) (position : Option Position := none)
  | yaml (value : String) (position : Option Position := none)
  | definition (identifier : String) (label : Option String)
               (url : String) (title : Option String)
               (position : Option Position := none)
  | thematicBreak (position : Option Position := none)
  | break_ (position : Option Position := none)
  deriving Repr, Inhabited

mutual
partial def MdastNode.toFormat : MdastNode → Format
  | .root cs _ => formatNode "root" [] cs
  | .heading d cs _ => formatNode "heading" [("depth", f!"{d}")] cs
  | .paragraph cs _ => formatNode "paragraph" [] cs
  | .text v _ => f!"(text value: \"{v}\")"
  | .strong cs _ => formatNode "strong" [] cs
  | .link url ti cs _ => 
    let attrs := [("url", f!"\"{url}\"")]
    let attrs := if let some t := ti then attrs ++ [("title", f!"\"{t}\"")] else attrs
    formatNode "link" attrs cs
  | .code v lang mt _ => codeToFormat v lang mt
  -- 他のケースも同様に formatNode を使って記述...
  | _ => f!"(unimplemented_node)"

private partial def codeToFormat (v: String) (lang mt: Option String) : Format := Id.run do
    let mut attrs := [("value", f!"\"{v}\"")]
    if let some l := lang then attrs := attrs ++ [("lang", f!"\"{l}\"")]
    if let some m := mt then attrs := attrs ++ [("meta", f!"\"{m}\"")]
    return formatNode "code" attrs

-- ヘルパー：属性と子要素をまとめてフォーマットする
private partial def formatNode (name : String) (attrs : List (String × Format) := []) (children : Array MdastNode := #[]) : Format :=
  let attrFmt := attrs.map fun (k, v) => f!"{k}: {v}"
  -- 子要素を再帰的に format する（ここでは後述の toFormat を想定）
  let childFmts := children.toList.map MdastNode.toFormat
  let allItems := attrFmt ++ childFmts
  
  if allItems.isEmpty then
    f!"({name})"
  else
    -- 全体をグループ化し、nest 2 でインデントを付ける
    -- Format.line は「必要があれば改行、なければスペース」として振る舞う
    let body := Format.joinSep allItems ("," ++ Format.line)
    Format.group (f!"({name}" ++ Format.nest 2 (Format.line ++ body) ++ ")")
end

private def optStr (label : String) (o : Option String) : String :=
  match o with | some v => ", " ++ label ++ ": \"" ++ v ++ "\"" | none => ""

private def optBoolStr (label : String) (o : Option Bool) : String :=
  match o with | some v => ", " ++ label ++ ": " ++ toString v | none => ""

private def optNatStr (label : String) (o : Option Nat) : String :=
  match o with | some v => ", " ++ label ++ ": " ++ toString v | none => ""

private def posStr (p : Option Position) : String :=
  match p with | some v => ", position: " ++ toString v | none => ""

private partial def childrenStr (cs : Array MdastNode) : String :=
  ", children: [" ++ ", ".intercalate (cs.toList.map MdastNode.toString) ++ "]"
where
  MdastNode.toString : MdastNode → String
    | .root cs p => "(root" ++ childrenStr cs ++ posStr p ++ ")"
    | .heading d cs p => "(heading, depth: " ++ toString d ++ childrenStr cs ++ posStr p ++ ")"
    | .paragraph cs p => "(paragraph" ++ childrenStr cs ++ posStr p ++ ")"
    | .blockquote cs p => "(blockquote" ++ childrenStr cs ++ posStr p ++ ")"
    | .list ord st sp cs p =>
      "(list" ++ optBoolStr "ordered" ord ++ optNatStr "start" st ++ optBoolStr "spread" sp ++ childrenStr cs ++ posStr p ++ ")"
    | .listItem ck sp cs p =>
      "(listItem" ++ optBoolStr "checked" ck ++ optBoolStr "spread" sp ++ childrenStr cs ++ posStr p ++ ")"
    | .table al cs p =>
      let alStr := match al with
        | some arr => ", align: [" ++ ", ".intercalate (arr.toList.map fun a => match a with | some v => toString v | none => "none") ++ "]"
        | none => ""
      "(table" ++ alStr ++ childrenStr cs ++ posStr p ++ ")"
    | .tableRow cs p => "(tableRow" ++ childrenStr cs ++ posStr p ++ ")"
    | .tableCell cs p => "(tableCell" ++ childrenStr cs ++ posStr p ++ ")"
    | .strong cs p => "(strong" ++ childrenStr cs ++ posStr p ++ ")"
    | .emphasis cs p => "(emphasis" ++ childrenStr cs ++ posStr p ++ ")"
    | .delete cs p => "(delete" ++ childrenStr cs ++ posStr p ++ ")"
    | .link url ti cs p =>
      "(link, url: \"" ++ url ++ "\"" ++ optStr "title" ti ++ childrenStr cs ++ posStr p ++ ")"
    | .linkReference ident lbl rt cs p =>
      "(linkReference, identifier: \"" ++ ident ++ "\"" ++ optStr "label" lbl ++ ", referenceType: " ++ toString rt ++ childrenStr cs ++ posStr p ++ ")"
    | .image url ti alt p =>
      "(image, url: \"" ++ url ++ "\"" ++ optStr "title" ti ++ optStr "alt" alt ++ posStr p ++ ")"
    | .imageReference ident lbl rt alt p =>
      "(imageReference, identifier: \"" ++ ident ++ "\"" ++ optStr "label" lbl ++ ", referenceType: " ++ toString rt ++ optStr "alt" alt ++ posStr p ++ ")"
    | .footnoteDefinition ident lbl cs p =>
      "(footnoteDefinition, identifier: \"" ++ ident ++ "\"" ++ optStr "label" lbl ++ childrenStr cs ++ posStr p ++ ")"
    | .footnoteReference ident lbl p =>
      "(footnoteReference, identifier: \"" ++ ident ++ "\"" ++ optStr "label" lbl ++ posStr p ++ ")"
    | .text v p => "(text, value: \"" ++ v ++ "\"" ++ posStr p ++ ")"
    | .code v lang mt p =>
      "(code, value: \"" ++ v ++ "\"" ++ optStr "lang" lang ++ optStr "meta" mt ++ posStr p ++ ")"
    | .inlineCode v p => "(inlineCode, value: \"" ++ v ++ "\"" ++ posStr p ++ ")"
    | .html v p => "(html, value: \"" ++ v ++ "\"" ++ posStr p ++ ")"
    | .yaml v p => "(yaml, value: \"" ++ v ++ "\"" ++ posStr p ++ ")"
    | .definition ident lbl url ti p =>
      "(definition, identifier: \"" ++ ident ++ "\"" ++ optStr "label" lbl ++ ", url: \"" ++ url ++ "\"" ++ optStr "title" ti ++ posStr p ++ ")"
    | .thematicBreak p => "(thematicBreak" ++ posStr p ++ ")"
    | .break_ p => "(break" ++ posStr p ++ ")"

instance : ToString MdastNode where
  toString := childrenStr.MdastNode.toString

-- TODO: regenerate ToFormat oriented code

end Mdast
