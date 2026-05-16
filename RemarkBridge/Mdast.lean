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

instance : ToFormat AlignType where
  format
    | .left => "left"
    | .center => "center"
    | .right => "right"

instance : ToString AlignType where
  toString a := f!"{format a}".pretty

inductive ReferenceType where
  | shortcut
  | collapsed
  | full
  deriving Repr, BEq, Inhabited

instance : ToFormat ReferenceType where
  format
    | .shortcut => "shortcut"
    | .collapsed => "collapsed"
    | .full => "full"

instance : ToString ReferenceType where
  toString r := f!"{format r}".pretty

-- ## Unist base types

structure Point where
  line : Nat
  column : Nat
  offset : Option Nat := none
  deriving Repr, BEq, Inhabited

instance : ToFormat Point where
  format p :=
    let off := match p.offset with | some o => f!", offset: {o}" | none => f!""
    f!"\{line: {p.line}, column: {p.column}{off}}"

instance : ToString Point where
  toString p := f!"{format p}".pretty

structure Position where
  start : Point
  end_ : Point
  deriving Repr, BEq, Inhabited

instance : ToFormat Position where
  format p :=
    f!"\{start: {format p.start}, end: {format p.end_}}"

instance : ToString Position where
  toString p := f!"{format p}".pretty

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

-- ## Format Helpers for Option types

private def addOpt {α : Type} [ToFormat α] (attrs : List (String × Format)) (k : String) (opt : Option α) : List (String × Format) :=
  match opt with
  | some v => attrs ++ [(k, format v)]
  | none   => attrs

private def addOptStr (attrs : List (String × Format)) (k : String) (opt : Option String) : List (String × Format) :=
  match opt with
  | some v => attrs ++ [(k, f!"\"{v}\"")]
  | none   => attrs

-- ## Mutual formatting logic

mutual
partial def MdastNode.toFormat : MdastNode → Format
  | .root cs p =>
    let attrs := addOpt [] "position" p
    formatNode "root" attrs cs

  | .heading d cs p =>
    let attrs := [("depth", f!"{d}")]
    let attrs := addOpt attrs "position" p
    formatNode "heading" attrs cs

  | .paragraph cs p =>
    let attrs := addOpt [] "position" p
    formatNode "paragraph" attrs cs

  | .blockquote cs p =>
    let attrs := addOpt [] "position" p
    formatNode "blockquote" attrs cs

  | .list ord st sp cs p =>
    let attrs := addOpt [] "ordered" ord
    let attrs := addOpt attrs "start" st
    let attrs := addOpt attrs "spread" sp
    let attrs := addOpt attrs "position" p
    formatNode "list" attrs cs

  | .listItem ck sp cs p =>
    let attrs := addOpt [] "checked" ck
    let attrs := addOpt attrs "spread" sp
    let attrs := addOpt attrs "position" p
    formatNode "listItem" attrs cs

  | .table al cs p =>
    let attrs := match al with
      | some arr =>
        let fmts := arr.toList.map fun a => match a with | some v => format v | none => "none"
        let alFmt := f!"[{Format.joinSep fmts ", "}]"
        [("align", alFmt)]
      | none => []
    let attrs := addOpt attrs "position" p
    formatNode "table" attrs cs

  | .tableRow cs p =>
    let attrs := addOpt [] "position" p
    formatNode "tableRow" attrs cs

  | .tableCell cs p =>
    let attrs := addOpt [] "position" p
    formatNode "tableCell" attrs cs

  | .strong cs p =>
    let attrs := addOpt [] "position" p
    formatNode "strong" attrs cs

  | .emphasis cs p =>
    let attrs := addOpt [] "position" p
    formatNode "emphasis" attrs cs

  | .delete cs p =>
    let attrs := addOpt [] "position" p
    formatNode "delete" attrs cs

  | .link url ti cs p =>
    let attrs := [("url", f!"\"{url}\"")]
    let attrs := addOptStr attrs "title" ti
    let attrs := addOpt attrs "position" p
    formatNode "link" attrs cs

  | .linkReference ident lbl rt cs p =>
    let attrs := [("identifier", f!"\"{ident}\"")]
    let attrs := addOptStr attrs "label" lbl
    let attrs := attrs ++ [("referenceType", format rt)]
    let attrs := addOpt attrs "position" p
    formatNode "linkReference" attrs cs

  | .image url ti alt p =>
    let attrs := [("url", f!"\"{url}\"")]
    let attrs := addOptStr attrs "title" ti
    let attrs := addOptStr attrs "alt" alt
    let attrs := addOpt attrs "position" p
    formatNode "image" attrs #[]

  | .imageReference ident lbl rt alt p =>
    let attrs := [("identifier", f!"\"{ident}\"")]
    let attrs := addOptStr attrs "label" lbl
    let attrs := attrs ++ [("referenceType", format rt)]
    let attrs := addOptStr attrs "alt" alt
    let attrs := addOpt attrs "position" p
    formatNode "imageReference" attrs #[]

  | .footnoteDefinition ident lbl cs p =>
    let attrs := [("identifier", f!"\"{ident}\"")]
    let attrs := addOptStr attrs "label" lbl
    let attrs := addOpt attrs "position" p
    formatNode "footnoteDefinition" attrs cs

  | .footnoteReference ident lbl p =>
    let attrs := [("identifier", f!"\"{ident}\"")]
    let attrs := addOptStr attrs "label" lbl
    let attrs := addOpt attrs "position" p
    formatNode "footnoteReference" attrs #[]

  | .text v p =>
    let attrs := [("value", f!"\"{v}\"")]
    let attrs := addOpt attrs "position" p
    formatNode "text" attrs #[]

  | .code v lang mt p =>
    let attrs := [("value", f!"\"{v}\"")]
    let attrs := addOptStr attrs "lang" lang
    let attrs := addOptStr attrs "meta" mt
    let attrs := addOpt attrs "position" p
    formatNode "code" attrs #[]

  | .inlineCode v p =>
    let attrs := [("value", f!"\"{v}\"")]
    let attrs := addOpt attrs "position" p
    formatNode "inlineCode" attrs #[]

  | .html v p =>
    let attrs := [("value", f!"\"{v}\"")]
    let attrs := addOpt attrs "position" p
    formatNode "html" attrs #[]

  | .yaml v p =>
    let attrs := [("value", f!"\"{v}\"")]
    let attrs := addOpt attrs "position" p
    formatNode "yaml" attrs #[]

  | .definition ident lbl url ti p =>
    let attrs := [("identifier", f!"\"{ident}\"")]
    let attrs := addOptStr attrs "label" lbl
    let attrs := attrs ++ [("url", f!"\"{url}\"")]
    let attrs := addOptStr attrs "title" ti
    let attrs := addOpt attrs "position" p
    formatNode "definition" attrs #[]

  | .thematicBreak p =>
    let attrs := addOpt [] "position" p
    formatNode "thematicBreak" attrs #[]

  | .break_ p =>
    let attrs := addOpt [] "position" p
    formatNode "break" attrs #[]

-- ヘルパー：属性と子要素をまとめてフォーマットする
private partial def formatNode (name : String) (attrs : List (String × Format) := []) (children : Array MdastNode := #[]) : Format :=
  let attrFmt := attrs.map fun (k, v) => f!"{k}: {v}"
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

-- ## Core Instances

instance : ToFormat MdastNode where
  format := MdastNode.toFormat

instance : ToString MdastNode where
  toString n := f!"{format n}".pretty

end Mdast
