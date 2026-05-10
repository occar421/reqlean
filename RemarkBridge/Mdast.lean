/-
  Lean representation of mdast (Markdown Abstract Syntax Tree).
  Based on https://github.com/DefinitelyTyped/DefinitelyTyped/blob/master/types/mdast/index.d.ts
-/

namespace Mdast

-- ## Enumerations

inductive AlignType where
  | left
  | center
  | right
  deriving Repr, BEq, Inhabited

inductive ReferenceType where
  | shortcut
  | collapsed
  | full
  deriving Repr, BEq, Inhabited

-- ## Unist base types

structure Point where
  line : Nat
  column : Nat
  offset : Option Nat := none
  deriving Repr, BEq, Inhabited

structure Position where
  start : Point
  end_ : Point
  deriving Repr, BEq, Inhabited

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

end Mdast
