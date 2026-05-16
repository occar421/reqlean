/-- ログレベルの定義 -/
inductive LogLevel
  | debug
  | info
  | warn
  | error
  deriving Inhabited, BEq

def LogLevel.toNat : LogLevel → Nat
  | .debug => 0
  | .info  => 1
  | .warn  => 2
  | .error => 3

instance : LT LogLevel where
  lt a b := a.toNat < b.toNat

instance (a b : LogLevel) : Decidable (a < b) :=
  show Decidable (a.toNat < b.toNat) from inferInstance
