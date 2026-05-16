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

/-- ロガーの設定を保持する構造体 -/
structure Logger where
  minLevel : LogLevel := .info

namespace Logger

/-- 共通のログ出力内部関数 -/
def log (logger : Logger) (level : LogLevel) (msg : String) : IO Unit := do
  -- 設定された最小レベル未満のログは無視する
  if level < logger.minLevel then
    return ()

  let messagePrefix := match level with
    | .debug => "[DEBUG]"
    | .info  => "[INFO] "
    | .warn  => "[WARN] "
    | .error => "[ERROR]"

  -- エラーと警告は標準エラー出力(stderr)、それ以外は標準出力(stdout)へ
  if level == .error || level == .warn then
    (← IO.getStderr).putStrLn s!"{messagePrefix} {msg}"
  else
    (← IO.getStdout).putStrLn s!"{messagePrefix} {msg}"

-- 各レベル用のショートカット関数
def debug (logger : Logger) (msg : String) : IO Unit := logger.log .debug msg
def info  (logger : Logger) (msg : String) : IO Unit := logger.log .info msg
def warn  (logger : Logger) (msg : String) : IO Unit := logger.log .warn msg
def error (logger : Logger) (msg : String) : IO Unit := logger.log .error msg

end Logger