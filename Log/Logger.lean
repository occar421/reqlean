import Log.LogLevel
import Log.Color

/-- ロガーの設定を保持する構造体 -/
structure Logger where
  minLevel : LogLevel := .info
  useColor : Bool := true

/-- 共通のログ出力内部関数 -/
def Logger.log (logger : Logger) (level : LogLevel) (msg : String) : IO Unit := do
  -- 設定された最小レベル未満のログは無視する
  if level < logger.minLevel then
    return ()

  let messagePrefix :=
    if logger.useColor then
      match level with
        | .debug => colorize gray   "[DEBUG]"
        | .info  => colorize green  "[INFO] "
        | .warn  => colorize yellow "[WARN] "
        | .error => colorize red    "[ERROR]"
    else
      match level with
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
def Logger.debug (logger : Logger) (msg : String) : IO Unit := logger.log .debug msg
def Logger.info  (logger : Logger) (msg : String) : IO Unit := logger.log .info msg
def Logger.warn  (logger : Logger) (msg : String) : IO Unit := logger.log .warn msg
def Logger.error (logger : Logger) (msg : String) : IO Unit := logger.log .error msg
