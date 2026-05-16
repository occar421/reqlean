def reset     : String := "\x1b[0m"
def black     : String := "\x1b[30m"
def red       : String := "\x1b[31m"
def green     : String := "\x1b[32m"
def yellow    : String := "\x1b[33m"
def blue      : String := "\x1b[34m"
def magenta   : String := "\x1b[35m"
def cyan      : String := "\x1b[36m"
def white     : String := "\x1b[37m"
def gray      : String := "\x1b[90m"

/-- 文字列を指定した色で挟むヘルパー関数 -/
def colorize (color : String) (msg : String) : String :=
  s!"{color}{msg}{reset}"