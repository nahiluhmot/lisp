module Lisp.Prelude (defPrelude) where

import Lisp.Prelude.Bool
import Lisp.Prelude.Error
import Lisp.Prelude.Constants
import Lisp.Prelude.IO
import Lisp.Prelude.List
import Lisp.Prelude.Macros
import Lisp.Prelude.Math
import Lisp.Prelude.Meta
import Lisp.Data

defPrelude :: LispM ()
defPrelude = do
  defPreludeBool
  defPreludeConstants
  defPreludeError
  defPreludeIO
  defPreludeList
  defPreludeMacros
  defPreludeMath
  defPreludeMeta
