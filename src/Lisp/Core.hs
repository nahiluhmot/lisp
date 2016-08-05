module Lisp.Core (defCore) where

import Lisp.Core.Bool
import Lisp.Core.Constants
import Lisp.Core.IO
import Lisp.Core.List
import Lisp.Core.Macros
import Lisp.Core.Math
import Lisp.Core.Meta
import Lisp.Data

defCore :: LispM ()
defCore = do
  defCoreBool
  defCoreConstants
  defCoreIO
  defCoreList
  defCoreMacros
  defCoreMath
  defCoreMeta
