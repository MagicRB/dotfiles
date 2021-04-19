{-# LANGUAGE TemplateHaskell #-}
module Shell where
import Shh
$(loadEnv SearchPath)

e :: Cmd
e = emacsclient
