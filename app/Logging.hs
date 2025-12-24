module Logging where

import String.ANSI
import System.IO

noticeLabel = "[ " ++ brightMagenta "NOTICE" ++ " ] "
warningLabel = "[ " ++ yellow "WARNING" ++ " ] "

noticeM title msg = hPutStrLn stderr $ noticeLabel ++ title ++ " : " ++ msg
warningM title msg = hPutStrLn stderr $ warningLabel ++ title ++ " : " ++ msg
