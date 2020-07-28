{-# OPTIONS_GHC -fno-warn-orphans #-}

module NgxExport.Tools.Subrequest.Internal where

import Data.Binary
import Data.CaseInsensitive

instance (FoldCase a, Binary a) => Binary (CI a) where
    get = fmap mk get
    put = put . original

