module Config.Error
    ( ConfigError(..)
    ) where

import Control.Exception (IOException)
import Toml (TomlDecodeError)

data ConfigError
    = IOError IOException
    | Disabled
    | InvalidPath
    | MissingApiKey
    | ParseError [TomlDecodeError]
    deriving (Show, Eq)
