module Reshape.Engine.JSON where

data JSON = JSON String

data JSONQueryContext = JSONQueryContext { source :: String
                                         , foo    :: Int
                                         , json   :: JSON
                                         }

load :: String -> JSONQueryContext
load sourcePath = JSONQueryContext { source = sourcePath
                                   , foo = 0
                                   , json = JSON "hello world"
                                   }

query :: JSONQueryContext -> String -> JSONQueryContext
query (c@JSONQueryContext{..}) q = c{ foo = foo + length q }

