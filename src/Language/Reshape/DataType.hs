module Language.Reshape.DataType where


data Value = VInt Integer
           | VFloat Double
           | VString String
           | VList [Value]
           | VSource String String
           | VQuery String
           | VRsModel
