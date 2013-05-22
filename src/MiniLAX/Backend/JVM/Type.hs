-- |
module MiniLAX.Backend.JVM.Type where

--
import Data.List (intercalate)

type Package = [String]

data QName = QName Package String 

data JVMType = JVMInt
             | JVMBoolean
             | JVMChar
             | JVMShort
             | JVMFloat
             | JVMDouble
             | JVMVoid
             | JVMClass QName
             | JVMArray JVMType
             
data Method = Method String [JVMType] JVMType

printPackage :: Package -> String
printPackage = intercalate "/"

instance Show QName where
    show (QName p n) = printPackage p ++ "/" ++ n
    
instance Show JVMType where
    show JVMInt     = "I"
    show JVMBoolean = "Z" 
    show JVMChar    = "C"
    show JVMShort   = "S"
    show JVMFloat   = "F"
    show JVMDouble  = "D"
    show JVMVoid    = "V"
    show (JVMClass n) = 'L' : show n ++ ";"
    show (JVMArray t) = '[' : show t
    
instance Show Method where
    show (Method n args t) = 
        show n ++ "(" ++ argTypes ++ ")" ++ show t
        where argTypes = concatMap show args  
    