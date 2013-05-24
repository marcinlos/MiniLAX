-- | Code generation monad and functions
-- Well, ok, actually not "code" as in "bytecode", it's jasmin assembly.
module MiniLAX.Backend.JVM.CodeGen where

-- Imports
import Prelude hiding (mapM, mapM_, foldr, foldl, concatMap)
import Control.Monad (void)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Foldable
import Data.Traversable

import MiniLAX.IR
import MiniLAX.IR.Generate (Code)
import MiniLAX.Backend.JVM.Skeleton
import MiniLAX.Backend.JVM.Type
import MiniLAX.Printer
import MiniLAX.Static.Symbols
import MiniLAX.Static.Types hiding (Int2Real, Real2Int)

data Index = ConstIndex Integer 
           | VarIndex Integer
           | NoIndex
    deriving (Show)

data LocalVar = LocalVar { localVarPos :: Integer
                         , localVarIdx :: Index
                         , localVarType :: Type
                         }
    deriving (Show)
                       
type Context = (SMap LocalVar, String, String -> String) 


genJVM :: SMap (Procedure, Code) -> String -> PrinterMonad ()
genJVM procs entry = do
    fileHeader
    classHeader className
    endl
    put ".field private static scanner Ljava/util/Scanner;" >> endl
    endl
    method "static <cinit>()V" $ do
        limitStack 2
        new "java/util/Scanner"
        dup
        invokespecial "java/util/Scanner/<init>()V"
        putstatic (className ++ "/scanner") "Ljava/util/Scanner;"
        returnJ
    void $ mapM mkProc procs  
    javaMain $ do
        invokestatic $ mkSig entry
        returnJ 
    where mkProc = uncurry $ makeProcedure mkSig className
          mkSig = show . fullProcSig className . fst . (procs !)
          className = entry ++ "Class"
            

makeProcedure :: (String -> String) -> String -> Procedure -> Code -> PrinterMonad ()
makeProcedure f clazz p c = 
    method name $ do
        vars <- makeIntro p
        mapM_ (ir2JVM (vars, clazz, f)) c
    where name  = "static " ++ show (procSig p)
          
makeIntro :: Procedure -> PrinterMonad (SMap LocalVar)
makeIntro proc = do
    put "; Procedure preamble" >> endl
    let (args, n) = gatherParams 0 proc
    let (vars, n') = gatherLocals n proc
    let m = vars `M.union` args
    limitLocals n'
    limitStack 1000
    mapM_ (allocParam m) $ procParams proc
    mapM_ allocVar vars 
    --put ("; " ++ show args) >> endl
    --put ("; " ++ show vars) >> endl
    put "; - - - - - - " >> endl
    return m
          
pushIndex :: Index -> PrinterMonad ()
pushIndex (ConstIndex n) = iconst n
pushIndex (VarIndex n)   = iload n
pushIndex NoIndex = return ()

assignLocal :: Integer -> Local -> (Integer, LocalVar)
assignLocal n Local { localType = t } = (n + 1, v)
    where v = LocalVar n idx t
          idx | isArray t = NoIndex
              | otherwise = ConstIndex 0
        
assignParam :: Integer -> Parameter -> (Integer, LocalVar)
assignParam n Parameter { paramType = t, paramKind = kind } =
    case t of
        ArrayT {} -> (n + 1, LocalVar n NoIndex t)
        _ | kind == ByVal -> (n + 1, LocalVar n (ConstIndex 0) t)
          | otherwise     -> (n + 2, LocalVar n (VarIndex $ n + 1) t)
        
gatherLocals :: Integer -> Procedure -> (SMap LocalVar, Integer)
gatherLocals first proc = 
    foldl addOne (M.empty, first) $ procVars proc
    where addOne (m, n) l = let (n', l') = assignLocal n l 
                            in (M.insert (localName l) l' m, n')
                               
gatherParams :: Integer -> Procedure -> (SMap LocalVar, Integer)
gatherParams first proc = 
    foldl addOne (M.empty, first) params
    where addOne (m, n) l = let (n', l') = assignParam n l 
                            in (M.insert (paramName l) l' m, n')
          params = procParams proc  
                                         
allocVar :: LocalVar -> PrinterMonad ()
allocVar l = case localVarType l of
    IntegerT      -> mkArray "int"
    RealT         -> mkArray "float"
    BooleanT      -> mkArray "boolean"
    t @ ArrayT {} -> allocArray t >> astore n
    TypeError     -> error "Internal error: TypeError (allocVar)"
    where mkArray t = do
              iconst 1
              newarray t
              astore n
          n = localVarPos l
          
allocArray :: Type -> PrinterMonad ()
allocArray t @ (ArrayT ArrayT {} _ _) = do
    mapM_ (iconst . uncurry (flip (-))) bounds
    multianewarray tstr dims
    where tstr = show $ typeToJVM t
          bounds = arrayBounds t
          dims   = length bounds 
          
allocArray (ArrayT t low high) = do
    iconst (high - low)
    newarray str
    where str = case t of IntegerT -> "int"
                          BooleanT -> "boolean"
                          RealT    -> "float"
                          _ -> error $ "Internal error: " ++ show t ++ 
                                       "(allocArray)"
    
allocArray _ = error "Internal error: Not an array (allocArray)"
          
allocParam :: SMap LocalVar -> Parameter -> PrinterMonad ()
allocParam vars p @ Parameter { paramKind = ByVal, paramType = t }
  | isArray t = return ()
  | otherwise = do 
      iconst 1
      newarray $ typeToArrayDesc t
      dup
      iconst 0
      load n
      store
      astore n
      where n = localVarPos $ vars ! paramName p
            (store, load) = case t of IntegerT -> (iastore, iload)
                                      RealT    -> (fastore, fload)
                                      BooleanT -> (bastore, iload)
                                      _ -> error "Internal error: (allocParam)"
allocParam _ _ = return ()

ir2JVM :: Context -> IR -> PrinterMonad ()
ir2JVM _ (LoadIntConst n) = iconst n
ir2JVM _ (LoadRealConst x) = fconst x
ir2JVM _ (LoadBoolConst b) = iconst $ if b then 1 else 0
ir2JVM _ FetchArrayInt = iaload
ir2JVM _ FetchArrayReal = faload
ir2JVM _ FetchArrayBool = baload
ir2JVM _ FetchArrayArray = aaload
ir2JVM _ StoreArrayInt = iastore
ir2JVM _ StoreArrayReal = fastore
ir2JVM _ StoreArrayBool = bastore

ir2JVM (vars, _, _) (LoadBool v) = genericLoad vars v >> baload
ir2JVM (vars, _, _) (LoadInt v) = genericLoad vars v >> iaload
ir2JVM (vars, _, _) (LoadReal v) = genericLoad vars v >> faload

ir2JVM (vars, _, _) (LoadBoolVar v) = genericLoad vars v
ir2JVM (vars, _, _) (LoadIntVar v)  = genericLoad vars v
ir2JVM (vars, _, _) (LoadRealVar v) = genericLoad vars v
ir2JVM (vars, _, _) (LoadArray v) = aload $ localVarPos $ vars ! v  

ir2JVM _ (StoreBool _) = bastore
ir2JVM _ (StoreInt _) = iastore
ir2JVM _ (StoreReal _) = fastore

ir2JVM _ (Jump l) = goto (show l) 
ir2JVM _ (PreCall _) = return ()
ir2JVM (_, _, f) (Call name) = invokestatic $ f name  
ir2JVM _ WriteBool = printBool
ir2JVM _ WriteInt  = printInt
ir2JVM _ WriteReal = printFloat 

ir2JVM (_, c, _) ReadBool = readBool c >> bastore
ir2JVM (_, c, _) ReadInt  = readInt c  >> iastore
ir2JVM (_, c, _) ReadReal = readReal c >> fastore

ir2JVM _ (IfBool l)    = ifne (show l)
ir2JVM _ (IfNotBool l) = ifeq (show l)
ir2JVM _ (IfGteInt l)  = if_icmpge (show l)
ir2JVM _ (IfLessInt l) = if_icmplt (show l)
ir2JVM _ (IfLessReal l) = fcmpl >> iconst (-1) >> if_icmpeq (show l)
ir2JVM _ (IfGteReal l) = fcmpl >> iconst (-1) >> if_icmpne (show l) 

ir2JVM _ LessReal = fcmpl >> iconst (-1) >> condZeroOrOne if_icmpne
ir2JVM _ LessInt = condZeroOrOne if_icmpge
ir2JVM _ NotBool = ineg >> iconst 1 >> iadd

ir2JVM _ AddInt  = iadd
ir2JVM _ AddReal = fadd
ir2JVM _ MulInt  = imul
ir2JVM _ MulReal = fmul

ir2JVM _ Int2Real = i2f
ir2JVM _ Real2Int = f2i

ir2JVM _ (PutLabel l) = lab $ show l
ir2JVM _ Ret = returnJ

ir2JVM _ _ = nop 
    
condZeroOrOne :: (String -> PrinterMonad ()) -> PrinterMonad ()
condZeroOrOne jmp = do
    jmp "$+7"
    iconst 1
    goto "$+4"
    iconst 0
    
genericLoad :: SMap LocalVar -> String -> PrinterMonad ()
genericLoad vars v = do
    aload n 
    pushIndex i 
    where LocalVar { localVarPos = n, localVarIdx = i } = vars ! v 
    
procSig :: Procedure -> Method
procSig Procedure { procName = name, procParams = params } = 
    Method name params' JVMVoid
    where params' = concatMap paramToJVM params
    
fullProcSig :: String -> Procedure -> Method
fullProcSig name proc @ Procedure { procName = n } = 
    procSig proc { procName = name ++ "/" ++ n }
    
readThing :: String -> String -> PrinterMonad ()
readThing c m  = do 
    getstatic (c ++ "/scanner") "Ljava/util/Scanner;"
    invokevirtual $ "java/util/Scanner/" ++ m
    
readInt :: String -> PrinterMonad ()
readInt = flip readThing "nextInt()I"

readReal :: String -> PrinterMonad ()
readReal = flip readThing "nextFloat()F"

readBool :: String -> PrinterMonad ()
readBool = flip readThing "nextBoolean()Z"


paramToJVM :: Parameter -> [JVMType]
paramToJVM p = case paramType p of
    IntegerT | isByVal   -> [JVMInt]
             | otherwise -> [JVMArray JVMInt, JVMInt]
    RealT    | isByVal   -> [JVMFloat]
             | otherwise -> [JVMArray JVMFloat, JVMInt]
    BooleanT | isByVal   -> [JVMBoolean]
             | otherwise -> [JVMArray JVMBoolean, JVMInt]
    a -> [typeToJVM a] 
    where isByVal = paramKind p == ByVal
         
typeToJVM :: Type -> JVMType
typeToJVM IntegerT = JVMInt
typeToJVM RealT    = JVMFloat
typeToJVM BooleanT = JVMBoolean
typeToJVM (ArrayT t _ _) = JVMArray (typeToJVM t)
typeToJVM TypeError = error "Internal error: TypeError (typeToJVM)" 
    
typeToArrayDesc :: Type -> String
typeToArrayDesc IntegerT = "int"
typeToArrayDesc RealT    = "float"
typeToArrayDesc BooleanT = "boolean"
typeToArrayDesc t = error $ "Internal error: " ++ show t ++ " (typeToArrayDesc)"
    
