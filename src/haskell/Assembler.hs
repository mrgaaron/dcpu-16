module Assembler where

import Parser
import qualified Data.ByteString.Lazy as B
import Data.Binary.Put
import Data.Bits
import Data.Word
import Data.Char as C
import Numeric
import Control.Monad (forM)
import System.IO (openBinaryFile, IOMode(..), hClose)

genRegisterHex :: Expr -> Word16
genRegisterHex (Register "A") = 0x00 :: Word16
genRegisterHex (Register "B") = 0x01 :: Word16
genRegisterHex (Register "C") = 0x02 :: Word16
genRegisterHex (Register "X") = 0x03 :: Word16
genRegisterHex (Register "Y") = 0x04 :: Word16
genRegisterHex (Register "Z") = 0x05 :: Word16
genRegisterHex (Register "I") = 0x06 :: Word16
genRegisterHex (Register "J") = 0x07 :: Word16

genMemLocHex :: Expr -> Word16
genMemLocHex (MemLocation "A") = 0x08 :: Word16
genMemLocHex (MemLocation "B") = 0x09 :: Word16
genMemLocHex (MemLocation "C") = 0x0a :: Word16
genMemLocHex (MemLocation "X") = 0x0b :: Word16
genMemLocHex (MemLocation "Y") = 0x0c :: Word16
genMemLocHex (MemLocation "Z") = 0x0d :: Word16
genMemLocHex (MemLocation "I") = 0x0e :: Word16
genMemLocHex (MemLocation "J") = 0x0f :: Word16
genMemLocHex (MemLocation i) = read i :: Word16

genLiteralHex (Literal i) = (read i :: Word16) + 32

genAddressHex (Address i) = read i :: Word16

genIdentHex ident = case ident of
                        MemLocation _ -> genMemLocHex ident
                        Register _    -> genRegisterHex ident
                        Literal _     -> genLiteralHex ident
                        Address _     -> genAddressHex ident

genCmdHex :: Binop -> Word16
genCmdHex SET = 0x1 :: Word16
genCmdHex ADD = 0x2 :: Word16
genCmdHex SUB = 0x3 :: Word16
genCmdHex MUL = 0x4 :: Word16
genCmdHex DIV = 0x5 :: Word16
genCmdHex MOD = 0x6 :: Word16
genCmdHex SHL = 0x7 :: Word16
genCmdHex SHR = 0x8 :: Word16
genCmdHex AND = 0x9 :: Word16
genCmdHex BOR = 0xa :: Word16
genCmdHex XOR = 0xb :: Word16
genCmdHex IFE = 0xc :: Word16
genCmdHex IFN = 0xd :: Word16
genCmdHex IFG = 0xe :: Word16
genCmdHex IFB = 0xf :: Word16

assemble :: Expr -> Word16
assemble (Bin cmd (BinArg a b)) = 
    (genCmdHex cmd) .|. ((genIdentHex a) `shiftL` 4) .|. 
        ((genIdentHex b) `shiftL` 10)

assembleFromFile path = do
    instructions <- parseAssemblerFile path
    case (head instructions) of
        Error err -> return []
        otherwise -> return (map assemble instructions)

writeInstruction instr = do
    return $ putWord16be instr

serializeInstructions instrs = do
    return $ map runPut instrs

writeAssembledFile inPath = do
    instrs <- assembleFromFile inPath
    written <- forM instrs writeInstruction
    serialized <- serializeInstructions written
    outh <- openBinaryFile (genPath inPath) WriteMode
    forM serialized (B.hPut outh)
    hClose outh
        where genPath inp = concat $ (takeWhile notPeriod inp) : [".hex"]
              notPeriod c = 
                    if c /= '.' then True else False
    