{-# LANGUAGE DeriveDataTypeable #-}

module Main where

-- Imports for the program logic
import           Control.Exception
import           System.Console.CmdArgs
import           System.FilePath         (replaceExtension)

-- Imports for Hasty
import           Analysis.Ctx
import           Analysis.Typecheck
import           Backend.Regalloc        (allocFrags)
import           Backend.X86gen          (xFrags)
import           Data.IORef
import           IR.Canon                (xFragments)
import           IR.Hasty                (Program)
import           IR.Tasty                (Program)
import           IR.Translate            (xProg)
import qualified Parser.DebugHastyParser as DebugParser
import           Parser.HastyLexer       (Token, alexScanTokens)
import           Parser.HastyParser      (parseFile)

-----------------------------------------------------------------------------------------
-- Handle command-line arguments
-----------------------------------------------------------------------------------------
data HastyArgs = Compile { file :: FilePath, show_store :: Bool }
               | Lex { file :: FilePath }
               | Parse { file :: FilePath, debug :: Bool }
               | Typecheck { file :: FilePath, fails :: Bool }
  deriving (Show, Data, Typeable)

-- hasty [compile] [PROGRAM]
compileArgs :: HastyArgs
compileArgs =  Compile { file = def &= argPos 0 &= typ "PROGRAM" &= opt ""
                       , show_store = False }
        &= help "Compile the program"
        &= auto

-- hasty lex [PROGRAM]
lexArgs :: HastyArgs
lexArgs =  Lex { file = def &= argPos 0 &= typ "PROGRAM" &= opt "" }
        &= help "Lex the program"

-- hasty parse [PROGRAM]
parseArgs :: HastyArgs
parseArgs =  Parse { file = def &= argPos 0 &= typ "PROGRAM" &= opt "", debug = False }
        &= help "Parse the program"

-- hasty typecheck [PROGRAM]
typeCheckArgs :: HastyArgs
typeCheckArgs =  Typecheck { file = def &= argPos 0 &= typ "PROGRAM" &= opt "", fails = False }
        &= help "Typecheck the program"


mode :: HastyArgs
mode = modes [compileArgs, lexArgs, parseArgs, typeCheckArgs]
     &= program "hasty"
     &= help "The hasty programming language"

-----------------------------------------------------------------------------------------
-- Lex the program
-----------------------------------------------------------------------------------------
lexProgram :: FilePath -> IO [Parser.HastyLexer.Token]
lexProgram filename = do
  putStrLn ("Lexing Hasty code in " ++ show filename)
  sourceCode <- readFile filename
  return $ alexScanTokens sourceCode

-----------------------------------------------------------------------------------------
-- Parse the program
-----------------------------------------------------------------------------------------
parseProgram :: Bool -> FilePath -> IO IR.Hasty.Program
parseProgram doDebug filename = do
  let parser = if doDebug then DebugParser.parseFile else parseFile
  putStrLn ("Parsing Hasty code in " ++ show filename)
  parser filename

debugParseProgram :: FilePath -> IO IR.Hasty.Program
debugParseProgram = parseProgram True

-----------------------------------------------------------------------------------------
-- Typecheck the program
-----------------------------------------------------------------------------------------
typeCheckProgram :: Bool -> IR.Hasty.Program -> IORef Integer -> IO ()
typeCheckProgram shouldFail hastyIR ct = do
  result <- catch (typeCheckAndTranslate hastyIR ct >> return (not shouldFail)) handler
  let behavior = if shouldFail then "failed" else "succeeded"
  let sign = if result then "✔︎" else "✖︎"
  if result
    then putStrLn (sign ++ " Typechecking " ++ behavior ++ " (as expected)")
    else putStrLn (sign ++ " Typechecking " ++ behavior ++ " (which was not expected)")

  where handler :: ErrorCall -> IO Bool
        handler _ = return shouldFail

typeCheckAndTranslate ::  IR.Hasty.Program -> IORef Integer -> IO IR.Tasty.Program
typeCheckAndTranslate hastyIR ct = do
  putStrLn "Typechecking (and translating to Tasty)"
  checkProg (mkInitialCtx ct hastyIR) hastyIR

-----------------------------------------------------------------------------------------
-- Compile the program
-----------------------------------------------------------------------------------------
compileProgram :: FilePath -> IO ()
compileProgram fileName = do

  -- A mutable counter used throughout the compiler code so that we can
  -- create "fresh" variable names and assembly labels.
  ct <- Data.IORef.newIORef 0

  -- Parse the program
  hastyIR <- parseProgram False fileName

  -- Typecheck the program
  tastyIR <- typeCheckAndTranslate hastyIR ct

  let tastyFileName = replaceExtension fileName ".tasty"
  putStrLn $ "Writing Tasty code to " ++ tastyFileName
  writeFile tastyFileName (pp $ show tastyIR)

  -- Translate Tasty to Target, and write the Target file
  putStrLn "Translating to Target code"
  targetIR <- xProg ct tastyIR

  let targetFileName = replaceExtension fileName ".t"
  putStrLn $ "Writing Target code to " ++ targetFileName
  writeFile targetFileName (show targetIR)

  -- Translate Target to NormTarget, and write the NormTarget file
  putStrLn "Translating to NormTarget"
  normTargetIR <- xFragments ct targetIR

  let normtargetFileName = replaceExtension fileName ".nt"
  putStrLn $ "Writing NormTarget code to " ++ normtargetFileName
  writeFile normtargetFileName (show normTargetIR)

  -- Translate NormTarget code to Assem code
  --    X86-64 assembly, but with as many temporaries as we want
  putStrLn "Generating assembly code..."
  x86IR <- xFrags ct normTargetIR

  -- For debugging, write the Assem code to an .assem file
  let rtlCode = unlines (map show x86IR)
  let rtlFileName = replaceExtension fileName ".assem"
  writeFile rtlFileName rtlCode
  putStrLn $ "Wrote file " ++ rtlFileName

  -- Go from as many temporaries as we want to just
  --   the 16 x86-64 hardware registers
  -- As part of this process, we determine which temporaries are live
  --    just before each instruction. Again for debugging purposes,
  --    allocFrags writes this liveness information (and the inferred
  --    control-flow graph) to a .liveness file
  putStrLn "Doing register allocation..."
  assemblyFrags <- allocFrags ct (replaceExtension fileName ".liveness") x86IR

  -- Write the register-allocated Assem code to an .s file
  let assemblyCode = unlines (map show assemblyFrags)
  let assemblyFileName = replaceExtension fileName ".s"
  writeFile assemblyFileName assemblyCode
  putStrLn $ "Wrote file " ++ assemblyFileName

-----------------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------------
pp :: String -> String
pp []           = []
pp (',' : rest) = ',' : '\n' : pp rest
pp (c   : rest) = c : pp rest

-----------------------------------------------------------------------------------------
-- Main
-----------------------------------------------------------------------------------------
main :: IO ()
main = do
          -- Process the command-line arguments
          programArgs <- cmdArgs mode

          -- Parse the program in the user-supplied file
          let filename = file programArgs

          -- Run the command
          case programArgs of
            Compile{} -> compileProgram filename
            Typecheck{} -> do ct <- Data.IORef.newIORef 0
                              hastyIR <- parseProgram False filename
                              typeCheckProgram (fails programArgs) hastyIR ct
            Lex{}     -> lexProgram filename >>= print
            Parse{}   -> parseProgram (debug programArgs) filename >>= print
