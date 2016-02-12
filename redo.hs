import System.Environment (getArgs,getEnvironment)
import System.Process (createProcess, waitForProcess, shell,CreateProcess(..))
import System.Directory (renameFile,removeFile,doesFileExist)
import System.FilePath (replaceBaseName,hasExtension,takeBaseName)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn,stderr)
import Control.Monad (filterM,liftM)
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)


main :: IO ()
main =  getArgs >>= mapM_ redo

redo :: String -> IO ()
redo target = do
  let tmp = target ++ "---redoing"
  targetPath <-redoPath target
  case targetPath of 
  	Nothing -> error "files not present"
  	Just targetPath -> do 

  						putStrLn targetPath
  						oldEnv <- getEnvironment
					  	(_, _, _, ph) <- createProcess (shell  $ cmd targetPath){env = Just ( ("REDO_TARGET", target) : oldEnv)}  
					  	exit <- waitForProcess ph
					  	case exit of
						   	ExitSuccess ->  renameFile tmp target
						   	ExitFailure code -> do 
					   						hPutStrLn stderr $ "Redo script exited with non zero code" ++ show code 
					   						removeFile tmp
					   	where cmd targetPath = trace' $ "sh " ++ targetPath ++ " 0 " ++ " " ++ takeBaseName target ++ " " ++ tmp ++ " > " ++ tmp

redoPath :: FilePath -> IO (Maybe FilePath)

redoPath target = liftM listToMaybe $ filterM doesFileExist filesToProcess
					where filesToProcess = (target ++ ".do") : [replaceBaseName target "default" ++ ".do" | hasExtension target]

trace' arg = trace arg arg
					
