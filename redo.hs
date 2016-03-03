import           Control.Monad        (filterM, liftM,unless)
import           Control.Monad.Loops  (allM)
import qualified Data.ByteString.Lazy as LB
import           Data.Digest.Pure.MD5 (md5)
import           Data.Maybe           (listToMaybe)
import           Debug.Trace          (trace)
import           System.Directory     (doesFileExist, getDirectoryContents,
                                       removeFile, renameFile,removeDirectoryRecursive,createDirectoryIfMissing)
import           System.Environment   (getArgs, getEnvironment)
import           System.Exit          (ExitCode (..))
import           System.FilePath      (hasExtension, replaceBaseName,
                                       takeBaseName, (</>))
import           System.IO            (hPutStrLn, stderr,hGetLine,withFile,IOMode (..))
import           System.Process       (CreateProcess (..), createProcess, shell,
                                       waitForProcess)

 
main :: IO ()
main =  getArgs >>= mapM_ redo


redo :: String -> IO ()
redo target = do 

	let tmp = target ++ "---redoing"
	hasUpdated <-  hasFilesUpdated target
	unless hasUpdated $
		do 
			targetPath <-redoPath target
			case targetPath of
		  		Nothing -> error "files not present"
		  		Just targetPath -> do
		  						removeDirectoryRecursive depsDir
								createDirectoryIfMissing True depsDir
								targetMd5 <- md5' targetPath
								writeFile (depsDir </> targetPath) targetMd5
		  						oldEnv <- getEnvironment
							  	(_, _, _, ph) <- createProcess (shell  $ cmd targetPath){env = Just ( ("REDO_TARGET", target) : oldEnv)}
							  	exit <- waitForProcess ph
							  	case exit of
								   	ExitSuccess ->  renameFile tmp target
								   	ExitFailure code -> do
							   						hPutStrLn stderr $ "Redo script exited with non zero code" ++ show code
							   						removeFile tmp
							   	where 
							   		cmd targetPath = trace' $ "sh -x " ++ targetPath ++ " 0 " ++ " " ++ takeBaseName target ++ " " ++ tmp ++ " > " ++ tmp
							   		depsDir = ".redo" </> target



redoPath :: FilePath -> IO (Maybe FilePath)

redoPath target = liftM listToMaybe $ filterM doesFileExist filesToProcess
					where filesToProcess = (target ++ ".do") : [replaceBaseName target "default" ++ ".do" | hasExtension target]

trace' arg = trace arg arg

hasFilesUpdated :: String -> IO Bool
hasFilesUpdated dep = do
	files <- liftM  (filter  (`notElem` [".",".." ]))  (getDirectoryContents  (".redo" </> dep))
	(\x -> hasFileUpdated (".redo" </> dep </> x) x) `allM` files
hasFileUpdated :: FilePath -> FilePath -> IO Bool
hasFileUpdated dep target  = do
	oldMD5 <- withFile dep ReadMode hGetLine
	putStrLn $ "oldMD5 file :" ++ oldMD5
	putStrLn $ "file updated target file :" ++ target
	newMD5 <- md5' $ target
	putStrLn $ "file updated hash :" ++ newMD5
	return $ oldMD5 == newMD5


md5' :: FilePath -> IO String
md5' path = do
	newMD5 <- liftM md5 (LB.readFile path)
	return $ show newMD5
