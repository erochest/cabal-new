{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module CabalNew.Files
    ( expandUserDir
    , configDir
    , sed
    ) where


import           ClassyPrelude             hiding ((</>), (<>), FilePath)
import qualified Data.Text                 as T
import           Filesystem                (getHomeDirectory)
import qualified Filesystem.Path.CurrentOS as FS
import           Shelly


expandUserDir :: FilePath -> IO FilePath
expandUserDir filepath =
    case FS.encodeString filepath of
        ('~':'/':xs) -> (</> xs) <$> getHomeDirectory
        _            -> return filepath

configDir :: String -> Sh FilePath
configDir = canonicalize <=< liftIO . expandUserDir . fromString

sed :: FilePath -> (T.Text -> T.Text) -> Sh ()
sed fp f = withTmpDir $ \tmpDir -> do
    let tmpFile = tmpDir FS.</> FS.filename fp
    mv fp tmpFile
    writefile fp . T.unlines . map f . T.lines =<< readfile tmpFile
