module Module.File where

readFileContents :: FilePath -> IO String
readFileContents filePath = do
    readFile filePath

writeFileContents :: FilePath -> String -> IO ()
writeFileContents filePath contents = do
    writeFile filePath contents

splitLines :: String -> [String]
splitLines str = lines str