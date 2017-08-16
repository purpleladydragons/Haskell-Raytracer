import RenderInfo (getRenderInfo, Render(Render))
import Output (getImage, createPPM)
import System.Environment (getArgs)

main :: IO ()
main = do args <- getArgs
          if (length args /= 1)
            then error "Requires <output_path>"
            else do let output = head args
                    let (Render depth resolution scene) =
                          getRenderInfo ()
                    writeFile output
                      (createPPM resolution (getImage depth resolution scene))
                    putStr $ "Image output written to " ++ output ++ "\n"

