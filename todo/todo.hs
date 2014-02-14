import System.Environment
import System.Directory
import System.IO
import Data.List

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args


dispatch :: [(String, [String] -> IO ())]
dispatch = 
    [("add", add),
     ("view", view),
     ("remove", remove)]

view :: [String] -> IO ()
view [fileName] = do 
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show (n+1) ++ ") " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

add :: [String] -> IO ()
add [fileName, task] = appendFile fileName (task ++ "\n")

remove :: [String] -> IO()
remove (fileName:number:_) = do
    contents <- readFile fileName
    let todoTasks = lines contents
        taskNum = read number
        newTodoTasks = delete (todoTasks !! (taskNum-1)) todoTasks
    length newTodoTasks `seq` writeFile fileName (unlines newTodoTasks)
