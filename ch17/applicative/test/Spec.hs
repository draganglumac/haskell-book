import ApplicativeTests
import ZipListTests

main :: IO ()
main = do
  putStrLn "badMonoid"
  badMonoid
  putStrLn "applicativeList"
  applicativeList
  putStrLn "applicativeListBottom"
  applicativeListBottom
  putStrLn "testZipListSum"
  testZipListSum
