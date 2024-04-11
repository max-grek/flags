# flags
## how to use

```haskell
flags :: NonEmpty (Flag Short)
flags = 
    define "some-flag0" "0.0.0.0" "some flag0 description" <|
    define "some flag1" (9000 :: Word16) "some flag1 description" <|
    define "some-flag2" "15s" "some flag2 description" <|
    define "some-flag3" False "some flag3 description" :| []

main :: IO ()
main = do
    args <- getArgs
    print $ parseArgs flags args
```
