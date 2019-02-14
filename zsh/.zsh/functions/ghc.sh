# Run a quick haskell one-liner
rh()
{
  echo "main = print $ $*" | runhaskell
}

