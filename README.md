core-server - generating GHC Core from Haskell code through JSON

Usage: core-server (-p|--port PORT) (-d|--delay DELAY) (-t|--hsdir HSDIR)
  A JSON server that generates GHC Core from Haskell code

Available options:
  -h,--help                Show this help text
  -p,--port PORT           Make the server listen on port PORT
  -d,--delay DELAY         Give DELAY milliseconds to GHC before killing it
  -t,--hsdir HSDIR         Store the haskell code in temporary files in the HSDIR directory
