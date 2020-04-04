# Curling

A library for making basic HTTP requests with Haskell.
This is a wrapper around the `Network.HTTP.Req` package.
It supports very simple GET/POST/PUT/DELETE requests.

## Usage

Use overleaded strings, generics (for JSON), and import the library.

```
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main (main) where

import qualified GHC.Generics as G
import qualified Data.Aeson as J
import qualified Network.HTTP.Curling.Client as C

main :: IO ()
main = do
    ...
```

Construct a request:

```
main :: IO ()
main = do

  let request = C.Request
        { C.requestMethod = C.GET
        , C.requestUrl = "https://httpbin.org/get"
        , C.requestBody = Nothing
        , C.requestQueryParams = Nothing } 
```

Now make the request:

```
main :: IO ()
main = do

  let request = C.Request
        { C.requestMethod = C.GET
        , C.requestUrl = "https://httpbin.org/get"
        , C.requestBody = Nothing
        , C.requestQueryParams = Nothing } 

  result <- C.run request
```

Handle the result as you see fit:

```
main :: IO ()
main = do

  let request = C.Request
        { C.requestMethod = C.GET
        , C.requestUrl = "https://httpbin.org/get"
        , C.requestBody = Nothing
        , C.requestQueryParams = Nothing } 

  result <- C.run request

  case result of
    Left e -> show e
    Right r -> show r
```




## Build/run

To build the code (use `cabal new-*` commands for `cabal` less than 3.0):

    cabal build

Load in GHCi:

    cabal repl curling

Clean it:

    cabal clean


