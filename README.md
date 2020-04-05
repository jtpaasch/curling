# Curling

A library for making basic HTTP requests with Haskell.
This is a wrapper around the `Network.HTTP.Req` package.
It supports very simple GET/POST/PUT/DELETE requests.

## Usage

Use overloaded strings and generics (for JSON), and import 
the required libraries.

```
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main (main) where

import qualified GHC.Generics as G
import qualified Data.Aeson as J
import Network.HTTP.Req (http, https, (/:))
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
        , C.requestUrl = https "httpbin.org" /: "get"
        , C.requestBody = Nothing
        , C.requestQueryParams = Nothing } 

   ...

```

Now make the request:

```
  result <- C.run request
```

Then handle the result as you see fit:

```
  case result of
    Left e -> putStrLn $ show e
    Right r -> putStrLn $ show r
```

If the request was a success, you can inspect the status code
and the response body (decoded from JSON into an Aeson `Value`):

```
    Right r ->
      let code = C.responseCode r
          body = C.responseBody r
      in putStrLn $ (show code) ++ ": " ++ (show body) 
```

If the request returned an `Error`, you can inspect/match on it:

```
    Left e ->
      case e of
        C.StatusCode code status ->
          putStrLn $ "Error: " ++ (show code) ++ ": " ++ (show status)
        C.ResponseTimeout -> putStrLn "Timed out waiting for response"
        ...

``` 

### Query parameters and JSON data

Specify query parameters as a list of key/value pairs, where the key
and the value are each `Text`. For example:

```
  -- To send '?foo=15&bar=true':
  let params = [("foo", "15"), ("bar", "true")]

  let request = C.Request
        { C.requestMethod = C.GET
        , C.requestUrl = https "httpbin.org" /: "get"
        , C.requestBody = Nothing
        , C.requestQueryParams = Just params } -- Add query params. 

   ...
  
```

To send JSON data in the body of the request, first encode your data
into JSON (so it is a bytestring). For example, suppose we have some
important type of data that can be encoded as JSON (that is to say, it is an
instance of Aeson's `ToJSON` type class):

```
data ImportantData = ImportantData
  { biz :: Int
  , baz :: Bool
  } deriving (Show, G.Generic)

instance J.ToJSON ImportantData
instance J.FromJSON ImportantData
```

First encode your data:

```
  let myData = ImportantData { biz = 15, baz = True }
  let payload = J.encode myData
```

Then add it to a request:

```
  let request = C.Request
        { C.requestMethod = C.POST
        , C.requestUrl = https "httpbin.org" /: "post"
        , C.requestBody = Just payload -- Add the payload
        , C.requestQueryParams = Nothing }

   ...

```


## Build/run

To build the code (use `cabal new-*` commands for `cabal` less than 3.0):

    cabal build

Load in GHCi:

    cabal repl curling

Clean it:

    cabal clean


