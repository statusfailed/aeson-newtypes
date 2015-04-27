# aeson-newtypes

Utility types for Data.Aeson

## Quoted

`Quoted` is a cheap way to decode types that have been "stringified".

For example, if you're dealing with prices in dollars, you need fixed-point
numbers.
Some applications might send these as strings, like this:

    {"prices": ["299.99", "19.99"]}

To decode this using "Quoted" we might write the following:

    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE TemplateHaskell #-}
    module Main where

    import Data.Fixed
    import Data.Aeson
    import Data.Aeson.TH
    import Data.Aeson.Newtypes

    -- NOTE: "Centi" is the fixed-point type for numbers at 2 decimal places.

    data Prices = Prices
      { prices :: [Quoted Centi]
      } deriving(Eq, Show)

    $(deriveJSON defaultOptions ''Prices)
    exampleData = "{\"prices\": [\"299.99\", \"19.99\"]}"
    main = print (decode exampleData :: Maybe Prices)

Running it, we'll see this:

    *Main> main
    Just (Prices {prices = [Quoted {unQuoted = 299.99},Quoted {unQuoted = 19.99}]})

You'll have to `map unQuoted` the prices yourself, but it saves writing a
FromJSON instance manually.
