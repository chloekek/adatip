{-# LANGUAGE OverloadedStrings #-}

module Adatipd.NicknameSpec
  ( spec
  ) where

import Adatipd.Nickname (format, parse)
import Control.Applicative ((<|>))
import Data.Function ((&))
import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.Hedgehog ((===), hedgehog)

import qualified Data.Char as Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Text as Text (toLower)
import qualified Hedgehog as H (Gen, forAll)
import qualified Hedgehog.Gen as HG
import qualified Hedgehog.Range as HR

minLength, maxLength :: Int
minLength =  1
maxLength = 20

validChars, invalidChars :: H.Gen Char
validChars = HG.lower <|> HG.upper <|> HG.digit
invalidChars = HG.filter (not . isValid) HG.unicode
  where isValid c = Char.isAsciiUpper c ||
                    Char.isAsciiLower c ||
                    Char.isDigit c

spec :: Spec
spec = do

  it "parse accepts valid input" $
    hedgehog $ do
      let lengthRange = HR.linear minLength maxLength
      input <- H.forAll $ HG.text lengthRange validChars
      let actual = parse input & fmap format
      let expected = Right (Text.toLower input)
      actual === expected

  it "parse rejects empty input" $
    parse "" `shouldBe` Left "Nickname is empty"

  it "parse rejects overly long input" $
    hedgehog $ do
      let lengthRange = HR.linear (maxLength + 1) (maxLength * 2)
      input <- H.forAll $ HG.text lengthRange validChars
      parse input === Left "Nickname is too long"

  it "parse rejects input with invalid characters" $
    hedgehog $ do
      let lengthRange = HR.linear minLength maxLength
      input <- H.forAll $ HG.text lengthRange invalidChars
      parse input === Left "Nickname contains invalid characters"
