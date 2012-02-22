{-# LANGUAGE DeriveDataTypeable #-}
-- | A function to convert attoparsec 'Parser's into 'Iteratee's.
module Data.Attoparsec.Iteratee
  ( ParseError(..), parserToIteratee ) where


------------------------------------------------------------------------------
import           Control.Exception
import qualified Data.Attoparsec as Atto
import           Data.Attoparsec hiding (many, Result(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Iteratee
import           Data.Typeable
------------------------------------------------------------------------------

data ParseError
    = ParseError {errorContexts :: [String], errorMessage :: String}
    deriving (Show, Typeable)

instance Exception ParseError

-- | A function to convert attoparsec 'Parser's into 'Iteratee's.
parserToIteratee :: (Monad m) =>
                    Parser a
                 -> Iteratee ByteString m a
parserToIteratee p =
    icontP (f (parse p))
  where
    f k (EOF Nothing) =
        case feed (k B.empty) B.empty of
          Atto.Fail _ err dsc -> (throwErr (toException $ ParseError err dsc), EOF Nothing)
          Atto.Partial _ -> (throwErr (toException EofException), EOF Nothing)
          Atto.Done rest v
              | B.null rest -> (idone v, EOF Nothing)
              | otherwise -> (idone v, EOF Nothing)
    f _ (EOF (Just e)) = (throwErr e, EOF (Just e))
    f k (Chunk s)
        | B.null s = (icontP (f k), Chunk s)
        | otherwise =
            case k s of
              Atto.Fail _ err dsc -> (throwErr (toException $
                                               ParseError err dsc)
                                      , Chunk empty)
              Atto.Partial k' -> (icontP (f k'), Chunk empty)
              Atto.Done rest v -> (idone v, Chunk rest)
