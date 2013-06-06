{-# LANGUAGE DeriveDataTypeable #-}
-- | A function to convert attoparsec 'Parser's into 'Iteratee's.
module Data.Attoparsec.Iteratee
  ( ParseError(..), parserToIteratee ) where


------------------------------------------------------------------------------
import           Control.Exception
import qualified Data.Attoparsec as Atto
import           Data.Attoparsec hiding (Result)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Iteratee
import           Data.Typeable
------------------------------------------------------------------------------

data ParseError
    = ParseError {errorContexts :: [String], errorMessage :: String}
    deriving (Show, Typeable)

instance Exception ParseError where
  toException = iterExceptionToException
  fromException = iterExceptionFromException

instance IException ParseError

-- | A function to convert attoparsec 'Parser's into 'Iteratee's.
parserToIteratee :: (Monad m) =>
                    Parser a
                 -> Iteratee ByteString m a
parserToIteratee p =
    icontP (f (parse p))
  where
    f k (EOF Nothing) =
        case feed (k B.empty) B.empty of
          Atto.Fail _ err dsc -> let exc = toIterException $ ParseError err dsc
                                 in ContErr (throwErr exc) exc
          Atto.Partial k'  -> ContErr (icontP (f k'))
                                . toIterException $ EofException
                                "Attoparsec.parserToIteratee: unexpected EOF"
          Atto.Done rest v -> ContDone v (EOF Nothing)
    f k (EOF (Just e)) = ContErr (icontP (f k)) $ wrapEnumExc e
    f k NoData = continueP (f k)
    f k (Chunk s)
        | B.null s = continueP (f k)
        | otherwise =
            case k s of
              Atto.Fail _ err dsc -> let exc = toIterException
                                               $ ParseError err dsc
                                     in ContErr (throwErr exc) exc
              Atto.Partial k' -> continueP (f k')
              Atto.Done rest v -> ContDone v (Chunk rest)
