{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Query strings generally have the following form: @"key1=value1&key2=value2"@
--
-- >>> renderQuery False [("key1", Just "value1"), ("key2", Just "value2")]
-- "key1=value1&key2=value2"
--
-- But if the value of @key1@ is 'Nothing', it becomes: @key1&key2=value2@
--
-- >>> renderQuery False [("key1", Nothing), ("key2", Just "value2")]
-- "key1&key2=value2"
--
-- This module also provides type synonyms and functions to handle queries
-- that do not allow/expect keys without values ('SimpleQuery'), handle
-- queries which have partially escaped characters
module Network.HTTP.Types.URI (
    -- * Query strings

    -- ** Query
    Query,
    QueryItem,
    renderQuery,
    renderQueryBuilder,
    parseQuery,
    parseQueryReplacePlus,

    -- *** Query (Text)
    QueryText,
    queryTextToQuery,
    queryToQueryText,
    renderQueryText,
    parseQueryText,

    -- ** SimpleQuery

    -- | If values are guaranteed, it might be easier working with 'SimpleQuery'.
    --
    -- This way, you don't have to worry about any 'Maybe's, though when parsing
    -- a query string and there's no @\'=\'@ after the key in the query item, the
    -- value will just be an empty 'B.ByteString'.
    SimpleQuery,
    SimpleQueryItem,
    simpleQueryToQuery,
    renderSimpleQuery,
    parseSimpleQuery,

    -- ** PartialEscapeQuery

    -- | For some values in query items, certain characters must not be percent-encoded,
    -- for example @\'+\'@ or @\':\'@ in
    --
    -- @q=a+language:haskell+created:2009-01-01..2009-02-01&sort=stars@
    --
    -- Using specific 'EscapeItem's provides a way to decide which parts of a query string value
    -- will be URL encoded and which won't.
    --
    -- This is mandatory when searching for @\'+\'@ (@%2B@ being a percent-encoded @\'+\'@):
    --
    -- @q=%2B+language:haskell@
    PartialEscapeQuery,
    PartialEscapeQueryItem,
    EscapeItem (..),
    renderQueryPartialEscape,
    renderQueryBuilderPartialEscape,

    -- * Path

    -- ** Segments + Query String
    extractPath,
    encodePath,
    decodePath,

    -- ** Path Segments
    encodePathSegments,
    encodePathSegmentsRelative,
    decodePathSegments,

    -- * URL encoding / decoding
    urlEncode,
    urlEncodeBuilder,
    urlDecode,
)
where

import Control.Arrow (second, (***))
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8)

-- | An item from the query string, split up into two parts.
--
-- The second part should be 'Nothing' if there was no key-value
-- separator after the query item name.
--
-- @since 0.2.0
type QueryItem = (B.ByteString, Maybe B.ByteString)

-- | A sequence of 'QueryItem's.
type Query = [QueryItem]

-- | Like Query, but with 'Text' instead of 'B.ByteString' (UTF8-encoded).
--
-- @since 0.5.2
type QueryText = [(Text, Maybe Text)]

-- | Convert 'QueryText' to 'Query'.
--
-- @since 0.5.2
queryTextToQuery :: QueryText -> Query
queryTextToQuery = map $ encodeUtf8 *** fmap encodeUtf8

-- | Convert 'QueryText' to a 'B.Builder'.
--
-- If you want a question mark (@?@) added to the front of the result, use 'True'.
--
-- @since 0.5.2
renderQueryText :: Bool -> QueryText -> B.Builder
renderQueryText b = renderQueryBuilder b . queryTextToQuery

-- | Convert 'Query' to 'QueryText' (leniently decoding the UTF-8).
--
-- @since 0.5.2
queryToQueryText :: Query -> QueryText
queryToQueryText =
    map $ go *** fmap go
  where
    go = decodeUtf8With lenientDecode

-- | Parse a 'QueryText' from a 'B.ByteString'. See 'parseQuery' for details.
--
-- @'queryToQueryText' . 'parseQuery'@
--
-- @since 0.5.2
parseQueryText :: B.ByteString -> QueryText
parseQueryText = queryToQueryText . parseQuery

-- | Simplified query item type without support for parameter-less items.
--
-- @since 0.2.0
type SimpleQueryItem = (B.ByteString, B.ByteString)

-- | A sequence of 'SimpleQueryItem's.
type SimpleQuery = [SimpleQueryItem]

-- | Convert 'SimpleQuery' to 'Query'.
--
-- @since 0.5
simpleQueryToQuery :: SimpleQuery -> Query
simpleQueryToQuery = map (second Just)

-- | Renders the given 'Query' into a 'Builder'.
--
-- If you want a question mark (@?@) added to the front of the result, use 'True'.
--
-- @since 0.5
renderQueryBuilder :: Bool -> Query -> B.Builder
renderQueryBuilder _ [] = mempty
renderQueryBuilder qmark' (p : ps) =
    -- FIXME: replace mconcat + map with foldr
    mconcat $
        go (if qmark' then qmark else mempty) p
            : map (go amp) ps
  where
    qmark = B.byteString "?"
    amp = B.byteString "&"
    equal = B.byteString "="
    go sep (k, mv) =
        mconcat
            [ sep
            , urlEncodeBuilder True k
            , case mv of
                Nothing -> mempty
                Just v -> equal `mappend` urlEncodeBuilder True v
            ]

-- | Renders the given 'Query' into a 'B.ByteString'.
--
-- If you want a question mark (@?@) added to the front of the result, use 'True'.
--
-- @since 0.2.0
renderQuery :: Bool -> Query -> B.ByteString
renderQuery qm = BL.toStrict . B.toLazyByteString . renderQueryBuilder qm

-- | Render the given 'SimpleQuery' into a 'ByteString'.
--
-- If you want a question mark (@?@) added to the front of the result, use 'True'.
--
-- @since 0.2.0
renderSimpleQuery :: Bool -> SimpleQuery -> B.ByteString
renderSimpleQuery useQuestionMark = renderQuery useQuestionMark . simpleQueryToQuery

-- | Split out the query string into a list of keys and values. A few
-- importants points:
--
-- * The result returned is still bytestrings, since we perform no character
-- decoding here. Most likely, you will want to use UTF-8 decoding, but this is
-- left to the user of the library.
--
-- * Percent decoding errors are ignored. In particular, @"%Q"@ will be output as
-- @"%Q"@.
--
-- * It decodes @\'+\'@ characters to @\' \'@
--
-- @since 0.2.0
parseQuery :: B.ByteString -> Query
parseQuery = parseQueryReplacePlus True

-- | Same functionality as 'parseQuery', but with the option to decode @\'+\'@ characters to @\' \'@
-- or to preserve any @\'+\'@ encountered.
--
-- If you want to replace any @\'+\'@ with a space, use 'True'.
--
-- @since 0.12.2
parseQueryReplacePlus :: Bool -> B.ByteString -> Query
parseQueryReplacePlus replacePlus bs = parseQueryString' $ dropQuestion bs
  where
    dropQuestion q =
        case B.uncons q of
            Just (63, q') -> q'
            _ -> q
    parseQueryString' q | B.null q = []
    parseQueryString' q =
        let (x, xs) = breakDiscard queryStringSeparators q
         in parsePair x : parseQueryString' xs
      where
        parsePair x =
            let (k, v) = B.break (== 61) x -- equal sign
                v'' =
                    case B.uncons v of
                        Just (_, v') -> Just $ urlDecode replacePlus v'
                        _ -> Nothing
             in (urlDecode replacePlus k, v'')

queryStringSeparators :: B.ByteString
queryStringSeparators = B.pack [38, 59] -- ampersand, semicolon

-- | Break the second bytestring at the first occurrence of any bytes from
-- the first bytestring, discarding that byte.
breakDiscard :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
breakDiscard seps s =
    let (x, y) = B.break (`B.elem` seps) s
     in (x, B.drop 1 y)

-- | Parse 'SimpleQuery' from a 'ByteString'.
--
-- This uses 'parseQuery' under the hood, and will transform
-- any 'Nothing' values into an empty 'B.ByteString'.
--
-- @since 0.2.0
parseSimpleQuery :: B.ByteString -> SimpleQuery
parseSimpleQuery = map (second $ fromMaybe B.empty) . parseQuery

ord8 :: Char -> Word8
ord8 = fromIntegral . ord

unreservedQS, unreservedPI :: [Word8]
unreservedQS = map ord8 "-_.~"
-- FIXME: According to RFC 3986, the following are also allowed in path segments:
-- "!'()*;"
--
-- https://www.rfc-editor.org/rfc/rfc3986#section-3.3
unreservedPI = map ord8 "-_.~:@&=+$,"

-- | Percent-encoding for URLs.
--
-- This will substitute every byte with its percent-encoded equivalent unless:
--
-- * The byte is alphanumeric. (i.e. one of @/[A-Za-z0-9]/@)
--
-- * The byte is one of the 'Word8' listed in the first argument.
urlEncodeBuilder' :: [Word8] -> B.ByteString -> B.Builder
urlEncodeBuilder' extraUnreserved =
    mconcat . map encodeChar . B.unpack
  where
    encodeChar ch
        | unreserved ch = B.word8 ch
        | otherwise = h2 ch

    unreserved ch
        | ch >= 65 && ch <= 90 = True -- A-Z
        | ch >= 97 && ch <= 122 = True -- a-z
        | ch >= 48 && ch <= 57 = True -- 0-9
    unreserved c = c `elem` extraUnreserved

    -- must be upper-case
    h2 v = B.word8 37 `mappend` B.word8 (h a) `mappend` B.word8 (h b) -- 37 = %
      where
        (a, b) = v `divMod` 16
    h i
        | i < 10 = 48 + i -- zero (0)
        | otherwise = 65 + i - 10 -- 65: A

-- | Percent-encoding for URLs.
--
-- Like 'urlEncode', but only makes the 'B.Builder'.
--
-- @since 0.5
urlEncodeBuilder :: Bool -> B.ByteString -> B.Builder
urlEncodeBuilder True = urlEncodeBuilder' unreservedQS
urlEncodeBuilder False = urlEncodeBuilder' unreservedPI

-- | Percent-encoding for URLs.
--
-- In short:
--
-- * if you're encoding (parts of) a path element, use 'False'.
--
-- * if you're encoding (parts of) a query string, use 'True'.
--
-- === __In-depth explanation__
--
-- This will substitute every byte with its percent-encoded equivalent unless:
--
-- * The byte is alphanumeric. (i.e. @A-Z@, @a-z@, or @0-9@)
--
-- * The byte is either a dash @\'-\'@, an underscore @\'_\'@, a dot @\'.\'@, or a tilde @\'~\'@
--
-- * If 'False' is used, the following will also /not/ be percent-encoded:
--
--     * colon @\':\'@, at sign @\'\@\'@, ampersand @\'&\'@, equals sign @\'=\'@, plus sign @\'+\'@, dollar sign @\'$\'@ or a comma @\',\'@
--
-- @since 0.2.0
urlEncode :: Bool -> B.ByteString -> B.ByteString
urlEncode q = BL.toStrict . B.toLazyByteString . urlEncodeBuilder q

-- | Percent-decoding.
--
-- If you want to replace any @\'+\'@ with a space, use 'True'.
--
-- @since 0.2.0
urlDecode :: Bool -> B.ByteString -> B.ByteString
urlDecode replacePlus z = fst $ B.unfoldrN (B.length z) go z
  where
    go bs =
        case B.uncons bs of
            Nothing -> Nothing
            -- plus to space
            Just (43, ws) | replacePlus -> Just (32, ws)
            -- percent
            Just (37, ws) -> Just $ fromMaybe (37, ws) $ do
                (x, xs) <- B.uncons ws
                x' <- hexVal x
                (y, ys) <- B.uncons xs
                y' <- hexVal y
                Just (combine x' y', ys)
            Just (w, ws) -> Just (w, ws)
    hexVal w
        | 48 <= w && w <= 57 = Just $ w - 48 -- 0 - 9
        | 65 <= w && w <= 70 = Just $ w - 55 -- A - F
        | 97 <= w && w <= 102 = Just $ w - 87 -- a - f
        | otherwise = Nothing
    combine :: Word8 -> Word8 -> Word8
    combine a b = shiftL a 4 .|. b

-- | Encodes a list of path segments into a valid URL fragment.
--
-- This function takes the following three steps:
--
-- * UTF-8 encodes the characters.
--
-- * Prepends each segment with a slash.
--
-- * Performs percent-encoding on all characters that are __not__:
--
--     * alphanumeric (i.e. @A-Z@ and @a-z@)
--
--     * digits (i.e. @0-9@)
--
--     * a dash @\'-\'@, an underscore @\'_\'@, a dot @\'.\'@, or a tilde @\'~\'@
--
-- For example:
--
-- >>> encodePathSegments ["foo", "bar1", "~baz"]
-- "/foo/bar1/~baz"
--
-- >>> encodePathSegments ["foo bar", "baz/bin"]
-- "/foo%20bar/baz%2Fbin"
--
-- >>> encodePathSegments ["שלום"]
-- "/%D7%A9%D7%9C%D7%95%D7%9D"
--
-- Huge thanks to /Jeremy Shaw/ who created the original implementation of this
-- function in web-routes and did such thorough research to determine all
-- correct escaping procedures.
--
-- @since 0.5
encodePathSegments :: [Text] -> B.Builder
encodePathSegments = foldr (\x -> mappend (B.byteString "/" `mappend` encodePathSegment x)) mempty

-- | Like 'encodePathSegments', but without the initial slash.
--
-- @since 0.6.10
encodePathSegmentsRelative :: [Text] -> B.Builder
encodePathSegmentsRelative xs = mconcat $ intersperse (B.byteString "/") (map encodePathSegment xs)

encodePathSegment :: Text -> B.Builder
encodePathSegment = urlEncodeBuilder False . encodeUtf8

-- | Parse a list of path segments from a valid URL fragment.
--
-- Will also decode any percent-encoded characters.
--
-- @since 0.5
decodePathSegments :: B.ByteString -> [Text]
decodePathSegments "" = []
decodePathSegments "/" = []
decodePathSegments a =
    go $ drop1Slash a
  where
    drop1Slash bs =
        case B.uncons bs of
            Just (47, bs') -> bs' -- 47 == /
            _ -> bs
    go bs =
        let (x, y) = B.break (== 47) bs
         in decodePathSegment x
                : if B.null y
                    then []
                    else go $ B.drop 1 y

decodePathSegment :: B.ByteString -> Text
decodePathSegment = decodeUtf8With lenientDecode . urlDecode False

-- | Extract whole path (path segments + query) from a
-- [RFC 2616 Request-URI](http://tools.ietf.org/html/rfc2616#section-5.1.2).
--
-- Though a more accurate description of this function's behaviour is that
-- it removes the domain/origin if the string starts with an HTTP protocol.
-- (i.e. @http://@ or @https://@)
--
-- This function will not change anything when given any other 'B.ByteString'.
-- (except return a root path @\"\/\"@ if given an empty string)
--
-- >>> extractPath "/path"
-- "/path"
--
-- >>> extractPath "http://example.com:8080/path"
-- "/path"
--
-- >>> extractPath "http://example.com"
-- "/"
--
-- >>> extractPath ""
-- "/"
--
-- >>> extractPath "www.google.com/some/path"
-- "www.google.com/some/path"
--
-- @since 0.8.5
extractPath :: B.ByteString -> B.ByteString
extractPath = ensureNonEmpty . extract
  where
    extract path
        | "http://" `B.isPrefixOf` path = (snd . breakOnSlash . B.drop 7) path
        | "https://" `B.isPrefixOf` path = (snd . breakOnSlash . B.drop 8) path
        | otherwise = path
    breakOnSlash = B.break (== 47)
    ensureNonEmpty "" = "/"
    ensureNonEmpty p = p

-- | Encode a whole path (path segments + query).
--
-- @since 0.5
encodePath :: [Text] -> Query -> B.Builder
encodePath x [] = encodePathSegments x
encodePath x y = encodePathSegments x `mappend` renderQueryBuilder True y

-- | Decode a whole path (path segments + query).
--
-- @since 0.5
decodePath :: B.ByteString -> ([Text], Query)
decodePath b =
    let (x, y) = B.break (== 63) b -- question mark
     in (decodePathSegments x, parseQuery y)

-----------------------------------------------------------------------------------------

-- | Section of a query item value that decides whether to use
-- regular URL encoding (using @'urlEncode True'@) with 'QE',
-- or to not encode /anything/ with 'QN'.
--
-- @since 0.12.1
data EscapeItem
    = -- | will be URL encoded
      QE B.ByteString
    | -- | will NOT /at all/ be URL encoded
      QN B.ByteString
    deriving (Show, Eq, Ord)

-- | Partially escaped query item.
--
-- The key will always be encoded using @'urlEncode True'@,
-- but the value will be encoded depending on which 'EscapeItem's are used.
--
-- @since 0.12.1
type PartialEscapeQueryItem = (B.ByteString, [EscapeItem])

-- | Query with some characters that should not be escaped.
--
-- General form: @a=b&c=d:e+f&g=h@
--
-- @since 0.12.1
type PartialEscapeQuery = [PartialEscapeQueryItem]

-- | Convert 'PartialEscapeQuery' to 'ByteString'.
--
-- If you want a question mark (@?@) added to the front of the result, use 'True'.
--
-- >>> renderQueryPartialEscape True [("a", [QN "x:z + ", QE (encodeUtf8 "They said: \"שלום\"")])]
-- "?a=x:z + They%20said%3A%20%22%D7%A9%D7%9C%D7%95%D7%9D%22"
--
-- @since 0.12.1
renderQueryPartialEscape :: Bool -> PartialEscapeQuery -> B.ByteString
renderQueryPartialEscape qm =
    BL.toStrict . B.toLazyByteString . renderQueryBuilderPartialEscape qm

-- | Convert a 'PartialEscapeQuery' to a 'B.Builder'.
--
-- If you want a question mark (@?@) added to the front of the result, use 'True'.
--
-- @since 0.12.1
renderQueryBuilderPartialEscape :: Bool -> PartialEscapeQuery -> B.Builder
renderQueryBuilderPartialEscape _ [] = mempty
-- FIXME replace mconcat + map with foldr
renderQueryBuilderPartialEscape qmark' (p : ps) =
    mconcat $
        go (if qmark' then qmark else mempty) p
            : map (go amp) ps
  where
    qmark = B.byteString "?"
    amp = B.byteString "&"
    equal = B.byteString "="
    go sep (k, mv) =
        mconcat
            [ sep
            , urlEncodeBuilder True k
            , case mv of
                [] -> mempty
                vs -> equal `mappend` mconcat (map encode vs)
            ]
    encode (QE v) = urlEncodeBuilder True v
    encode (QN v) = B.byteString v
