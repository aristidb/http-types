module Network.HTTP.Types (
    -- * Methods

    -- | __For more information__: "Network.HTTP.Types.Method"

    Method,

    -- ** Constants
    methodGet,
    methodPost,
    methodHead,
    methodPut,
    methodDelete,
    methodTrace,
    methodConnect,
    methodOptions,
    methodPatch,
    StdMethod (..),

    -- ** Parsing and redering methods
    parseMethod,
    renderMethod,
    renderStdMethod,

    -- * Versions

    -- | __For more information__: "Network.HTTP.Types.Version"

    HttpVersion (..),
    http09,
    http10,
    http11,
    http20,

    -- * Status

    -- | __For more information__: "Network.HTTP.Types.Status"

    Status (..),

    -- ** Constants
    mkStatus,
    status100,
    continue100,
    status101,
    switchingProtocols101,
    status200,
    ok200,
    status201,
    created201,
    status202,
    accepted202,
    status203,
    nonAuthoritative203,
    status204,
    noContent204,
    status205,
    resetContent205,
    status206,
    partialContent206,
    status300,
    multipleChoices300,
    status301,
    movedPermanently301,
    status302,
    found302,
    status303,
    seeOther303,
    status304,
    notModified304,
    status305,
    useProxy305,
    status307,
    temporaryRedirect307,
    status308,
    permanentRedirect308,
    status400,
    badRequest400,
    status401,
    unauthorized401,
    status402,
    paymentRequired402,
    status403,
    forbidden403,
    status404,
    notFound404,
    status405,
    methodNotAllowed405,
    status406,
    notAcceptable406,
    status407,
    proxyAuthenticationRequired407,
    status408,
    requestTimeout408,
    status409,
    conflict409,
    status410,
    gone410,
    status411,
    lengthRequired411,
    status412,
    preconditionFailed412,
    status413,
    requestEntityTooLarge413,
    status414,
    requestURITooLong414,
    status415,
    unsupportedMediaType415,
    status416,
    requestedRangeNotSatisfiable416,
    status417,
    expectationFailed417,
    status418,
    imATeapot418,
    status422,
    unprocessableEntity422,
    status428,
    preconditionRequired428,
    status429,
    tooManyRequests429,
    status431,
    requestHeaderFieldsTooLarge431,
    status500,
    internalServerError500,
    status501,
    notImplemented501,
    status502,
    badGateway502,
    status503,
    serviceUnavailable503,
    status504,
    gatewayTimeout504,
    status505,
    httpVersionNotSupported505,
    status511,
    networkAuthenticationRequired511,
    statusIsInformational,
    statusIsSuccessful,
    statusIsRedirection,
    statusIsClientError,
    statusIsServerError,

    -- * Headers

    -- | __For more information__: "Network.HTTP.Types.Header"

    -- ** Types
    Header,
    HeaderName,
    RequestHeaders,
    ResponseHeaders,

    -- ** Common headers
    hAccept,
    hAcceptLanguage,
    hAuthorization,
    hCacheControl,
    hCookie,
    hConnection,
    hContentEncoding,
    hContentLength,
    hContentMD5,
    hContentType,
    hDate,
    hIfModifiedSince,
    hIfRange,
    hLastModified,
    hLocation,
    hRange,
    hReferer,
    hServer,
    hUserAgent,

    -- ** Byte ranges
    ByteRange (..),
    renderByteRangeBuilder,
    renderByteRange,
    ByteRanges,
    renderByteRangesBuilder,
    renderByteRanges,
    parseByteRanges,

    -- * URI

    -- | __For more extensive information__: "Network.HTTP.Types.URI"

    -- ** Query strings

    -- *** Query
    Query,
    QueryItem,
    renderQuery,
    renderQueryBuilder,
    parseQuery,
    parseQueryReplacePlus,

    -- *** Text query string (UTF8 encoded)
    QueryText,
    queryTextToQuery,
    queryToQueryText,
    renderQueryText,
    parseQueryText,

    -- *** SimpleQuery
    SimpleQuery,
    SimpleQueryItem,
    simpleQueryToQuery,
    renderSimpleQuery,
    parseSimpleQuery,

    -- *** PartialEscapeQuery
    PartialEscapeQuery,
    PartialEscapeQueryItem,
    EscapeItem (..),
    renderQueryPartialEscape,
    renderQueryBuilderPartialEscape,

    -- ** Generalized query types
    QueryLike (toQuery),

    -- ** Path

    -- *** Segments + Query String
    extractPath,
    encodePath,
    decodePath,

    -- *** Path Segments
    encodePathSegments,
    encodePathSegmentsRelative,
    decodePathSegments,

    -- ** URL encoding / decoding
    urlEncode,
    urlEncodeBuilder,
    urlDecode,
)
where

import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.QueryLike
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI
import Network.HTTP.Types.Version
