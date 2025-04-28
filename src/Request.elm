module Request exposing
    ( Request, HttpMethod(..), map
    , Config, simple
    , http, get, post, put, delete, patch, options, head
    , Expect, expectJson, expectBytes, expectString, expectWhatever
    , withHeaders, excludeDefaultHeaders, ignoreDefaultHeaders, risky
    , cmd, task
    )

{-|

@docs Request, HttpMethod, map


## Config objects

@docs Config, simple


## Create requests

@docs http, get, post, put, delete, patch, options, head


## Declare task content

@docs Expect, expectJson, expectBytes, expectString, expectWhatever


## Set request parameters

@docs withHeaders, excludeDefaultHeaders, ignoreDefaultHeaders, risky


## Convert to commands and tasks

@docs cmd, task

-}

import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Dict exposing (Dict)
import Http
import Http.Tasks
import Json.Decode as D
import Set exposing (Set)
import Task exposing (Task)


{-| Represent an HTTP request
-}
type Request url value
    = Req
        { method : HttpMethod
        , url : url
        , expect : Expect value
        , body : Http.Body
        , timeout : Maybe Float
        , risky : Bool
        , headers : Headers
        , tracker : Maybe String
        }


{-| Modify the expected return value for the effect
-}
map : (a -> b) -> Request url a -> Request url b
map fn (Req request) =
    Req
        { method = request.method
        , url = request.url
        , expect = mapExpect fn request.expect
        , body = request.body
        , timeout = request.timeout
        , risky = request.risky
        , headers = request.headers
        , tracker = request.tracker
        }


{-| Config objects allow Requests that uses different URL representations
than Strings and different error types than Http.Error.
-}
type alias Config url error =
    { toUrl : url -> String
    , fromHttpError : Http.Error -> error
    , timeout : Maybe Float
    , risky : Bool
    , headers : List ( String, String )
    }


{-| The default config
-}
simple : Config String Http.Error
simple =
    { toUrl = identity
    , fromHttpError = identity
    , timeout = Nothing
    , risky = False
    , headers = []
    }


{-| An enum that represent all valid HTTP methods.
-}
type HttpMethod
    = GET
    | POST
    | PUT
    | DELETE
    | PATCH
    | OPTIONS
    | HEAD


httpMethodToString : HttpMethod -> String
httpMethodToString method =
    case method of
        GET ->
            "GET"

        POST ->
            "POST"

        PUT ->
            "PUT"

        DELETE ->
            "DELETE"

        PATCH ->
            "PATCH"

        OPTIONS ->
            "OPTIONS"

        HEAD ->
            "HEAD"


type Exclude a
    = ExcludeAll
    | ExcludeSome (Set a)


type alias Headers =
    { exclude : Exclude String
    , include : Dict String String
    }


resolveHeaders : List ( String, String ) -> Headers -> List Http.Header
resolveHeaders defaultHeaders headers =
    let
        exclude =
            case headers.exclude of
                ExcludeAll ->
                    Set.empty

                ExcludeSome set ->
                    set
    in
    List.filter
        (\( key, _ ) -> not (Set.member key exclude))
        (defaultHeaders ++ Dict.toList headers.include)
        |> List.map
            (\( key, value ) -> Http.header key value)


{-| Represents the expected response in an Http request
-}
type Expect value
    = ExpectJson (D.Decoder value)
    | ExpectString (String -> value)
    | ExpectBytes (BD.Decoder value)
    | ExpectWhatever (() -> value)


expectToHttpExpect : (Result Http.Error a -> msg) -> Expect a -> Http.Expect msg
expectToHttpExpect msg expect =
    case expect of
        ExpectJson decoder ->
            Http.expectJson msg decoder

        ExpectBytes decoder ->
            Http.expectBytes msg decoder

        ExpectString fn ->
            Http.expectString (Result.map fn >> msg)

        ExpectWhatever fn ->
            Http.expectWhatever (Result.map fn >> msg)


expectToHttpResolver : Expect a -> Http.Resolver Http.Error a
expectToHttpResolver expect =
    case expect of
        ExpectJson decoder ->
            Http.Tasks.resolveJson decoder

        ExpectBytes decoder ->
            Http.bytesResolver (resolveBytes decoder)

        ExpectString fn ->
            Http.Tasks.customResolver (fn >> Ok)

        ExpectWhatever fn ->
            Http.Tasks.customResolver (\_ -> Ok (fn ()))


resolveBytes : BD.Decoder a -> Http.Response Bytes -> Result Http.Error a
resolveBytes decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            case BD.decode decoder body of
                Just value ->
                    Ok value

                Nothing ->
                    Err (Http.BadBody "Invalid body")


mapExpect : (a -> b) -> Expect a -> Expect b
mapExpect fn expect =
    case expect of
        ExpectJson decoder ->
            ExpectJson (D.map fn decoder)

        ExpectBytes decoder ->
            ExpectBytes (BD.map fn decoder)

        ExpectString f ->
            ExpectString (f >> fn)

        ExpectWhatever f ->
            ExpectWhatever (f >> fn)


{-| Create a simple http request
-}
http : HttpMethod -> { url : url, expect : Expect a } -> Request url a
http method cfg =
    Req
        { method = method
        , url = cfg.url
        , expect = cfg.expect
        , timeout = Nothing
        , body = Http.emptyBody
        , risky = False
        , headers =
            { exclude = ExcludeSome Set.empty
            , include = Dict.empty
            }
        , tracker = Nothing
        }


{-| Create a GET request.
-}
get : { url : url, expect : Expect a } -> Request url a
get =
    http GET


{-| Create a POST request.
-}
post : { url : url, expect : Expect a } -> Request url a
post =
    http POST


{-| Create a PUT request.
-}
put : { url : url, expect : Expect a } -> Request url a
put =
    http PUT


{-| Create a DELETE request.
-}
delete : { url : url, expect : Expect a } -> Request url a
delete =
    http DELETE


{-| Create a PATCH request.
-}
patch : { url : url, expect : Expect a } -> Request url a
patch =
    http PATCH


{-| Create an OPTIONS request.
-}
options : { url : url, expect : Expect a } -> Request url a
options =
    http OPTIONS


{-| Create a HEAD request.
-}
head : { url : url, expect : Expect a } -> Request url a
head =
    http HEAD


{-| Declares a request that expects a JSON response handled with the given decoder.
-}
expectJson : D.Decoder a -> Request url b -> Request url a
expectJson decoder (Req request) =
    Req
        { method = request.method
        , url = request.url
        , expect = ExpectJson decoder
        , timeout = request.timeout
        , risky = request.risky
        , headers = request.headers
        , tracker = request.tracker
        , body = request.body
        }


{-| Declares a request that expects a binary response handled with the given decoder.
-}
expectBytes : BD.Decoder a -> Request url b -> Request url a
expectBytes decoder (Req request) =
    Req
        { method = request.method
        , url = request.url
        , expect = ExpectBytes decoder
        , timeout = request.timeout
        , risky = request.risky
        , headers = request.headers
        , tracker = request.tracker
        , body = request.body
        }


{-| Declares a request that expects a simple string response.
-}
expectString : Request url b -> Request url String
expectString (Req request) =
    Req
        { method = request.method
        , url = request.url
        , expect = ExpectString identity
        , timeout = request.timeout
        , risky = request.risky
        , headers = request.headers
        , tracker = request.tracker
        , body = request.body
        }


{-| Ignores the content of the received response.
-}
expectWhatever : Request url b -> Request url ()
expectWhatever (Req request) =
    Req
        { method = request.method
        , url = request.url
        , expect =
            ExpectWhatever (\_ -> ())
        , timeout = request.timeout
        , risky = request.risky
        , headers = request.headers
        , tracker = request.tracker
        , body = request.body
        }


{-| Declares the request as risky (or not) according to the boolean argument.

    Request.get
        { url = "https://example.com"
        , expect = Request.expectJson Json.Decode.string
        }
        |> Request.risky True

-}
risky : Bool -> Request url value -> Request url value
risky value (Req request) =
    Req { request | risky = value }


{-| Adds a list of headers to the request.

    Request.get
        { url = "https://example.com"
        , expect = Request.expectJson Json.Decode.string
        }
        |> Request.withHeaders [ ( "Authorization", "Bearer " ++ token ) ]

-}
withHeaders : List ( String, String ) -> Request url a -> Request url a
withHeaders headers (Req request) =
    request
        |> updateHeaders
            (updateInclude
                (Dict.fromList headers |> Dict.union)
            )
        |> Req


{-| Ignore specific default headers declared in the config.

    Request.get
        { url = "https://example.com"
        , expect = Request.expectJson Json.Decode.string
        }
        |> Request.excludeDefaultHeaders [ "Authorization", "Cookie" ]

-}
excludeDefaultHeaders : List String -> Request url a -> Request url a
excludeDefaultHeaders headers (Req request) =
    request
        |> updateHeaders
            (updateExclude
                (always (ExcludeSome <| Set.fromList headers))
            )
        |> Req


{-| Ignore all default headers declared in the config.

    Request.get
        { url = "https://example.com"
        , expect = Request.expectJson Json.Decode.string
        }
        |> Request.ignoreDefaultHeaders

-}
ignoreDefaultHeaders : Request url a -> Request url a
ignoreDefaultHeaders (Req request) =
    request
        |> updateHeaders
            (updateExclude
                (always ExcludeAll)
            )
        |> Req


updateHeaders : (Headers -> Headers) -> { a | headers : Headers } -> { a | headers : Headers }
updateHeaders update record =
    { record | headers = update record.headers }


updateInclude : (b -> b) -> { a | include : b } -> { a | include : b }
updateInclude update record =
    { record | include = update record.include }


updateExclude : (b -> b) -> { a | exclude : b } -> { a | exclude : b }
updateExclude update record =
    { record | exclude = update record.exclude }


{-| Convert a request to a command.

This function requires a config object. If you don not need one, use the `simple`
config exported by this package.

    Request.get
        { url = "https://example.com"
        , expect = Request.expectJson Json.Decode.string
        }
        |> Request.cmd Request.simple ResponseReceived

-}
cmd : Config url error -> (Result error value -> msg) -> Request url value -> Cmd msg
cmd cfg msg (Req request) =
    let
        httpRequest =
            iff request.risky Http.request Http.riskyRequest
    in
    httpRequest
        { method = request.method |> httpMethodToString
        , headers = resolveHeaders cfg.headers request.headers
        , url = cfg.toUrl request.url
        , body = request.body
        , expect = expectToHttpExpect (Result.mapError cfg.fromHttpError >> msg) request.expect
        , tracker = request.tracker
        , timeout = request.timeout
        }


{-| Convert a request to a task.

This function requires a config object. If you don not need one, use the `simple`
config exported by this package.

    Request.get
        { url = "https://example.com"
        , expect = Request.expectJson Json.Decode.string
        }
        |> Request.task Request.simple

-}
task : Config url error -> Request url value -> Task error value
task cfg (Req request) =
    let
        httpTask =
            iff request.risky Http.task Http.riskyTask
    in
    httpTask
        { method = request.method |> httpMethodToString
        , headers = resolveHeaders cfg.headers request.headers
        , url = cfg.toUrl request.url
        , body = request.body
        , resolver = expectToHttpResolver request.expect
        , timeout = request.timeout
        }
        |> Task.mapError cfg.fromHttpError


iff : Bool -> a -> a -> a
iff condition a b =
    if condition then
        a

    else
        b
