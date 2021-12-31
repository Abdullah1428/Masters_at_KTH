% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW1: Rudy - a small web server
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% --------------------------------------------------------
% 1 A HTTP parser
-module(http).
-export([parse_request/1,ok/1,get/1]).


% from RFC 2016 the request consists of the following
% Request = Request-Line 
%           *(( general-header   
%             | request-header
%             | entity-header ) CRLF)  
%           CRLF
%           [ message-body ]          

% --------------------------------------------------------
% 1.1 a HTTP Request
% defining the main parse request funtion to follow
% the above request format
parse_request(R0) ->
  {Request, R1} = request_line(R0),
  {Headers, R2} = headers(R1),
  {Body, _} = message_body(R2),
  {Request, Headers, Body}.

% --------------------------------------------------------
% 1.2 the request line
% starting with request line
% Request-Line = Method SP Request-URI SP HTTP-Version CRLF
request_line([$G, $E, $T, 32 |R0]) -> % here | is notation for union
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
    [13,10|R3] = R2, % 13,10 is the CRLF = carriage return line feed
    {{get, URI, Ver}, R3}.

% --------------------------------------------------------
% 1.3 the URI
% request_uri method to get URI
% Recursive definition
% add first character to list ([C|R0]) and now call
% recursively to add rest of characters ({Rest,R1} = request_uri(R0)
% break the sequence when C = space = 32
request_uri([32|R0])->
    {[], R0};
request_uri([C|R0]) ->
    {Rest, R1} = request_uri(R0),
    {[C|Rest], R1}.

% --------------------------------------------------------
% 1.4 version information
% http_version method to get the version of http
% defining two version 1.1 and 1.0
http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
    {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
    {v10, R0}.

% --- the request line methods are done
% --- now we move to headers section

% --------------------------------------------------------
% 1.5 headers

headers([13,10|R0]) ->
    {[],R0};
headers(R0) ->
    {Header, R1} = header(R0),
    {Rest, R2} = headers(R1),
    {[Header|Rest], R2}.

header([13,10|R0]) ->
    {[], R0};
header([C|R0]) ->
    {Rest, R1} = header(R0),
    {[C|Rest], R1}.

% --- the header methods are done
% --- now we move to body section

% --------------------------------------------------------
% 1.6 the body
% assuming that everything left is body which
% is not the case in actual
message_body(R) ->
    {R, []}.

% --------------------------------------------------------
% 1.7 a small test
% done everything working as expected

% --------------------------------------------------------
% 1.8 what about replies

% ok body with status code 200 
ok(Body) ->
    "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

% get uri 
get(URI) ->
    "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n". 

% --------------------------------------------------------