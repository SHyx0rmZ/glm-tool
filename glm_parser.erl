-module(glm_parser).
-export([parse/1]).

-record(glm_file, {offset, name, size, compressed, timestamp}).

parse(Binary) ->
    HeaderLocationOffset = byte_size(Binary) - 4,
    <<_:HeaderLocationOffset/binary, HeaderLocation:32/little-unsigned>> = Binary,
    parse_header(Binary, HeaderLocation).

parse_header(Binary, HeaderOffset) when is_integer(HeaderOffset) ->
    <<_:HeaderOffset/binary, Header/binary>> = Binary,
    parse_header(Binary, Header);
parse_header(Binary, <<"CHNKBLXX", Position:32/little-unsigned, Size:32/little-unsigned, Count:32/little-unsigned, Rest/binary>>) ->
    <<_:Position/binary, NameStorage/binary>> = Binary,
    Names = parse_names(NameStorage, Count),
    Files = parse_files(Rest, []),
    lists:map(fun(File) -> File#glm_file{ name = maps:get(File#glm_file.name, Names) } end, Files).

parse_names(Names, Count) ->
    parse_names(Names, Count, #{}, 0).

parse_names(_, 0, Names, _) -> Names;
parse_names(Binary, Count, Names, Offset) ->
    Length = string_length(Binary),
    <<String:Length/binary, 0, Rest/binary>> = Binary,
    Names1 = maps:put(Offset, binary_to_list(String), Names),
    parse_names(Rest, Count - 1, Names1, Offset + Length + 1).

string_length(String) ->
    string_length(String, 0).

string_length(<<0, _/binary>>, Length) -> Length;
string_length(<<_, Rest/binary>>, Length) -> string_length(Rest, Length + 1).

parse_files(<<Position:32/little-integer, Size:32/little-integer, OriginalSize:32/little-integer, NamePosition:32/little-integer, Flags:16/bits, Timestamp:32/little-integer, Rest/binary>>, Files) ->
    File = #glm_file{ offset = Position, size = Size, name = NamePosition, compressed = parse_flags(Flags, compressed, OriginalSize), timestamp = erlang_timestamp(Timestamp) },
    parse_files(Rest, [ File | Files ]);
parse_files(_, Files) ->
    lists:reverse(Files).

parse_flags(<<_:7, Compressed:1, _:8>>, compressed, OriginalSize) ->
    case Compressed of
        1 -> { compressed, OriginalSize };
        0 -> uncompressed
    end.

erlang_timestamp(Timestamp) ->
    { Timestamp div 1000000, Timestamp div 1000 rem 1000, Timestamp rem 1000 }.
