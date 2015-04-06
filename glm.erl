-module (glm).
-export ([main/1]).

main(L) when is_list(L) ->
    [ P ] = [ atom_to_list(Atom) || Atom <- L ],
    { ok, F } = file:read_file(P),
    G = parse(F),
    io:format("~p~n~p~n", [P, G]),
    init:stop().

parse(Binary) ->
    Size = byte_size(Binary),
    Offset = Size - 4,
    <<_:Offset/binary, HeaderLocation:32/little-integer>> = Binary,
    <<Payload:HeaderLocation/binary, Header/binary>> = Binary,
    {header, HeaderData} = parse_header(Payload, Header).

parse_header(Payload, Binary) ->
    <<"CHNKBLXX", FileNamePosition:32/little-integer, FileNameSize:32/little-integer, FileNameCount:32/little-integer, MetaData/binary>> = Binary,
    <<_:FileNamePosition/binary, FileNames/binary>> = Payload,
    Files = parse_header(FileNames, MetaData, FileNameCount, []),
    Data = {header, {
        {filename_position, FileNamePosition},
        {filename_size, FileNameSize},
        {filename_count, FileNameCount},
        {files, Files}
    }}.

parse_header(Payload, Binary, 0, List) ->
    List;
parse_header(Payload, Binary, FileCount, List) ->
    <<Position:32/little-integer, Size:32/little-integer, OriginalSize:32/little-integer, NamePosition:32/little-integer, Flags:16/little-integer, Timestamp:32/little-integer, Rest/binary>> = Binary,
    File = {
        {name, parse_name(Payload, NamePosition)},
        {position, Position},
        {size, Size},
        {original_size, OriginalSize},
        {flags, Flags},
        {timestamp, Timestamp}
    },
    parse_header(Payload, Rest, FileCount - 1, [ File | List ]).

parse_name(Binary, Position) ->
    <<_:Position/binary, NameBinary/binary>> = Binary,
    Length = string_length(NameBinary),
    <<Name:Length/binary, 0, _/binary>> = NameBinary,
    binary_to_list(Name).

string_length(String) ->
    string_length(String, 0).

string_length(String, N) ->
    <<C, Rest/binary>> = String,
    case C of
        0 ->
            N;
        _ ->
            string_length(Rest, N + 1)
    end.
