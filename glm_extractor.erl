-module(glm_extractor).
-export([extract/3]).

-include("glm_file.hrl").

extract(Archive, File, TargetDirectory) when is_record(File, glm_file) ->
    Directory = directorify(TargetDirectory),
    FilePath = Directory ++ File#glm_file.name,
    case file:make_dir(Directory) of
        ok ->
            ok;
        { error, eexist } ->
            ok
    end,
    Data = extract(Archive, File),
    file:write_file(FilePath, Data).

directorify(Directory) ->
    directorify(lists:reverse(Directory), reversed).

directorify([ $/ | Directory ], reversed) -> lists:reverse([ $/ | Directory ]);
directorify(Directory, reversed) -> lists:reverse([ $/ | Directory ]).

extract(Archive, #glm_file{ name = Name, offset = Offset, size = Size, compressed = { compressed, OriginalSize } }) ->
    io:format("Extracting and decompressing ~s~n", [ Name ]),
    Z = zlib:open(),
    ok = zlib:inflateInit(Z),
    <<_:Offset/binary, RawData:Size/binary, _/binary>> = Archive,
    Data = zlib:inflate(Z, RawData),
    ok = zlib:inflateEnd(Z),
    ok = zlib:close(Z),
    Data;
extract(Archive, #glm_file{ name = Name, offset = Offset, size = Size }) ->
    io:format("Extracting ~s~n", [ Name ]),
    <<_:Offset/binary, Data:Size/binary, _/binary>> = Archive,
    Data.
