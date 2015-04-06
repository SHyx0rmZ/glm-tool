-module(glm_tool).
-export([main/1]).

main(List) when is_list(List) ->
    Args = [ atom_to_list(Atom) || Atom <- List ],
    [ FilePath ] = Args,
    { ok, FileContents } = file:read_file(FilePath),
    GLM = glm_parser:parse(FileContents),
    io:format("~p~n", [ GLM ]),
    init:stop().
