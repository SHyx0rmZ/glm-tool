-module(glm_tool).
-export([main/1]).

-include ("glm_file.hrl").

main(List) when is_list(List) ->
    Args = [ atom_to_list(Atom) || Atom <- List ],
    parse_options(Args).

parse_options([ Command | Args ]) ->
    case list_to_atom(Command) of
        list ->
            [ FilePath ] = Args,
            { ok, FileContents } = file:read_file(FilePath),
            GLM = glm_parser:parse(FileContents),
            io:format("~p~n", [ GLM ]);
        extract ->
            [ FilePath, TargetDirectory ] = Args,
            { ok, FileContents } = file:read_file(FilePath),
            GLM = glm_parser:parse(FileContents),
            lists:foreach(fun(File) -> glm_extractor:extract(FileContents, File, TargetDirectory) end, GLM);
        _ ->
            io:format("Usage:~n"),
            io:format("  glm_tool list <file-path>~n"),
            io:format("  glm_tool extract <file-path> <target-directory>~n")
    end,
    init:stop().
