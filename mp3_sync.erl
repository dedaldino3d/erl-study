-module(mp3_sync).
-export([find_sync/2]).
-vsn("0.1.1").
-author({dedaldino, antonio}).



find_sync(Bin, N) ->
    case is_header(N, Bin) of
        {ok, Len1, _} ->
            case is_header(N, Bin) of
                {ok, Len2, _} ->
                    case is_header(N + Len1 + Len2, Bin) of
                        {o, _, _} ->
                            {ok, N};
                        error ->
                            find_sync(Bin,N + 1)
                    end;
                error -> 
                    find_sync(Bin,N + 1)
            end;
        error ->
            find_sync(Bin, N + 1)
    end.

is_header(N, Bin) ->
    unpack_header(get_word(N, Bin)).

get_word(N, Bin) ->
    {_,<<C:4/binary>>,<<_/binary>>} = split_binary(N, Bin),
    C.

unpack_header(X) ->
    try decode_header(X)
    catch
        _:_ -> error
    end.

decode_header(<<2#11111111111:11,B:2,C:2,_D:1,E:4,F:2,G:1,Bits:9>>) ->
    Vsn = case B  of
            0 -> {2,5};
            1 -> exit(badVsn);
            2 -> 2;
            3 -> 1
        end,
    Layer = case C of
                0 -> exit(badLayer);
                1 -> 3;
                2 -> 2;
                3 -> 1
            end,
    % Protection D
    BitRate = bitrate(Vsn, Layer, E) * 1000, % bitrate not defined yet
    SampleRate = samplerate(Vsn, F), % samplerate not defined yet
    Padding = G,
    FrameLength = framelength(Layer, BitRate, SampleTate,Padding), % framelenght not defined yet
    if
        FrameLength < 21 ->
            exit(frameLenght);
        true ->
            {ok, FrameLenght, {Layer, BitRate, SampleRate, Vsn, Bits}}
    end;

decode_header(_) ->
    exit(badHeader).
