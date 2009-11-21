%%
%% Partial Git Implementation
%%

-module(packfile).
-export([get_packfile_data/2]).

get_packfile_data(PackFilePath, Offset) ->
  case file:open(PackFilePath, [read, binary]) of
    {ok, IoDevice} ->
      file:position(IoDevice, Offset),
      {ok, Byte} = file:read(IoDevice, 1),
      <<ContinueBit:1, Type:3, InitSize:4>> = Byte,
      %io:fwrite("Object Data: ~p ~p ~p~n", [ContinueBit, Type, InitSize]),
      {Size, Data} = read_object_data(IoDevice, ContinueBit, InitSize, 0),
      %io:fwrite("Final Object Data: ~p ~p ~p~n", [Size, Type, Data]),
      {type_int_to_term(Type), Size, Data};
    {error, Reason} ->
      error
  end.

read_object_data(IoDevice, 1, Size, Offset) ->
  {ok, Byte} = file:read(IoDevice, 1),
  <<ContinueBit:1, NextSize:7>> = Byte,
  % bit shift the size
  NextOffset = Offset + 4,
  SizeOr = NextSize bsl NextOffset,
  NewSize = Size bor SizeOr,
  %io:fwrite("Object Data: ~p ~p (~p)~n", [ContinueBit, NextSize, NewSize]),
  read_object_data(IoDevice, ContinueBit, NewSize, NextOffset);
read_object_data(IoDevice, 0, Size, Offset) ->
  Z = zlib:open(),
  ok = zlib:inflateInit(Z),
  Data = inflate_object_data(Z, IoDevice, []),
  %io:fwrite("Bits:~p~n", [Data]),
  ok = zlib:inflateEnd(Z),
  zlib:close(Z),
  {Size, Data}.

inflate_object_data(Z, IoDevice, SoFar) ->
  case file:read(IoDevice, 4096) of
    {ok, Bytes} ->
      Inflated = case catch zlib:inflate(Z, Bytes) of
          {'EXIT', {'data_error', _Backtrace} } ->
              io:format("zlib:inflate data_error,~n"),
              SoFar;
          {'EXIT', Reason} ->
              io:format("zlib:inflate error -> [~p]~n", [Reason]),
              SoFar;
          Iolist ->
              [Data] = Iolist,
              ListData = list_to_binary([binary_to_list(Data)|SoFar]),
              io:fwrite("Size: ~p~n", [size(ListData)]),          
              inflate_object_data(Z, IoDevice, ListData)
      end;
    _Else ->
      SoFar
  end.

type_int_to_term(ObjInt) ->  
  case ObjInt of
    1 -> commit;
    2 -> tree;
    3 -> blob;
    4 -> tag;
    6 -> delta_ofs;
    7 -> delta_base
  end.