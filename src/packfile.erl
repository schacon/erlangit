%%
%% Partial Git Implementation
%%

-module(packfile).
-export([get_packfile_data/2]).

get_packfile_data(PackFilePath, Offset) ->
  case file:open(PackFilePath, [read, binary]) of
    {ok, IoDevice} ->
      read_packfile_object_offset(IoDevice, Offset);
    {error, _Reason} ->
      error
  end.

read_packfile_object_offset(IoDevice, Offset) ->
  file:position(IoDevice, Offset),
  {ok, Byte} = file:read(IoDevice, 1),
  <<ContinueBit:1, Type:3, InitSize:4>> = Byte,
  TypeTerm = type_int_to_term(Type),
  %io:fwrite("Object Data: ~p ~p ~p~n", [ContinueBit, Type, InitSize]),
  {FinalType, Size, Data} = read_object_data(IoDevice, ContinueBit, InitSize, 4, Offset, Offset + 1, TypeTerm),
  io:fwrite("Final Object Data: ~p ~p ~p~n", [Size, FinalType, Data]),
  {FinalType, Size, Data}.

read_object_data(IoDevice, 1, Size, Offset, OrigOffset, FileOffset, TypeTerm) ->
  {ok, Byte} = file:read(IoDevice, 1),
  <<ContinueBit:1, NextSize:7>> = Byte,
  % bit shift the size
  SizeOr = NextSize bsl Offset,
  NewSize = Size bor SizeOr,
  NextOffset = Offset + 7,
  %io:fwrite("Object Data: ~p ~p (~p)~n", [ContinueBit, NextSize, NewSize]),
  read_object_data(IoDevice, ContinueBit, NewSize, NextOffset, OrigOffset, FileOffset + 1, TypeTerm);
read_object_data(IoDevice, 0, Size, Offset, OrigOffset, FileOffset, ofs_delta) ->
  read_ofs_deltified_object_data(IoDevice, Size, Offset, FileOffset, OrigOffset);
read_object_data(IoDevice, 0, Size, Offset, OrigOffset, FileOffset, ref_delta) ->
  read_ref_deltified_object_data(IoDevice, Size, Offset, FileOffset, OrigOffset);
read_object_data(IoDevice, 0, Size, _Offset, _OrigOffset, _FileOffset, NormalType) ->
  Z = zlib:open(),
  ok = zlib:inflateInit(Z),
  Data = inflate_object_data(Z, IoDevice, []),
  %io:fwrite("Bits:~p~n", [Data]),
  ok = zlib:inflateEnd(Z),
  zlib:close(Z),
  {NormalType, Size, Data}.

read_ofs_deltified_object_data(IoDevice, Size, Offset, FileOffset, OrigOffset) ->
  {ok, BaseOffset, BytesRead} = get_base_offset(IoDevice),
  NewOffset = OrigOffset - BaseOffset,
  %io:fwrite("BaseOffset: ~p:~p:~p~n", [NewOffset, OrigOffset, BaseOffset]),
  {TypeTerm, BaseSize, BaseData} = read_packfile_object_offset(IoDevice, NewOffset),
  file:position(IoDevice, FileOffset + BytesRead),
  {TypeTerm, _DeltaSize, DeltaData} = read_object_data(IoDevice, 0, Size, Offset, OrigOffset, FileOffset + BytesRead, TypeTerm),
  %io:fwrite("DeltaData: ~p:~p:~p~n", [TypeTerm, DeltaSize, DeltaData]),
  PatchedData = patch_delta(DeltaData, BaseData),
  {TypeTerm, length(PatchedData), list_to_binary(PatchedData)}.

patch_delta(DeltaData, BaseData) ->
  DeltaList = binary_to_list(DeltaData),
  BaseList = binary_to_list(BaseData),
  {ok, SrcSize, PosA} = patch_delta_header_size(DeltaList, 1),
  % SrcSize should == BaseData.size
  io:fwrite("PatchA: ~p:~p~n", [SrcSize, PosA]),
  {ok, DestSize, PosB} = patch_delta_header_size(DeltaList, PosA),
  io:fwrite("PatchB: ~p:~p~n", [DestSize, PosB]),
  PatchedData = patch_delta([], BaseList, DeltaList, PosB, length(DeltaList) + 1),
  io:fwrite("PatchData: ~p~n", [length(PatchedData)]),
  PatchedData.

patch_delta(PatchedList, _BaseList, _DeltaList, _DeltaPos, _DeltaPos) ->
  PatchedList;
patch_delta(PatchedList, BaseList, DeltaList, DeltaPos, Size) ->
  io:fwrite("Patch Delta ~p:~p~n", [DeltaPos, DeltaList]),
  Byte = lists:nth(DeltaPos, DeltaList),
  io:fwrite("Patch Delta DataA~n"),
  <<PatchBit:1, PatchData:7>> = <<Byte>>,
  case PatchBit of
    1 ->
      % append data from base list
      <<_Pb:1, S16:1, S8:1, S0:1, Of24:1, Of16:1, Of8:1, Of0:1>> = <<Byte>>,
      StartPos = DeltaPos + 1,
      {CpOffA,  DeltaOffA} = calc_cp(DeltaList, StartPos,  Of0,   0, 0),
      {CpOffB,  DeltaOffB} = calc_cp(DeltaList, DeltaOffA, Of8,   8, CpOffA),
      {CpOffC,  DeltaOffC} = calc_cp(DeltaList, DeltaOffB, Of16, 16, CpOffB),
      {CpOff,   DeltaOffD} = calc_cp(DeltaList, DeltaOffC, Of24, 24, CpOffC),
      {CpSizeA, DeltaOffE} = calc_cp(DeltaList, DeltaOffD, S0,    0, 0),
      {CpSizeB, DeltaOffF} = calc_cp(DeltaList, DeltaOffE, S8,    8, CpSizeA),
      {CpSize,  DeltaOff}  = calc_cp(DeltaList, DeltaOffF, S16,  16, CpSizeB),
      io:fwrite("Off/Size: ~p:~p:~p~n", [CpOff, CpSize, DeltaOff]),
      PatchDataList = lists:sublist(BaseList, CpOff + 1, CpSize),
      patch_delta(PatchedList ++ PatchDataList, BaseList, DeltaList, DeltaOff, Size);
    0 ->
      % append data from delta list
      io:fwrite("Patch Delta Data~n"),
      PatchDataList = lists:sublist(DeltaList, DeltaPos, PatchData),
      patch_delta(PatchedList ++ PatchDataList, BaseList, DeltaList, DeltaPos + PatchData, Size)
  end.


calc_cp(DList, DPos, 1, Shift, Cp) ->
  Data = lists:nth(DPos, DList),
  ShiftCp = Data bsl Shift,
  NewCp = Cp bor ShiftCp,
  {NewCp, DPos + 1};
calc_cp(_DList, DPos, 0, _Shift, Cp) ->
  {Cp, DPos}.

patch_delta_header_size(DeltaData, Pos) ->
  patch_delta_header_size(DeltaData, Pos, 0, 1, 0).

patch_delta_header_size(DeltaData, Pos, Shift, 1, Size) ->
  Byte = lists:nth(Pos, DeltaData),
  <<ContinueBit:1, NextSize:7>> = <<Byte>>,
  io:fwrite("Byte: ~p:~p:~p:~p:~p:~p~n", [Byte, Pos, Shift, Size, ContinueBit, NextSize]),
  SizeOr = NextSize bsl Shift,
  NewSize = Size bor SizeOr,
  patch_delta_header_size(DeltaData, Pos + 1, Shift + 7, ContinueBit, NewSize);
patch_delta_header_size(_DeltaData, Pos, _Shift, 0, Size) ->
  {ok, Size, Pos}.

get_base_offset(IoDevice) ->
  {ok, Byte} = file:read(IoDevice, 1),
  <<ContinueBit:1, NextData:7>> = Byte,
  get_base_offset(IoDevice, ContinueBit, NextData, 1).

get_base_offset(IoDevice, 1, Offset, BytesRead) ->
  {ok, Byte} = file:read(IoDevice, 1),
  <<ContinueBit:1, NextData:7>> = Byte,
  %io:fwrite("Data:~p:~p~n", [ContinueBit, NextData]),
  Offset2 = Offset + 1,
  Offset3 = Offset2 bsl 7,
  NewOffset = Offset3 bor NextData,
  get_base_offset(IoDevice, ContinueBit, NewOffset, BytesRead + 1);
get_base_offset(_IoDevice, 0, Offset, BytesRead) ->
  {ok, Offset, BytesRead}.


read_ref_deltified_object_data(IoDevice, _Size, _Offset, _FileOffset, _OrigOffset) ->
  {ok, Sha} = file:read(IoDevice, 20),
  _HexSha = hex:bin_to_hexstr(Sha),
  % find the offset of HexSha, run read_object_data again
  % {Size, Data} = get_packfile_data(Path, Offset),
  % DeltaData = read_object_data(%here),
  % patch_delta(Data, DeltaData)
  {100, <<"Ref Data">>}.

inflate_object_data(Z, IoDevice, SoFar) ->
  case file:read(IoDevice, 4096) of
    {ok, Bytes} ->
      _Inflated = case catch zlib:inflate(Z, Bytes) of
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
    6 -> ofs_delta;
    7 -> ref_delta
  end.

