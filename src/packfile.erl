%%
%% Partial Git Implementation
%%

-module(packfile).
-export([get_packfile_data/3]).

get_packfile_data(Git, PackFilePath, Offset) ->
  case file:open(PackFilePath, [read, binary]) of
    {ok, IoDevice} ->
      read_packfile_object_offset(Git, IoDevice, Offset);
    {error, _Reason} ->
      error
  end.

read_packfile_object_offset(Git, IoDevice, Offset) ->
  file:position(IoDevice, Offset),
  {ok, Byte} = file:read(IoDevice, 1),
  <<ContinueBit:1, Type:3, InitSize:4>> = Byte,
  TypeTerm = type_int_to_term(Type),
  %io:fwrite("Object Data: ~p ~p ~p~n", [ContinueBit, Type, InitSize]),
  {FinalType, Size, Data} = read_object_data(Git, IoDevice, ContinueBit, InitSize, 4, Offset, Offset + 1, TypeTerm),
  %io:fwrite("Final Object Data: ~p ~p ~p~n", [Size, FinalType, Data]),
  {FinalType, Size, Data}.

read_object_data(Git, IoDevice, 1, Size, Shift, OrigOffset, FileOffset, TypeTerm) ->
  {ok, Byte} = file:read(IoDevice, 1),
  <<ContinueBit:1, NextSize:7>> = Byte,
  % bit shift the size
  SizeOr = NextSize bsl Shift,
  NewSize = Size bor SizeOr,
  NextShift = Shift + 7,
  %io:fwrite("Object Data: [~p] ~p ~p (~p)~n", [FileOffset, ContinueBit, NextSize, NewSize]),
  read_object_data(Git, IoDevice, ContinueBit, NewSize, NextShift, OrigOffset, FileOffset + 1, TypeTerm);
read_object_data(Git, IoDevice, 0, Size, Shift, OrigOffset, FileOffset, ofs_delta) ->
  read_ofs_deltified_object_data(Git, IoDevice, Size, Shift, FileOffset, OrigOffset);
read_object_data(Git, IoDevice, 0, Size, Shift, OrigOffset, FileOffset, ref_delta) ->
  read_ref_deltified_object_data(Git, IoDevice, Size, Shift, FileOffset, OrigOffset);
read_object_data(_Git, IoDevice, 0, Size, _Shift, _OrigOffset, FileOffset, NormalType) ->
  %io:fwrite("File Offset:~p~n", [FileOffset]),
  file:position(IoDevice, FileOffset),
  Z = zlib:open(),
  ok = zlib:inflateInit(Z),
  Data = inflate_object_data(Z, IoDevice, []),
  %io:fwrite("Bits:~p~n", [Data]),
  ok = zlib:inflateEnd(Z),
  zlib:close(Z),
  {NormalType, Size, Data}.

% git rev-list --objects --all | git pack-objects --window=250 --depth=250 --stdout > #{packfile_pack}
% git index-pack -o #{packfile_idx} #{packfile_pack}
read_ref_deltified_object_data(Git, IoDevice, Size, Offset, FileOffset, OrigOffset) ->
  {ok, Sha} = file:read(IoDevice, 20),
  HexSha = hex:bin_to_hexstr(Sha),
  {TypeTerm, _BaseSize, BaseData} = git:object_data(Git, HexSha),
  %io:fwrite("Base:~p~n", [BaseSize]),
  {TypeTerm, _DeltaSize, DeltaData} = read_object_data(Git, IoDevice, 0, Size, Offset, OrigOffset, FileOffset + 20, TypeTerm),
  %io:fwrite("DeltaData: ~p:~p:~p~n", [TypeTerm, DeltaSize, DeltaData]),
  PatchedData = patch_delta(DeltaData, BaseData),
  {TypeTerm, length(PatchedData), list_to_binary(PatchedData)}.

read_ofs_deltified_object_data(Git, IoDevice, Size, Offset, FileOffset, OrigOffset) ->
  {ok, BaseOffset, BytesRead} = get_base_offset(IoDevice),
  NewOffset = OrigOffset - BaseOffset,
  %io:fwrite("BaseOffset: ~p:~p:~p~n", [NewOffset, OrigOffset, BaseOffset]),
  {TypeTerm, _BaseSize, BaseData} = read_packfile_object_offset(Git, IoDevice, NewOffset),
  file:position(IoDevice, FileOffset + BytesRead),
  {TypeTerm, _DeltaSize, DeltaData} = read_object_data(Git, IoDevice, 0, Size, Offset, OrigOffset, FileOffset + BytesRead, TypeTerm),
  %io:fwrite("DeltaData: ~p:~p:~p~n", [TypeTerm, DeltaSize, DeltaData]),
  PatchedData = patch_delta(DeltaData, BaseData),
  {TypeTerm, length(PatchedData), list_to_binary(PatchedData)}.

patch_delta(DeltaData, BaseData) ->
  DeltaList = binary_to_list(DeltaData),
  BaseList = binary_to_list(BaseData),
  {ok, _SrcSize, PosA} = patch_delta_header_size(DeltaList, 1),
  % SrcSize should == BaseData.size
  %io:fwrite("Patch:SrcSize : ~p:~p~n", [SrcSize, PosA]),
  {ok, _DestSize, PosB} = patch_delta_header_size(DeltaList, PosA),
  %io:fwrite("Patch:DestSize: ~p:~p~n", [DestSize, PosB]),
  PatchedData = patch_delta([], BaseList, DeltaList, PosB, length(DeltaList) + 1),
  %io:fwrite("PatchData: ~p~n", [length(PatchedData)]),
  PatchedData.

patch_delta(PatchedList, _BaseList, _DeltaList, _DeltaPos, _DeltaPos) ->
  PatchedList;
patch_delta(PatchedList, BaseList, DeltaList, DeltaPos, Size) ->
  Byte = lists:nth(DeltaPos, DeltaList),
  StartPos = DeltaPos + 1,
  <<PatchBit:1, PatchData:7>> = <<Byte>>,
  %io:fwrite("Patch: ~p:~p:~p~n", [PatchBit, DeltaPos, Size]),
  case PatchBit of
    1 ->
      % append data from base list
      <<_Pb:1, S16:1, S8:1, S0:1, Of24:1, Of16:1, Of8:1, Of0:1>> = <<Byte>>,
      {CpOffA,  DeltaOffA} = calc_cp(DeltaList, StartPos,  Of0,   0, 0),
      {CpOffB,  DeltaOffB} = calc_cp(DeltaList, DeltaOffA, Of8,   8, CpOffA),
      {CpOffC,  DeltaOffC} = calc_cp(DeltaList, DeltaOffB, Of16, 16, CpOffB),
      {CpOff,   DeltaOffD} = calc_cp(DeltaList, DeltaOffC, Of24, 24, CpOffC),
      {CpSizeA, DeltaOffE} = calc_cp(DeltaList, DeltaOffD, S0,    0, 0),
      {CpSizeB, DeltaOffF} = calc_cp(DeltaList, DeltaOffE, S8,    8, CpSizeA),
      {CpSize,  DeltaOff}  = calc_cp(DeltaList, DeltaOffF, S16,  16, CpSizeB),
      %io:fwrite("Off/Size: ~p:~p:~p~n", [CpOff, CpSize, DeltaOff]),
      PatchDataList = lists:sublist(BaseList, CpOff + 1, CpSize),
      patch_delta(PatchedList ++ PatchDataList, BaseList, DeltaList, DeltaOff, Size);
    0 ->
      % append data from delta list
      %io:fwrite("Patch Delta Data (~p:~p)~n", [StartPos, PatchData]),
      PatchDataList = lists:sublist(DeltaList, StartPos, PatchData),
      patch_delta(PatchedList ++ PatchDataList, BaseList, DeltaList, StartPos + PatchData, Size)
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
  %io:fwrite("Byte: ~p:~p:~p:~p:~p:~p~n", [Byte, Pos, Shift, Size, ContinueBit, NextSize]),
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

inflate_object_data(Z, IoDevice, SoFar) ->
  case file:read(IoDevice, 4096) of
    {ok, Bytes} ->
      _Inflated = case catch zlib:inflate(Z, Bytes) of
          {'EXIT', {'data_error', _Backtrace} } ->
              %io:format("zlib:inflate data_error,~n"),
              SoFar;
          {'EXIT', _Reason} ->
              %io:format("zlib:inflate error -> [~p]~n", [Reason]),
              SoFar;
          [] ->
              SoFar;
          Iolist ->
              [Data] = Iolist,
              ListData = list_to_binary([binary_to_list(Data)|SoFar]),
              %io:fwrite("Size: ~p~n", [size(ListData)]),
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

