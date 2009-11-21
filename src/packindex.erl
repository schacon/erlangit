%%
%% Partial Git Implementation
%%

-module(packindex).
-export([extract_packfile_index/1, object_offset/2]).

-include("packindex.hrl").

%%%
% get an object offset from an index record
%%%
object_offset(Index, ObjectSha) ->
  ShaOffsets = lists:zip(Index#index.shalist, Index#index.offsets),
  case lists:keyfind(ObjectSha, 1, ShaOffsets) of
    {ObjectSha, Offset} ->
      {ok, Offset};
    _Else ->
      not_found
  end.

%%%
% extract a sha offset from packfile index data
% TODO: check to see if this is version 1 or version 2 (currently only v2)
% TODO: parse large offset section if it exists (curr. assumes it does not)
%%%
extract_packfile_index(Data) ->
  {Header, Data2}  = split_binary(Data, 4),  % \377tOc
  {Version, Data3} = split_binary(Data2, 4), % 0002
  {FanoutTable, Size, Data4} = extract_fanout(Data3),
  {ShaList, Data5} = extract_sha_list(Data4, Size),
  {CrcList, Data6} = extract_crc(Data5, Size),
  {OffsetList, Data7} = extract_offsets(Data6, Size),
  {PackCs, Data8} = split_binary(Data7, 20),
  {_IdxCs, _Empty} = split_binary(Data8, 20),
  Index = #index{header=Header, version=Version, size=Size, fanout=FanoutTable, 
    shalist=ShaList, crclist=CrcList, offsets=OffsetList, packcs=hex:bin_to_hexstr(PackCs)},
  {ok, Index}.

%%%
% extract offset list from packfile index
%%%
extract_offsets(IndexData, Size) ->
  extract_offsets(IndexData, Size, 0, []).
extract_offsets(IndexData, Size, Size, Listing) ->
  {lists:reverse(Listing), IndexData};
extract_offsets(IndexData, Size, Total, Listing) ->
  {Offset, IndexDataRem} = split_binary(IndexData, 4), % Offset
  <<OffsetInt:32>> = Offset,
  extract_offsets(IndexDataRem, Size, Total + 1, [OffsetInt|Listing]).

%%%
% extract crc checksums from packfile index
%%%
extract_crc(IndexData, Size) ->
  extract_crc(IndexData, Size, 0, []).
extract_crc(IndexData, Size, Size, Listing) ->
  {lists:reverse(Listing), IndexData};
extract_crc(IndexData, Size, Total, Listing) ->
  {Crc, IndexDataRem} = split_binary(IndexData, 4), % CRC
  extract_crc(IndexDataRem, Size, Total + 1, [Crc|Listing]).

%%%
% extract sha listing from packfile index
%%%
extract_sha_list(IndexData, Size) ->
  extract_sha_list(IndexData, Size, 0, []).
extract_sha_list(IndexData, Size, Size, Listing) ->
  {lists:reverse(Listing), IndexData};
extract_sha_list(IndexData, Size, Total, Listing) ->
  {Sha, IndexDataRem} = split_binary(IndexData, 20), % SHA
  HexSha = hex:bin_to_hexstr(Sha),
  extract_sha_list(IndexDataRem, Size, Total + 1, [HexSha|Listing]).

%%%
% extract fanout table from packfile index
%%%
extract_fanout(IndexData) ->
  extract_fanout(IndexData, 0, []).
extract_fanout(IndexData, 255, Fanout) ->
  {Size, IndexDataRem} = split_binary(IndexData, 4), % size
  <<SizeInt:32>> = Size,
  {lists:reverse(Fanout), SizeInt, IndexDataRem};
extract_fanout(IndexData, Count, Fanout) ->
  {Fan, IndexDataRem} = split_binary(IndexData, 4), % fanout entry
  <<FanInt:32>> = Fan,
  extract_fanout(IndexDataRem, Count + 1, [FanInt|Fanout]).
