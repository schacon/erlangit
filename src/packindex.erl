%%
%% Partial Git Implementation
%%

-module(packindex).
-export([extract_packfile_offset/2]).

%%%
% extract a sha offset from packfile index data
%%%
extract_packfile_offset(Data, ObjectSha) ->
  {Header, Data2}  = split_binary(Data, 4),  % \377tOc
  {Header2, Data3} = split_binary(Data2, 4), % 0002
  {FanoutTable, Size, Data4} = extract_fanout(Data3),
  io:fwrite("Offsets:~p~n~p~n", [FanoutTable, Size]),
  {ShaList, Data5} = extract_sha_list(Data4, Size),
  io:fwrite("ShaList:~p~n~p~n", [ShaList, length(ShaList)]),
  {CrcList, Data6} = extract_crc(Data5, Size),
  io:fwrite("CrcList:~p~n~p~n", [CrcList, length(CrcList)]),
  {OffsetList, Data7} = extract_offsets(Data6, Size),
  io:fwrite("OffsetList:~p~n~p~n", [OffsetList, length(OffsetList)]),
  invalid.

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
  extract_sha_list(IndexDataRem, Size, Total + 1, [Sha|Listing]).

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
