program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort #dfs
uses
	Generics.Collections,
	Generics.Defaults;
var
	k: int32;
	i: int8;
	a: TList<int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(x: int64; i: int8);
var
	j: int8;
begin
	a.Add(x);
	for j := 0 to i-1 do
		dfs(x * 10 + j, j);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(k);

	a := TList<int64>.Create;
	for i := 1 to 9 do dfs(i, i);
	a.Sort;

	writeln(a[k-1]);
	a.Free;
end.
