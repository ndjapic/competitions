program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
const
	nn = 40;
var
	n, i: int8;
	x, s: int64;
	a: TList<int64>;
	pre: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function dfs(i: int8; s: int64): int64;
begin
	if (i < 0) or (s < 0) or (s > pre[i]) then
		dfs := 0
	else if (s = 0) or (s = pre[i]) then
		dfs := 1
	else
		dfs := dfs(i-1, s) + dfs(i-1, s-a[i-1]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, s);

	a := TList<int64>.Create;
	for i := 1 to n do begin
		read(x);
		a.Add(x);
	end;
	readln;
	a.Sort;

	pre[0] := 0;
	for i := 0 to n-1 do pre[i+1] := pre[i] + a[i];

	writeln(dfs(n, s));
	a.Free;
end.
