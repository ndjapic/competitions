program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp #dictionary
uses
	generics.collections;
var
	n: int64;
	dp: tdictionary<int64, int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function dfs(n: int64): int64;
var
	h: int64;
begin
	if not dp.trygetvalue(n, result) then begin
		h := n div 2;
		result := n + dfs(h) + dfs(n-h);
		dp.addorsetvalue(n, result);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	dp := tdictionary<int64, int64>.create;
	dp.addorsetvalue(1, 0);

	writeln(dfs(n));

	dp.free;
end.
