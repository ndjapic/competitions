program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
var
	n, m, i, j, k: int32;
	ans: int64;
	captured: tdictionary<int64, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure add(i, j: int32);
begin
	if (1 <= i) and (i <= n) and (1 <= j) and (j <= n) then
		captured.addorsetvalue((int64(i) shl 30) + j, true);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	captured := tdictionary<int64, boolean>.create;

	for k := 1 to m do begin
		readln(i, j);
		add(i, j);
		add(i+2, j+1);
		add(i+1, j+2);
		add(i-1, j+2);
		add(i-2, j+1);
		add(i-2, j-1);
		add(i-1, j-2);
		add(i+1, j-2);
		add(i+2, j-1);
	end;

	ans := sqr(int64(n)) - captured.count;
	writeln(ans);
	captured.free;
end.
