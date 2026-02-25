program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults;
const
	mm = 10;
var
	n, i: int32;
	m, k: int8;
	aik, s, total: int64;
	a: array [1 .. mm] of tlist<int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for k := 1 to m do a[k] := tlist<int64>.create;

	for i := 1 to n do begin
		read(aik);
		a[k].add(aik);
	end;
	readln;

	total := 0;
	for k := 1 to m do begin
		a[k].sort;
		s := 0;
		for i := 0 to n-1 do begin
			inc(total, a[k][i] * i - s);
			inc(s, a[k][i]);
		end;
	end;

	writeln(total);

	for k := 1 to m do a[k].free;
end.
