program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults;
var
	n, k, m, i, p: int32;
	h: int8;
	ans: int64;
	e, b: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k, m);
	e := tlist<int32>.create;
	b := tlist<int32>.create;

	for i := 1 to n do begin
		readln(h, p);
		case h of
			1: e.add(p);
			0: b.add(p);
		end;
	end;

	if (m <= e.count) and (k-m <= b.count) then begin

		e.sort;
		b.sort;

		ans := 0;
		for i := e.count - m to e.count - 1 do inc(ans, e[i]);
		for i := b.count - k+m to b.count - 1 do inc(ans, b[i]);

	end else
		ans := -1;

	writeln(ans);
	e.free;
	b.free;
end.
