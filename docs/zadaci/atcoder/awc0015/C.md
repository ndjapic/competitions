# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults;
const
	nn = 200 * 1000;
var
	n, i, p, q: int32;
	m, d, ans: int64;
	link: array [0 .. nn] of int32;
	sport: array [0 .. nn] of tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for p := 1 to nn do sport[p] := tlist<int32>.create;

	for i := 1 to n do begin
		readln(p, q);
		sport[p].add(q);
	end;

	ans := 0;
	for p := 1 to nn do begin
		sport[p].sort;
		m := sport[p].count;
		inc(ans, sqr(m));

		link[0] := -1;
		for i := 1 to m-1 do
			if sport[p][i] = sport[p][i-1] then
				link[i] := link[i-1]
			else
				link[i] := i-1;

		i := m-1;
		while i >= 0 do begin
			d := i - link[i];
			dec(ans, sqr(d));
			i := link[i];
		end;
	end;

	writeln(ans div 2);

	for p := 1 to nn do sport[p].free;
end.

```
