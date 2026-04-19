# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults;
const
	dx = 1 shl 19;
var
	n, i: int32;
	x, y, p: int64;
	s: string;
	points: tlist<int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);
	readln(s);

	x := 1 shl 18;
	y := x;
	p := x shl 20 + y;
	points := tlist<int64>.create;
	points.add(p);

	for i := 1 to n do begin
		case s[i] of
			'R': inc(p, dx);
			'L': dec(p, dx);
			'U': inc(p);
			'D': dec(p);
		end;
		points.add(p);
		points.exchange(i, random(i+1));
	end;
	points.sort;

	i := 1;
	while (i <= n) and (points[i-1] < points[i]) do inc(i);

	if i <= n then
		writeln('Yes')
	else
		writeln('No');

	points.free;
end.

```
