# Problem: D.pas

```pascal
program _D; (* WA *)
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
const
	nn = 200 * 1000;
	rr = 1000 * 1000 * 1000;
var
	n, i, di: int32;
	k, l, r, m, s: int64;
	d: tlist<int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, k);

	d := tlist<int64>.create;
	for i := 0 to n-1 do begin
		read(di);
		d.add(di);
		d.exchange(i, random(i+1));
	end;
	readln;
	d.sort;

	l := 1;
	r := rr;

	while r-l > 1 do begin
		m := (l+r) div 2;

		s := 0;
		for i := 0 to n-1 do
			if d[i] < m then
				dec(s, m - d[i])
			else if d[i] > m+k-1 then
				inc(s, d[i] - (m+k-1));

		if s > 0 then
			l := m
		else
			r := m;
	end;

	s := 0;
	for i := 0 to n-1 do
		if d[i] < l then
			inc(s, l - d[i])
		else if d[i] > l+k-1 then
			inc(s, d[i] - (l+k-1));

	writeln(s);
	d.free;
end.

```
