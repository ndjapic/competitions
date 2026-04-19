# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	notc, tci, n, i, j, k, x, y, d: int32;
	p, a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, x, y);
		d := y-x;

		for i := 1 to n do read(p[i]);
		readln;

		for i := 1 to d do a[i] := p[i+x];
		for i := d+1 to y do a[i] := p[i-d];
		for i := y+1 to n do a[i] := p[i];

		j := 1;
		for i := 2 to d do
			if a[i] < a[j] then j := i;

		k := d+1;
		while (k <= n) and (a[k] < a[j]) do inc(k);

		for i := d+1 to k-1 do p[i-d] := a[i];
		for i := j to d do p[k-d + i-j] := a[i];
		for i := 1 to j-1 do p[k-j+i] := a[i];
		for i := k to n do p[i] := a[i];

		for i := 1 to n-1 do write(p[i], ' ');
		writeln(p[n]);

	end;
end.

```
