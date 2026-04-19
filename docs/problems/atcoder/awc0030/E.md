# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 1000 * 1000;
var
	n, i, d, a, s: int32;
	cohesion: int64;
	c: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for a := 1 to nn do c[a] := 0;

	for i := 1 to n do begin
		read(a);
		inc(c[a]);
	end;
	readln;

	cohesion := 0;
	for d := 1 to nn do begin
		a := d;
		s := 0;
		while a <= nn do begin
			inc(s, c[a]);
			inc(a, d);
		end;
		cohesion := max(cohesion, int64(s) * d);
	end;

	writeln(cohesion);
end.

```
