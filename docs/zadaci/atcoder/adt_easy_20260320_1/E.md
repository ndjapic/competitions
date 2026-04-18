# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
	inf = 1000 * 1000 * 1000;
var
	n, i, r, c, mxr, mxc, mnr, mnc, d: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	mxr := 1;
	mxc := 1;
	mnr := inf;
	mnc := inf;

	for i := 1 to n do begin
		readln(r, c);
		mxr := max(mxr, r);
		mxc := max(mxc, c);
		mnr := min(mnr, r);
		mnc := min(mnc, c);
	end;

	d := max(mxr - mnr, mxc - mnc);
	writeln((d+1) div 2);
end.

```
