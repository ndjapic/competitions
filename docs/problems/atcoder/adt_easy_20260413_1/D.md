# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections;
var
	n, i, xi, s: int32;
	x: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);

	x := TList<int32>.Create;
	for i := 0 to 5*n-1 do begin
		read(xi);
		x.Add(xi);
		x.Exchange(i, random(i+1));
	end;
	readln;
	x.Sort;

	s := 0;
	for i := n to 4*n-1 do inc(s, x[i]);

	writeln(s / (3*n):0:5);
	x.Free;
end.

```
