# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
var
	n, i, x: int32;
	ans: int64;
	h: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	h := TList<int32>.Create;
	for i := 1 to n do begin
		read(x);
		h.Add(x);
	end;
	readln;
	h.Sort;

	ans := abs(h[0]) + abs(h[n-1]);
	for i := 1 to n-1 do begin
		inc(ans, h[i] - h[i-1]);
	end;

	writeln(ans);
	h.Free;
end.

```
