# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections;
var
	n, i, k, x: int32;
	ans: int64;
	d: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	d := TList<int32>.Create;
	for i := 1 to n do begin
		read(x);
		d.Add(x);
	end;
	readln;
	d.Sort;

	ans := 0;
	for i := 0 to n-k-1 do inc(ans, d[i]);
	writeln(ans);
	d.Free;
end.

```
