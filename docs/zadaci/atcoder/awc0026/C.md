# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
var
	n, i, t, el: int32;
	e: int64;
	p: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, t, e);
	e := e div t;

	p := TList<int32>.Create;
	for i := 1 to n do begin
		read(el);
		p.Add(el);
	end;
	readln;
	p.Sort;

	i := 0;
	while (i < n) and (e >= 0) do begin
		dec(e, p[i]);
		inc(i);
	end;

	if e < 0 then dec(i);
	writeln(i);

	p.Free;
end.

```
