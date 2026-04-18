program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
var
	n: int32;
	x, y: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function mn(x: int64): int32;
var
	i, ai: int32;
	a: TList<int32>;
begin
	a := TList<int32>.Create;
	for i := 0 to n-1 do begin
		read(ai);
		a.Add(-ai);
		a.Exchange(i, random(i+1));
	end;
	readln;
	a.Sort;

	i := 0;
	while (i < n) and (x >= 0) do begin
		dec(x, -a[i]);
		inc(i);
	end;
	a.Free;
	mn := i;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, x, y);
	writeln(min(mn(x), mn(y)));
end.
