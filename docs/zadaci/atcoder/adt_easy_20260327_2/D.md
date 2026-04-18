# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults, Math;
const
	nn = 100;
var
	n, i, x: int8;
	s: string;
	wins: array [1 .. nn] of int8;
	p: TList<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ComparePositions(constref Left, Right: int8): Integer;
begin
	Result := - wins[Left] + wins[Right];
	if Result = 0 then Result := Left - Right;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	p := TList<int8>.Create;

	readln(n);

	for i := 0 to n-1 do begin
		readln(s);
		for j := 0 to n-1 do
			if ...

...

		p.Add(i);
	end;
	p.Sort(TComparer<TBlock>.Construct(CompareBlocks));

	p.Free;
end.

```
