# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
const
	nn = 1000;
var
	n, x, y, z, i, o: int32;
	p: TList<int32>;
	a, b: array [0 .. nn] of int32;
	admitted: array [0 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CompareMath(constref Left, Right: int32): Integer;
begin
	Result := - a[Left] + a[Right];
	if Result = 0 then
		Result := Left - Right;
end;

function CompareEnglish(constref Left, Right: int32): Integer;
begin
	Result := - b[Left] + b[Right];
	if Result = 0 then
		Result := Left - Right;
end;

function CompareSum(constref Left, Right: int32): Integer;
begin
	Result := - a[Left] - b[Left] + b[Right] + b[Right];
	if Result = 0 then
		Result := Left - Right;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x, y, z);

	p := TList<int32>.Create;
	p.Capacity := n;

	for i := 0 to n-1 do begin
		read(a[i]);
		p[i] := i;
		admitted[i] := false;
	end;
	readln;

	p.Sort(TComparer<int32>.Construct(CompareMath));
	o := 0;
	for i := 1 to x do begin
		while admitted[p[o]] do inc(o);
		admitted[p[o]] := true;
		inc(o);
	end;

	for i := 0 to n-1 do read(b[i]);
	readln;

	p.Sort(TComparer<int32>.Construct(CompareEnglish));
	o := 0;
	for i := 1 to y do begin
		while admitted[p[o]] do inc(o);
		admitted[p[o]] := true;
		inc(o);
	end;

	p.Sort(TComparer<int32>.Construct(CompareSum));
	o := 0;
	for i := 1 to z do begin
		while admitted[p[o]] do inc(o);
		admitted[p[o]] := true;
		inc(o);
	end;

	for i := 0 to n-1 do
		if admitted[i] then writeln(i+1);

	p.Free;
end.

```
