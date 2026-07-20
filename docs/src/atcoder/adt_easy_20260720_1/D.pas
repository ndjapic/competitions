program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections;
var
	n, i: int8;
	a: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ReadAndSort(n: int8): TList<int32>;
var
	i: int8;
	x: int32;
begin
	Result := TList<int32>.Create;
	Result.Capacity := n;
	for i := 0 to n-1 do begin
		Read(x);
		Result.Add(x);
		Result.Exchange(i, Random(i+1));
	end;
	ReadLn;
	Result.Sort;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n);

	a := ReadAndSort(n);
	i := 1;
	while a[i] - a[i-1] = 1 do inc(i);

	writeln(a[i] - 1);
	a.Free;
end.
