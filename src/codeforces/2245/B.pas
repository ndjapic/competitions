program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
var
	notc, tci, n, h, i: int32;
	c, ans: int64;
	a: TList<int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ReadAndSort(n: int32): TList<int64>;
var
	i: int32;
	x: int64;
begin
	Result := TList<int64>.Create;
	Result.Capacity := n;
	for i := 0 to n-1 do begin
		Read(x);
		Result.Add(x - c);
		Result.Exchange(i, Random(i+1));
	end;
	ReadLn;
	Result.Sort;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, c);
		h := n div 2;
		a := ReadAndSort(n);

		ans := 0;
		for i := 0 to h-1 do inc(ans, max(0, a[i]));
		for i := h to n-1 do inc(ans, a[i]);

		writeln(ans);
		a.Free;

	end;
end.
