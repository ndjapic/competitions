program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
var
	n, i, k, x: int32;
	t: Tlist<int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ReadAndSort: TList<int64>;
var
	i: int32;
	s, e: int64;
begin
	Result := TList<int64>.Create;

	for i := 0 to n-1 do begin
		ReadLn(s, e);
		Result.Add(4*s+2);
		Result.Exchange(Result.Count - 1, Random(Result.Count));
		Result.Add(4*e);
		Result.Exchange(Result.Count - 1, Random(Result.Count));
	end;

	Result.Sort;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, k);
	t := ReadAndSort;

	x := 0;
	i := 0;
	while (i < t.Count) and (x < k) do begin
		inc(x, t[i] mod 4 - 1);
		inc(i);
	end;

	if x >= k then
		writeln('Yes')
	else
		writeln('No');
	t.Free;
end.
