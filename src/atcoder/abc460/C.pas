program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
var
	n, m, i, j, c: int32;
	a, b: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ReadAndSort(n: int32): TList<int32>;
var
	i, x: int32;
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

	readln(n, m);

	a := ReadAndSort(n);
	b := ReadAndSort(m);

	c := 0;
	i := 0;
	j := 0;
	while (i < n) and (j < m) do
		if b[j] <= 2 * a[i] then begin
			inc(c);
			inc(i);
			inc(j);
		end else
			inc(i);

	writeln(c);

	a.Free;
	b.Free;
end.
