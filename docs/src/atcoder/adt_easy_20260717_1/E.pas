program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
var
	n, m, i, j, d, ans: int32;
	a, b: TList<int32>;
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

	i := 0;
	j := 0;
	ans := abs(a[i] - b[j]);

	while (i < n) and (j < m) do begin
		d := a[i] - b[j];
		ans := min(ans, abs(d));
		if d < 0 then
			inc(i)
		else
			inc(j);
	end;

	writeln(ans);

	a.Free;
	b.Free;
end.
