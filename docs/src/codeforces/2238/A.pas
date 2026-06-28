program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, math;
const
	INF = 1 shl 30;
var
	notc, tci, c, t: int32;
	n, i, x: int8;
	a, b: tlist<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function time(): int32;
var
	i: int8;
begin
	i := 0;
	while (i < n) and (a[i] >= b[i]) do inc(i);

	if i < n then
		result := INF
	else begin

		result := 0;
		for i := 0 to n-1 do
			inc(result, a[i] - b[i]);

	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, c);

		a := TList<int8>.Create;
		b := TList<int8>.Create;
		a.Capacity := n;
		b.Capacity := n;

		for i := 1 to n do begin
			Read(x);
			a.Add(x);
		end;
		ReadLn;

		for i := 1 to n do begin
			Read(x);
			b.Add(x);
		end;
		ReadLn;

		t := time();

		for i := 1 to n do begin
			a.Exchange(i-1, Random(i));
			b.Exchange(i-1, Random(i));
		end;

		a.Sort;
		b.Sort;

		t := min(t, c + time());
		if t >= INF then t := -1;

		writeln(t);

		a.Free;
	end;
end.
