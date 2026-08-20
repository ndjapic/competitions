program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
var
	notc, tci, n, i, bi: int32;
	b: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		b := TList<int32>.Create;
		b.Capacity := n;

		for i := 0 to n-1 do begin
			Read(bi);
			b.Add(bi);
			b.Exchange(i, Random(i+1));
		end;
		readln;
		b.Sort;

		i := n-3;
		while (i >= 0) and (b[i] = b[i+2] mod b[i+1]) do dec(i);

		if i < 0 then
			writeln(b[n-1], ' ', b[n-2])
		else
			writeln('-1');

		b.Free;

	end;
end.
