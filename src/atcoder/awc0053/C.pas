program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dictionary #sort #default
uses
	Generics.Collections, Generics.Defaults, Math;
const
	nn = 100 * 1000;
var
	n, i, x, l, r, c: int32;
	strength, ans: int64;
	d: TDictionary<int32, int64>;
	KeysList: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	d := TDictionary<int32, int64>.Create;
	KeysList := TList<int32>.Create;
	try

		readln(n);
		for i := 1 to n do begin
			readln(x, l, r, c);

			if not d.TryGetValue(x-l, strength) then strength := 0;
			d.AddOrSetValue(x-l, strength + c);

			if not d.TryGetValue(x+r+1, strength) then strength := 0;
			d.AddOrSetValue(x+r+1, strength - c);
		end;

		for x in d.Keys do
			KeysList.Add(x);
		KeysList.Sort;

		strength := 0;
		ans := 0;
		for x in KeysList do begin
			inc(strength, d[x]);
			ans := max(ans, strength);
		end;
		writeln(ans);

	finally
		d.Free;
		KeysList.Free;
	end;
end.
