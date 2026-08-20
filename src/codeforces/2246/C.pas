program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #naive #TLE
uses
	generics.collections,
	generics.defaults;
const
	NN = 200 * 1000;
	PRIME = 1000 * 1000 * 1000 + 7;
var
	notc, tci, n, i: int32;
	x0, x1, c0, c1: Int64;
	a: array [1 .. NN] of int32;
	dp0, dp1: TDictionary<int64, int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	dp0 := TDictionary<int64, int64>.Create;
	dp1 := TDictionary<int64, int64>.Create;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]);
		readln;

		dp0.AddOrSetValue(0, 1);
		for i := n downto 1 do begin

			for x0 in dp0.Keys.ToArray do begin
				x1 := a[i] - x0;
				if dp0.TryGetValue(x0, c0) then
					dp1.AddOrSetValue(x1, c0);
			end;

			for x1 in dp1.Keys.ToArray do begin
				if not dp0.TryGetValue(x1, c0) then c0 := 0;
				if not dp1.TryGetValue(x1, c1) then c1 := 0;
				dp0.AddOrSetValue(x1, (c0 + c1) mod PRIME);
			end;
			dp1.Clear;

		end;

		if not dp0.TryGetValue(0, c0) then c0 := 0;
		writeln(c0);
		dp0.Clear;

	end;
	dp0.Free;
	dp1.Free;
end.
