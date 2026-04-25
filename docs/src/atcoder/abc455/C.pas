program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
var
	n, m, i, j, k, x: int32;
	s: int64;
	d: TDictionary<int32, int64>;
	v: TList<int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, k);

	d := TDictionary<int32, int64>.Create;
	try

		for i := 0 to n-1 do begin
			read(x);
			if not d.TryGetValue(x, s) then s := 0;
			inc(s, x);
			d.AddOrSetValue(x, s);
		end;
		readln;

		v := TList<int64>.Create;
		j := 0;
		for s in d.Values do begin
			v.Add(s);
			v.Exchange(j, random(j+1));
			inc(j);
		end;
		v.Sort;

		m := v.Count;
		s := 0;
		for j := 0 to m-1-k do inc(s, v[j]);

		writeln(s);

	finally
		d.Free;
	end;
end.
