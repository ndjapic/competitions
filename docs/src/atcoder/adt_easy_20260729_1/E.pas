program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #custom #sort
uses
	Generics.Collections,
	Generics.Defaults, Math;
const
	NN = 300 * 1000;
var
	n, m, i, j, o, ans: int32;
	a, b, c, d: array [1 .. NN] of int32;
	p: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function BirdCompare(constref l, r: int32): int32;
begin
	Result := CompareValue(d[l], d[r]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, m);

	for i := 1 to n do c[i] := 0;

	p := TList<int32>.Create;

	ans := 0;
	for i := 1 to n do begin
		readln(a[i], d[i], b[i]);
		if c[a[i]] = 0 then inc(ans);
		inc(c[a[i]]);
		p.Add(i);
		p.Exchange(i-1, Random(i));
	end;
	p.Sort(TComparer<int32>.Construct(BirdCompare));

	o := 0;
	for j := 1 to m do begin
		while (o < n) and (d[p[o]] <= j) do begin
			i := p[o];
			dec(c[a[i]]);
			if c[a[i]] = 0 then dec(ans);
			if c[b[i]] = 0 then inc(ans);
			inc(c[b[i]]);
			inc(o);
		end;
		writeln(ans);
	end;

	p.Free;
end.
