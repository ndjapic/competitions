program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #custom #sort
uses
	Generics.Collections,
	Generics.Defaults, Math;
const
	NN = 300 * 1000;
var
	n, k, m, i, j, o: int32;
	ans: int64;
	c, v: array [1 .. NN] of int32;
	chosen: array [1 .. NN] of boolean;
	p: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function GemCompare(constref l, r: int32): int32;
begin
	Result := CompareValue(v[r], v[l]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, k, m);

	for i := 1 to n do chosen[i] := false;

	p := TList<int32>.Create;

	ans := 0;
	for i := 1 to n do begin
		readln(c[i], v[i]);
		p.Add(i);
		p.Exchange(i-1, Random(i));
	end;
	p.Sort(TComparer<int32>.Construct(GemCompare));

	ans := 0;

	o := 0;
	for j := 1 to m do begin
		while chosen[c[p[o]]] do inc(o);
		i := p[o];
		inc(ans, v[i]);
		v[i] := 0;
		chosen[c[i]] := true;
	end;

	o := 0;
	for j := 1 to k-m do begin
		while v[p[o]] = 0 do inc(o);
		i := p[o];
		inc(ans, v[i]);
		v[i] := 0;
	end;

	writeln(ans);

	p.Free;
end.
