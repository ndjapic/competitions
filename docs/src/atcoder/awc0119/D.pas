program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #custom #sort #sliding #window
uses
	Generics.Collections, Generics.Defaults, Math;
const
	NN = 200 * 1000;
var
	n, m, i, j, k, x, ans: int32;
	d, p: TList<int32>;
	e, c: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function cmp(constref l, r: int32): int32;
begin
	result := CompareValue(e[l], e[r]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, m);

	p := TList<int32>.Create(TComparer<int32>.Construct(cmp));
	d := TList<int32>.Create;

	for i := 1 to n do begin
		readln(e[i], c[i]);
		p.Add(i);
		p.Exchange(i-1, Random(i));
	end;
	p.Sort;

	for j := 1 to m do begin
		read(x);
		d.Add(x);
		d.Exchange(j-1, Random(j));
	end;
	readln;
	d.Sort;

	ans := 0;
	j := 0;
	k := 0;

	while (j < m) and (k < n) do begin
		i := p[k];
		if (c[i] > 0) and (d[j] <= e[i]) then begin
			dec(c[i]);
			inc(ans);
			inc(j);
		end else
			inc(k);
	end;

	writeln(ans);
	p.Free;
	d.Free;
end.
