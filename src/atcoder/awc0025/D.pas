program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, 
	Generics.Defaults;
const
	nn = 200 * 1000;
var
	n, s, i, o: int32;
	q: int64;
	x, f: array [0 .. nn] of int32;
	p: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function Compare(constref Left, Right: int32): Integer;
begin
	Result := x[Left] - x[Right];
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, s, q);

	p := TList<int32>.Create;
	for i := 0 to n-1 do begin
		read(x[i]);
		p.Add(i);
	end;
	readln;

	p.Sort(TComparer<int32>.Construct(Compare));

	f[p[0]] := p[1];
	f[p[n-1]] := p[n-2];
	for o := 1 to n-2 do
		if x[p[o+1]] - x[p[o]] < x[p[o]] - x[p[o-1]] then
			f[p[o]] := p[o+1]
		else if x[p[o+1]] - x[p[o]] > x[p[o]] - x[p[o-1]] then
			f[p[o]] := p[o-1]
		else if p[o-1] < p[o+1] then
			f[p[o]] := p[o-1]
		else
			f[p[o]] := p[o+1];

	i := s-1;
	while (q > 0) and (f[f[i]] <> i) do begin
		i := f[i];
		dec(q);
	end;

	if odd(q) then i := f[i];
	writeln(i+1);

	p.Free;
end.
